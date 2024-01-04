module Mosaico.Component.Article where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.UUID (UUID, emptyUUID, parseUUID)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import KSF.Paper (Paper)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera as Lettera
import Lettera.Models (MosaicoArticleType(..), ArticleStub, ArticleType(..), FullArticle, notFoundArticle)
import Mosaico.Analytics (sendArticleAnalytics)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial as Advertorial
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Cache as Cache
import Mosaico.Client.Handlers (Handlers)
import Mosaico.Eval as Eval
import React.Basic (JSX)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.Window (document) as Web

data InputArticle
  -- Should only happen with forward or backward navigation
  = OnlyUUID UUID
  -- Normal article element clicks
  | ClickedArticle ArticleStub
  -- From server side renders
  | InitialFullArticle FullArticle

inputArticleUUID :: InputArticle -> UUID
inputArticleUUID (OnlyUUID u) = u
inputArticleUUID (ClickedArticle s) = fromMaybe emptyUUID $ parseUUID s.uuid
inputArticleUUID (InitialFullArticle a) = fromMaybe emptyUUID $ parseUUID a.article.uuid

notInitial :: InputArticle -> Boolean
notInitial (InitialFullArticle _) = false
notInitial _ = true

type Props =
  { paper :: Paper
  , article :: InputArticle
  , handlers :: Handlers
  , user :: Maybe (Maybe User)
  , breakingNews :: String
  , latestArticles :: Array ArticleStub
  , mostReadArticles :: Array ArticleStub
  , advertorials :: Maybe (NonEmpty Array ArticleStub)
  , paywall :: JSX
  , paywallCounter :: Int
  , consent :: Maybe Boolean
  }

type State =
  { advertorial :: Maybe ArticleStub
  }

type Components =
  { image :: Image.Props -> JSX
  , box :: Box.Props -> JSX
  }

pickRandomElement :: forall a. NonEmpty Array a -> Effect a
pickRandomElement (NonEmpty head tail) = do
  randomIndex <- randomInt (-1) $ Array.length tail
  pure $ fromMaybe head $ Array.index tail randomIndex

pureComponent :: Props -> JSX
pureComponent props = render Nothing
  { image: Image.render mempty
  , box: Box.render mempty
  } props $ case props.article of
      -- Should always be this
      InitialFullArticle a -> Right a
      ClickedArticle a -> Left a
      _ -> Right notFoundArticle

component :: Cache.Cache -> Component Props
component cache = do
  components <-
    { image: _, box: _ }
    <$> Image.component
    <*> Box.component
  document <- Web.document =<< Web.window
  let setTitle = flip Web.setTitle document

  React.component "Article" $ \props -> React.do
    article /\ setArticle <- useState' $
      case props.article of
        InitialFullArticle { articleType: ErrorArticle } -> Just notFoundArticle
        InitialFullArticle article -> Just article
        _ -> Nothing
    let articleId = inputArticleUUID props.article
    lastConsent /\ setLastConsent <- useState' $ fromMaybe false props.consent
    advertorial /\ setAdvertorial <- useState' Nothing
    -- Load article if uuid changed or there's a chance that user has
    -- signed in or gone through paywall.  This can't depend on cusno
    -- since user gets set during Vetrina flow before it's completed
    -- and triggering an update for Article component would then reset
    -- Vetrina.
    useEffect { articleId
              , paywallCounter: props.paywallCounter
              , consent: props.consent
              } do
      foldMap (\a -> when (props.consent == Just true) $ evalEmbeds a.article) $ case props.article of
        InitialFullArticle x -> Just x
        _ -> Nothing
      -- Needs to not be a special article
      if not $ articleId /= emptyUUID &&
         -- Either it's somehow a different article than what it's supposed to be ...
         ((Just articleId /= (parseUUID <<< _.article.uuid =<< article) && notInitial props.article) ||
          -- or logged in and it's a preview
          (isJust (join props.user) &&
           (_.articleType <$> article) == Just PreviewArticle) ||
          -- or we had previously no consent and it has embeds in it
          (not lastConsent && Just true == props.consent &&
           maybe false (not <<< Array.null) (_.externalScripts <<< _.article =<< article))
         ) then pure $ pure unit else do
        setArticle Nothing
        setLastConsent $ fromMaybe false props.consent
        Aff.launchAff_ do
          -- Refresh latest & co if needed
          res <-
            (Cache.parallelWithCommonLists cache $
             Lettera.getArticleAuth articleId props.paper Nothing) >>= \res ->
            Cache.setFeeds cache (Cache.setCommonFeeds res) *> pure res.pageContent
          liftEffect do
            case res of
              Right a | parseUUID a.article.uuid == Just articleId -> do
                props.handlers.setAllowAds $ Article.adsAllowed a
                setArticle $ Just a
                when (props.consent == Just true) $ evalEmbeds a.article
                when (notInitial props.article) $
                  sendArticleAnalytics a.article $ join props.user
              Right _ -> pure unit
              Left _ -> do
                -- TODO handle other errors
                props.handlers.setAllowAds false
                -- Main component sets article title from route and
                -- articlestub data
                setTitle ""
                setArticle $ Just notFoundArticle
        -- TODO cancel load if article has been changed.  Simply
        -- changing this to killFiber won't work because it'll kill
        -- loading article just because of a rerender.
        pure mempty

    useEffect { articleId } do
      -- Don't show a link to an advertorial if the article itself is an advertorial
      let isNonAdvertorial = (article <#> _.article.articleType <#> ((==) Advertorial)) == Just false
      when isNonAdvertorial $ case props.advertorials of
        Nothing -> pure unit
        Just advertorials ->
          setAdvertorial <<< Just =<< pickRandomElement advertorials
      pure mempty

    let render' = render (Just { advertorial }) components props
    pure $ case {article, initialArticle: props.article} of
      { article: Just a } -> render' $ Right a
      { initialArticle: InitialFullArticle a } -> render' $ Right a
      { initialArticle: ClickedArticle a } -> render' $ Left a
      _ -> loadingSpinner
  where
    evalEmbeds = Eval.evalArticleScripts <<< map Eval.ScriptTag <<< map unwrap <<< fold <<< _.externalScripts



render :: Maybe State -> Components -> Props -> Either ArticleStub FullArticle -> JSX
render state components props activeArticle =
  let normalRender article =
        Article.render components.image components.box
        { paper: props.paper
        , article
        , handlers: props.handlers
        , user: props.user
        , mostReadArticles: props.mostReadArticles
        , latestArticles: props.latestArticles
        , advertorial: _.advertorial =<< state
        , breakingNews: props.breakingNews
        , paywall: props.paywall
        , consent: props.consent
        }
  in case activeArticle of
    Right { article } | article.articleType == Advertorial ->
      Advertorial.render components.image components.box {article}
    article -> normalRender article
