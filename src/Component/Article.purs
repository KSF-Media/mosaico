module Mosaico.Component.Article where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), either, hush, isLeft)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.UUID (emptyUUID, parseUUID)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import KSF.Paper (Paper)
import KSF.User (User)
import Lettera as Lettera
import Lettera.Models (MosaicoArticleType(..), ArticleStub, ArticleType(..), FullArticle, notFoundArticle)
import Mosaico.Analytics (sendArticleAnalytics)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial as Advertorial
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Eval as Eval
import Mosaico.Cache as Cache
import Mosaico.Client.Handlers (Handlers)
import React.Basic (JSX)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.Window (document) as Web

type Props =
  { paper :: Paper
  -- If given an ArticleStub, this component will handle loading the
  -- full article.  If for some reason there's an article which has
  -- only UUID available skip routing and let it go via standard a
  -- href handler and load from server.
  , article :: Either ArticleStub FullArticle
  , handlers :: Handlers
  , user :: Maybe (Maybe User)
  , breakingNews :: String
  , latestArticles :: Array ArticleStub
  , mostReadArticles :: Array ArticleStub
  , advertorials :: Maybe (NonEmpty Array ArticleStub)
  , paywall :: JSX
  , paywallCounter :: Int
  }

type State =
  { advertorial :: Maybe ArticleStub
  }

type Components =
  { image :: Image.Props -> JSX
  , box :: Box.Props -> JSX
  , nagbar :: Eval.Props -> JSX
  }

pickRandomElement :: forall a. NonEmpty Array a -> Effect a
pickRandomElement (NonEmpty head tail) = do
  randomIndex <- randomInt (-1) $ Array.length tail
  pure $ fromMaybe head $ Array.index tail randomIndex

pureComponent :: Props -> JSX
pureComponent = render Nothing
  { image: Image.render mempty
  , box: Box.render mempty
  , nagbar: Eval.render (Just false) <<< _.isArticle
  }

component :: Cache.Cache -> Component Props
component cache = do
  components <-
    { image: _, box: _, nagbar: _ }
    <$> Image.component
    <*> Box.component
    <*> Eval.embedNagbar
  document <- Web.document =<< Web.window
  let setTitle = flip Web.setTitle document

  React.component "Article" $ \props -> React.do
    article /\ setArticle <- useState' $
      case props.article of
        Right { articleType: ErrorArticle } -> Just notFoundArticle
        Right article -> Just article
        _ -> Nothing
    let articleId = fromMaybe emptyUUID $ parseUUID $ either _.uuid _.article.uuid props.article
    advertorial /\ setAdvertorial <- useState' Nothing
    -- Load article if uuid changed or there's a chance that user has
    -- signed in or gone through paywall.  This can't depend on cusno
    -- since user gets set during Vetrina flow before it's completed
    -- and triggering an update for Article component would then reset
    -- Vetrina.
    useEffect { articleId
              , paywallCounter: props.paywallCounter
              } do
      -- Only initial render has props.article Right
      foldMap (Article.evalEmbeds <<< _.article) $ hush props.article
      -- Needs to not be a special article
      if not $ articleId /= emptyUUID &&
         -- Either it's somehow a different article than what it's supposed to be ...
         ((Just articleId /= (parseUUID <<< _.article.uuid =<< article) && isLeft props.article) ||
          -- or logged in and it's a preview
          (isJust (join props.user) &&
           (_.articleType <$> article) == Just PreviewArticle)) then pure $ pure unit else do
        setArticle Nothing
        Aff.launchAff_ do
          -- Refresh latest & co if needed
          res <-
            (Cache.parallelWithCommonLists cache $
             Lettera.getArticleAuth articleId props.paper) >>= \res ->
            Cache.setFeeds cache (Cache.setCommonFeeds res) *> pure res.pageContent
          liftEffect do
            case res of
              Right a | parseUUID a.article.uuid == Just articleId -> do
                props.handlers.setAllowAds $ Article.adsAllowed a
                setArticle $ Just a
                Article.evalEmbeds a.article
                when (isLeft props.article) $
                  sendArticleAnalytics a.article $ join props.user
                when (a.article.articleType /= Advertorial) $ case props.advertorials of
                  Nothing -> pure unit
                  Just advertorials ->
                    setAdvertorial <<< Just =<< pickRandomElement advertorials
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

    pure $ render (Just { advertorial }) components $
      props { article = maybe props.article Right article }

render :: Maybe State -> Components -> Props -> JSX
render state components props =
  let normalRender article =
        Article.render components.nagbar components.image components.box
        { paper: props.paper
        , article
        , handlers: props.handlers
        , user: props.user
        , mostReadArticles: props.mostReadArticles
        , latestArticles: props.latestArticles
        , advertorial: _.advertorial =<< state
        , breakingNews: props.breakingNews
        , paywall: props.paywall
        }
  in case props.article of
    Right { article } | article.articleType == Advertorial ->
      Advertorial.render components.image components.box {article}
    article -> normalRender article
