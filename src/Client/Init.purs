module Mosaico.Client.Init where

import Prelude

import Affjax.Web (driver)
import Data.Argonaut (jsonNull)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toBoolean, toObject, toString) as JSON
import Data.Argonaut.Decode (decodeJson)
import Data.Array (cons, mapMaybe, splitAt)
import Data.Either (hush)
import Data.Foldable (findMap, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (toMaybe)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object as Object
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Driver (setDriver)
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.Vetrina as Vetrina
import Lettera.Models (MosaicoArticleType(..), articleToArticleStub, categoriesMap, correctionsCategory, notFoundArticle, parseArticleWithoutLocalizing, parseDraftArticle, readArticleType)
import Mosaico.Article (adsAllowed)
import Mosaico.Cache as Cache
import Mosaico.Client.Models (InitialValues, Props, State)
import Mosaico.Component.Article as Article
import Mosaico.Component.User as User
import Mosaico.Component.Epaper as Epaper
import Mosaico.Component.Header as Header
import Mosaico.Component.Korsord as Korsord
import Mosaico.Component.Search as Search
import Mosaico.Component.Webview as Webview
import Mosaico.Eval as Eval
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), feedsFromInitial, parseFeed)
import Mosaico.Routes as Routes
import Mosaico.StaticPage (StaticPageResponse(..), getInitialStaticPageContent, getInitialStaticPageScript)
import React.Basic (JSX)
import Routing.PushState (makeInterface)

foreign import sentryDsn :: String

type JSProps = { mosaicoVars :: Json }

type Components =
  { userComponent :: User.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  , epaperComponent :: Epaper.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , nagbarComponent :: Eval.Props -> JSX
  , paywallComponent :: Vetrina.Props -> JSX
  , korsordComponent :: Korsord.Props -> JSX
  }

-- Quick and dirty memoization: The main level Mosaico component has
-- constant props for the duration of the application.
createPropsMemo :: Effect (Ref.Ref (Maybe Props))
createPropsMemo = Ref.new Nothing

fromJSProps :: Ref.Ref (Maybe Props) -> JSProps -> Props
fromJSProps memo = case unsafePerformEffect $ Ref.read memo of
  Just props -> const props
  Nothing -> _.mosaicoVars >>> JSON.toObject >>> fromMaybe Object.empty >>> \obj ->
    let lookup k = fromMaybe jsonNull $ Object.lookup k obj
        articleType = readArticleType =<< JSON.toString (lookup "articleType")
        article = { articleType: _, article: _ }
                  <$> articleType
                  <*> (hush $ (case articleType of
                                  Just DraftArticle -> parseDraftArticle
                                  Just ErrorArticle -> const $ pure notFoundArticle.article
                                  _                 -> parseArticleWithoutLocalizing
                              ) $ lookup "article")
        initialFeeds =
          fromMaybe [] $ mapMaybe parseFeed <$> JSON.toArray (lookup "feeds")
        advertorials = findMap getAdvertorials initialFeeds
        globalDisableAds = fromMaybe false $ JSON.toBoolean $ (lookup "globalDisableAds")
        -- Decoding errors are being hushed here, although if this
        -- comes from `window.categoryStructure`, they should be
        -- valid categories
        categoryStructure =
          foldMap (mapMaybe (hush <<< decodeJson)) $ JSON.toArray (lookup "categoryStructure")
        catMap = categoriesMap $ correctionsCategory `cons` categoryStructure
        headless = fromMaybe false $ JSON.toBoolean (lookup "headless")
    in unsafePerformEffect do
      let props = { article, advertorials, initialFeeds, categoryStructure, catMap, globalDisableAds, headless }
      Ref.write (Just props) memo
      pure props
  where
    getAdvertorials (Tuple AdvertorialsFeed (ArticleList xs))
      | {before:[x], after} <- splitAt 1 xs = Just $ NonEmpty x after
    getAdvertorials _ = Nothing

staticComponents :: Components
staticComponents =
  { userComponent: const mempty
  , searchComponent: Search.render mempty
    -- TODO
  , webviewComponent: const loadingSpinner
  , articleComponent: Article.pureComponent
  , epaperComponent: Epaper.render Nothing
  , headerComponent: Header.render 0
  , nagbarComponent: const mempty
  , paywallComponent: const loadingSpinner
  , korsordComponent: Korsord.render Nothing
  }

getInitialValues :: Paper.Paper -> Effect InitialValues
getInitialValues paper = do
  Auth.enableCookieLogin
  setDriver driver
  cache <- Cache.initClientCache
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent
  staticPageScript <- toMaybe <$> getInitialStaticPageScript
  logger <- Sentry.mkLoggerWithSR sentryDsn 0.03 Nothing "mosaico"
  logger.setTag "paper" $ toLower $ Paper.toString paper

  pure
    { cache
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    , logger
    }

-- This used to be a part of InitialValues but it was split because it
-- would cause import loops with the new module set up.
getComponents :: InitialValues -> Effect Components
getComponents initialValues = do
  userComponent        <- User.component initialValues.logger
  searchComponent      <- Search.searchComponent
  webviewComponent     <- Webview.webviewComponent
  articleComponent     <- Article.component initialValues.cache
  epaperComponent      <- Epaper.component
  headerComponent      <- Header.component
  nagbarComponent      <- Eval.embedNagbar
  paywallComponent     <- Vetrina.component
  korsordComponent     <- Korsord.component
  pure
    { userComponent
    , searchComponent
    , webviewComponent
    , articleComponent
    , epaperComponent
    , headerComponent
    , nagbarComponent
    , paywallComponent
    , korsordComponent
    }

initialState :: InitialValues -> Props -> Routes.MosaicoPage -> State
initialState initialValues props initialRoute =
  { route: initialRoute
  , prevRoute: Nothing
  , clickedArticle: articleToArticleStub <<< _.article <$> props.article
  , modalView: Nothing
  , user: Nothing
  , staticPage: map StaticPageResponse $
    { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
    <$> case initialRoute of
          Routes.StaticPage pageName -> Just pageName
          _ -> Nothing
    <*> initialValues.staticPageContent
  , feeds: feedsFromInitial props.initialFeeds
  , ssrPreview: true
  , scrollToYPosition: Nothing
  , starting: true
  , articleAllowAds: maybe true adsAllowed props.article
  , paywallCounter: 0
  }
