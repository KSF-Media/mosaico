module Mosaico.Client.Init where

import Prelude

import Affjax.Web (driver)
import Data.Argonaut (jsonNull)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toBoolean, toObject, toString) as JSON
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array (catMaybes, cons, mapMaybe, splitAt)
import Data.Date (canonicalDate)
import Data.Either (Either(..), either, hush, note)
import Data.Enum (toEnum)
import Data.Foldable (findMap, foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (toMaybe)
import Data.String (toLower)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now (nowDate)
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
import Mosaico.Archive as Archive
import Mosaico.Article (adsAllowed)
import Mosaico.Cache as Cache
import Mosaico.Client.Models (InitialValues, Props, State)
import Mosaico.Component.Article as Article
import Mosaico.Component.Consent as Consent
import Mosaico.Component.Epaper as Epaper
import Mosaico.Component.Header as Header
import Mosaico.Component.Korsord as Korsord
import Mosaico.Component.Search as Search
import Mosaico.Component.User as User
import Mosaico.Component.Webview as Webview
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), feedsFromInitial, parseFeed)
import Mosaico.Routes as Routes
import Mosaico.StaticPage (StaticPageResponse(..), getInitialStaticPageContent, getInitialStaticPageScript)
import React.Basic (JSX)
import Routing.PushState (makeInterface)

foreign import sentryDsn :: String

type JSProps = { mosaicoVars :: Json }

type Components =
  { userComponent :: User.Props -> JSX
  , consentComponent :: Consent.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  , epaperComponent :: Epaper.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , paywallComponent :: Vetrina.Props -> JSX
  , korsordComponent :: Korsord.Props -> JSX
  , archiveComponent :: Archive.Props -> JSX
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
        initialCurrentDate = either (const fallbackDate) identity $ do
          let rawDate = fromMaybe jsonNull $ Object.lookup "initialCurrentDate" obj
              error = UnexpectedValue rawDate
          date <- decodeJson =<< decodeJson rawDate
          let xs = catMaybes <<< map Int.fromString $
                   String.split (String.Pattern "-") date
          case xs of
            [y, m, d] -> do
              note error $ canonicalDate <$> toEnum y <*> toEnum m <*> toEnum d
            _ ->
              Left error
        initialFeeds =
          fromMaybe [] $ mapMaybe (parseFeed initialCurrentDate) <$> JSON.toArray (lookup "feeds")
        advertorials = findMap getAdvertorials initialFeeds
        globalDisableAds = fromMaybe false $ JSON.toBoolean $ (lookup "globalDisableAds")
        -- Decoding errors are being hushed here, although if this
        -- comes from `window.categoryStructure`, they should be
        -- valid categories
        categoryStructure =
          foldMap (mapMaybe (hush <<< decodeJson)) $ JSON.toArray (lookup "categoryStructure")
        catMap = categoriesMap $ correctionsCategory `cons` categoryStructure
        headless = fromMaybe false $ JSON.toBoolean (lookup "headless")
        -- The case where date parsing fails should be unreachable as the date
        -- comes from the Mosaico server and not some random user input:
        fallbackDate = unsafePerformEffect nowDate

    in unsafePerformEffect do
      let props = { article, advertorials, initialFeeds, categoryStructure, catMap, globalDisableAds, headless, initialCurrentDate }
      Ref.write (Just props) memo
      pure props
  where
    getAdvertorials (Tuple AdvertorialsFeed (ArticleList xs))
      | {before:[x], after} <- splitAt 1 xs = Just $ NonEmpty x after
    getAdvertorials _ = Nothing

staticComponents :: Components
staticComponents =
  { userComponent: const mempty
  , consentComponent: const mempty
  , searchComponent: Search.render mempty
    -- TODO
  , webviewComponent: const loadingSpinner
  , articleComponent: Article.pureComponent
  , epaperComponent: Epaper.render Nothing
  , headerComponent: Header.render 0
  , paywallComponent: const loadingSpinner
  , korsordComponent: Korsord.render Nothing
  , archiveComponent: Archive.render
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
  currentDate <- nowDate

  pure
    { cache
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    , logger
    , currentDate
    }

-- This used to be a part of InitialValues but it was split because it
-- would cause import loops with the new module set up.
getComponents :: InitialValues -> Effect Components
getComponents initialValues = do
  userComponent    <- User.component initialValues.logger
  consentComponent <- Consent.component
  searchComponent  <- Search.searchComponent
  webviewComponent <- Webview.webviewComponent
  articleComponent <- Article.component initialValues.cache
  epaperComponent  <- Epaper.component
  headerComponent  <- Header.component
  paywallComponent <- Vetrina.component
  korsordComponent <- Korsord.component
  archiveComponent <- Archive.component
  pure
    { userComponent
    , consentComponent
    , searchComponent
    , webviewComponent
    , articleComponent
    , epaperComponent
    , headerComponent
    , paywallComponent
    , korsordComponent
    , archiveComponent
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
  , consent: Nothing
  , currentDate: initialValues.currentDate
  }
