module Mosaico where

import Prelude

import Affjax.Web (driver)
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toBoolean, toString) as JSON
import Data.Argonaut.Decode (decodeJson)
import Data.Array (any, catMaybes, cons, find, index, length, mapMaybe, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (ceil)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe)
import Data.String.Regex (match, regex) as Regex
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..), fst)
import Data.UUID as UUID
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Random (randomInt)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Auth (enableCookieLogin) as Auth
import KSF.Driver (setDriver)
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.Spinner (loadingSpinner)
import KSF.User (User, Subscription, logout, magicLogin)
import KSF.User.Cusno (Cusno)
import Lettera as Lettera
import Lettera.Models (Article, ArticleStub, ArticleType(..), Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle, MosaicoArticleType(..), articleToArticleStub, categoriesMap, correctionsCategory, frontpageCategoryLabel, notFoundArticle, parseArticleStubWithoutLocalizing, parseArticleWithoutLocalizing, parseDraftArticle, readArticleType, tagToURIComponent)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Analytics (sendArticleAnalytics, sendPageView, setUserVariable)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial as Advertorial
import Mosaico.Cache as Cache
import Mosaico.Epaper as Epaper
import Mosaico.Error as Error
import Mosaico.Eval as Eval
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), JSInitialFeed, parseFeed)
import Mosaico.Feed as Feed
import Mosaico.Footer (footer)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Frontpage.Events (onFrontpageClick)
import Mosaico.Frontpage.Models (Hook(..)) as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.LatestList as LatestList
import Mosaico.LoginModal as LoginModal
import Mosaico.MainContent (jumpToMainContent, mainContent)
import Mosaico.Meta as Meta
import Mosaico.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper, _mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Routes as Routes
import Mosaico.Search as Search
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage, getInitialStaticPageContent, getInitialStaticPageScript)
import Mosaico.Triggerbee (addToTriggerbeeObj, sendTriggerbeeEvent)
import Mosaico.Webview as Webview
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, Render, UseEffect, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing (match)
import Routing.PushState (LocationState, PushStateInterface, locations, makeInterface)
import Simple.JSON (read) as JSON
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.History (back) as Web
import Web.HTML.Window (document, history, scroll) as Web

foreign import refreshAdsImpl :: EffectFn1 (Array String) Unit
foreign import sentryDsn_ :: Effect String
foreign import setManualScrollRestoration :: Effect Unit

data ModalView = LoginModal

type State =
  { article :: Maybe (Either Unit FullArticle)
  , route :: Routes.MosaicoPage
  , prevRoute :: Maybe (Tuple Routes.MosaicoPage String)
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe (Maybe User)
  , staticPage :: Maybe StaticPageResponse
  , categoryStructure :: Array Category
  , catMap :: Categories
  , feeds :: HashMap ArticleFeedType ArticleFeed
  , ssrPreview :: Boolean
  , advertorials :: Maybe (Array ArticleStub)
  , singleAdvertorial :: Maybe ArticleStub
  , logger :: Sentry.Logger
  , scrollToYPosition :: Maybe Number
  , starting :: Boolean
  , loadingFeed :: Boolean
  }

type SetState = (State -> State) -> Effect Unit

type Components =
  { loginModalComponent :: LoginModal.Props -> JSX
  , searchComponent :: Search.Props -> JSX
  , webviewComponent :: Webview.Props -> JSX
  , articleComponent :: Article.Props -> JSX
  , epaperComponent :: Epaper.Props -> JSX
  , advertorialComponent :: Advertorial.Props -> JSX
  , headerComponent :: Header.Props -> JSX
  , nagbarComponent :: Eval.Props -> JSX
  }

type Props =
  { article :: Maybe FullArticle
  , staticPageName :: Maybe String
  , categoryStructure :: Array Category
  -- For tests, they are prone to break in uninteresting ways with ads
  , globalDisableAds :: Boolean
  , initialFeeds :: Array (Tuple ArticleFeedType ArticleFeed)
  , headless :: Boolean
  }

type JSProps =
  { article :: Json
  , articleType :: Json
  , mostReadArticles :: Json
  , latestArticles :: Json
  , staticPageName :: Json
  , categoryStructure :: Json
  , globalDisableAds :: Json
  , initialFrontpageFeed :: Nullable JSInitialFeed
  , initialBreakingNews :: Nullable String
  , headless :: Json
  }

app :: Component Props
app = do
  Auth.enableCookieLogin
  setDriver driver
  initialValues <- getInitialValues
  component "Mosaico" $ mosaicoComponent initialValues

getPathFromLocationState :: LocationState -> String
getPathFromLocationState locationState = Routes.stripFragment $ locationState.path <> locationState.search

mosaicoComponent
  :: InitialValues
  -> Props
  -> Render Unit
            (UseEffect (Tuple Routes.MosaicoPage (Maybe Cusno))
              (UseEffect (Tuple Routes.MosaicoPage (Maybe (Either Unit String)))
                (UseEffect Unit (UseState State Unit))))
            JSX
mosaicoComponent initialValues props = React.do
  let initialCatMap = if null props.categoryStructure then Map.empty
                      else categoriesMap $ correctionsCategory `cons` props.categoryStructure
      initialPath = getPathFromLocationState initialValues.locationState
  state /\ setState <- useState initialValues.state
                         { article = Right <$> props.article
                         , clickedArticle = articleToArticleStub <<< _.article <$> props.article
                         , staticPage = map StaticPageResponse $
                                        { pageName:_, pageContent:_, pageScript: initialValues.staticPageScript }
                                        <$> props.staticPageName
                                        <*> initialValues.staticPageContent
                         , categoryStructure = props.categoryStructure
                         , catMap = initialCatMap
                         , feeds = HashMap.fromArray props.initialFeeds
                         , route = fromMaybe Routes.Frontpage $ hush $
                                   match (Routes.routes initialCatMap) initialPath
                         , user = Nothing
                         , ssrPreview = true
                         }

  let setFeed feed content =
        liftEffect $ setState $ \s -> s { feeds = HashMap.insert feed content s.feeds
                                        , loadingFeed = false
                                        }
      loadArticle articleId withAnalytics = Aff.launchAff_ do
        case UUID.parseUUID articleId of
          Nothing -> liftEffect $ setState _ { article = Just $ Left unit }
          Just uuid -> do
            liftEffect $ setState _ { article = Nothing }
            cache <- Aff.AVar.read initialValues.cache
            eitherArticle <- Cache.parallelWithCommonActions cache setFeed $
                             Lettera.getArticleAuth uuid mosaicoPaper
            liftEffect case eitherArticle of
              Right a@{ article } -> do
                Article.evalEmbeds article
                when withAnalytics $
                  sendArticleAnalytics article $ join state.user
                randomAdvertorial <- pickRandomElement $ fromMaybe [] state.advertorials
                setState _
                  { article = Just $ Right a
                  , ssrPreview = false
                  , singleAdvertorial = randomAdvertorial
                  }
              Left _ -> do
                setState _ { article = Just $ Left unit }

  useEffectOnce do
    setManualScrollRestoration
    withLoginLock <- (\l -> Aff.bracket (Aff.AVar.put unit l)
                            (const $ Aff.AVar.take l) <<< const) <$> AVar.empty
    alreadySentInitialAnalytics <- AVar.new false
    let initialSendAnalytics u = do
          foldMap (\user -> runEffectFn1 sendTriggerbeeEvent user.email) u
          userStatus <- case u of
                Just user -> do
                  now <- JSDate.now
                  let isSubscriber = any (isActive) user.subs
                      isActive :: Subscription -> Boolean
                      isActive subscription = case toMaybe subscription.dates.end of
                        Just end -> now <= end
                        Nothing  -> true
                  pure { isLoggedIn: true, isSubscriber }
                Nothing -> pure { isLoggedIn: false, isSubscriber: false }
          runEffectFn1 addToTriggerbeeObj userStatus
          setUserVariable u userStatus.isSubscriber
          case props.article of
            Just a -> sendArticleAnalytics a.article u
            Nothing -> pure unit
    -- The initial loading state prevents opening the login dialog
    -- also so better have a timeout.
    giveUpLogin <- Aff.killFiber (Exception.error "give up login") <$> Aff.launchAff do
      Aff.delay $ Milliseconds 4999.0
      withLoginLock do
        liftEffect $ setState _ { user = Just Nothing }
        liftEffect $ initialSendAnalytics Nothing
        _ <- Aff.AVar.take alreadySentInitialAnalytics
        Aff.AVar.put true alreadySentInitialAnalytics

    foldMap (Article.evalEmbeds <<< _.article) props.article
    void $ Cache.initClientCache props.initialFeeds >>= flip AVar.tryPut initialValues.cache
    Aff.launchAff_ do
      when (not $ Map.isEmpty initialCatMap) $ Aff.AVar.put initialCatMap initialValues.catMap
      cats <- if null props.categoryStructure
              then Lettera.getCategoryStructure mosaicoPaper
              else pure props.categoryStructure
      let catMap = categoriesMap $ correctionsCategory `cons` cats
      liftEffect do
        setState _ { categoryStructure = cats
                   , catMap = catMap
                   }
        -- Listen for route changes and set state accordingly
        void $ locations (routeListener catMap setState) initialValues.nav
      when (Map.isEmpty initialCatMap) $ Aff.AVar.put catMap initialValues.catMap
      -- magicLogin doesn't actually call the callback if it fails
      magicLogin Nothing $ hush >>> \u -> Aff.launchAff_ $ withLoginLock do
        giveUpLogin
        liftEffect do
          setState _ { user = Just u }
          state.logger.setUser u
        alreadySent <- Aff.AVar.take alreadySentInitialAnalytics
        when (not alreadySent) $ liftEffect $ initialSendAnalytics u
      advertorials <- Lettera.responseBody <$> Lettera.getAdvertorials mosaicoPaper
      randomAdvertorial <- liftEffect $ pickRandomElement $ fromMaybe [] advertorials
      liftEffect $ setState \s -> s
        { advertorials = advertorials
        , singleAdvertorial = s.singleAdvertorial <|> randomAdvertorial
        }
    pure $ Aff.launchAff_ giveUpLogin

  let setFrontpage feedName limit = Aff.launchAff_ do
        -- In SPA mode, this may be called before catMap has been
        -- populated to state.  Synchronize with an AVar.
        catMap <- Aff.AVar.read initialValues.catMap
        cache <- Aff.AVar.read initialValues.cache
        fresh <- Cache.isFresh cache catMap feedName limit
        when (not fresh) $ liftEffect $
          setState $ \s -> s { feeds = HashMap.delete feedName s.feeds }
        liftEffect $ setState _ { loadingFeed = true }
        foldMap (Cache.parallelLoadFeeds cache setFeed) $
          (map <<< map) (Tuple feedName) $ Cache.loadFeed cache catMap feedName limit
      onPaywallEvent = do
        maybe (pure unit) (\u -> loadArticle u true) $ _.article.uuid <$> (join <<< map hush $ state.article)

  useEffect (state.route /\ map (map _.article.title) state.article) do
    let setTitle t = Web.setTitle t =<< Web.document =<< Web.window
    setTitle $ Meta.pageTitle state.route state.article
    Meta.updateMeta $ Meta.getMeta state.route state.article
    pure mempty

  useEffect (state.route /\ map _.cusno (join state.user)) do
    case state.route of
      Routes.Frontpage -> setFrontpage (CategoryFeed frontpageCategoryLabel) Nothing
      Routes.TagPage tag limit -> setFrontpage (TagFeed tag) limit
      Routes.SearchPage (Just query) limit -> setFrontpage (SearchFeed query) limit
      Routes.ArticlePage articleId
        | Just article <- map _.article (join $ map hush state.article)
        , articleId == article.uuid
        ->
          -- When we're in a SSR premium article, don't send analytics events
          -- We already get it when trying to resolve authed user on startup
          if state.ssrPreview && _.premium article
          then loadArticle articleId false
          -- We already have the article content, but we need feeds
          else Aff.launchAff_ do
            cache <- Aff.AVar.read initialValues.cache
            Cache.parallelWithCommonActions cache setFeed mempty
        | otherwise -> loadArticle articleId true
      Routes.CategoryPage (Category c) limit -> setFrontpage (CategoryFeed c.label) limit
      Routes.StaticPage page
        | Just (StaticPageResponse r) <- state.staticPage
        , r.pageName == page
        -> when (isJust state.prevRoute) do
             foldMap (\p -> Eval.evalExternalScripts [Eval.ScriptTag $ "<script>" <> p <> "</script>"]) r.pageScript
        | otherwise ->
          Aff.launchAff_ do
            staticPage <- fetchStaticPage page
            liftEffect $ setState _  { staticPage = Just staticPage }
            case staticPage of
              StaticPageResponse r
                | Just p <- r.pageScript -> liftEffect $ Eval.evalExternalScripts [Eval.ScriptTag $ "<script>" <> p <> "</script>"]
              _ -> mempty
      Routes.DeployPreview -> liftEffect $ setState _  { route = Routes.Frontpage }
      _ -> pure unit

    let scrollToYPos y = Web.window >>= Web.scroll 0 y
    case state of
      -- User may have already started scrolling while content is loading
      { starting: true }              -> setState _ { starting = false }

      -- Let's always scroll to top with article pages, as the behaviour of going back in
      -- browser history is a bit buggy currently. This is because each time we land on an article page,
      -- the page is basically blank, so the browser loses the position anyway (there's nothing to recover to).
      -- If we want to fix this, we'd have to keep prev article in state too.
      { route: Routes.ArticlePage _ } -> scrollToYPos 0

      -- Keep position when getting more results.
      { route: Routes.SearchPage s1 l1
      , prevRoute: Just (Tuple (Routes.SearchPage s2 l2) _)
      , scrollToYPosition: pos }      ->
        when (s1 /= s2 || fromMaybe 20 l1 <= fromMaybe 20 l2) $
          scrollToYPos $ maybe 0 ceil pos
      { route: Routes.CategoryPage c1 l1
      , prevRoute: Just (Tuple (Routes.CategoryPage c2 l2) _)
      , scrollToYPosition: pos }      ->
        when (c1 /= c2 || fromMaybe 20 l1 <= fromMaybe 20 l2) $
          scrollToYPos $ maybe 0 ceil pos
      { route: Routes.TagPage t1 l1
      , prevRoute: Just (Tuple (Routes.TagPage t2 l2) _)
      , scrollToYPosition: pos }      ->
        when (t1 /= t2 || fromMaybe 20 l1 <= fromMaybe 20 l2) $
          scrollToYPos $ maybe 0 ceil pos

      -- Otherwise, scroll to the y position, or top of page if not specified
      { scrollToYPosition: Just y }   -> scrollToYPos (ceil y)
      { scrollToYPosition: Nothing }  -> scrollToYPos 0

    pure mempty

  pure $ render props setState state initialValues.components initialValues.nav onPaywallEvent

pickRandomElement :: forall a. Array a -> Effect (Maybe a)
pickRandomElement [] = pure Nothing
pickRandomElement elements = do
  randomIndex <- randomInt 0 (length elements - 1)
  pure $ index elements randomIndex

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState oldLoc location = do
  runEffectFn1 refreshAdsImpl ["mosaico-ad__top-parade", "mosaico-ad__parade"]

  let newRoute = getRoute location
      oldRoute = getRoute <$> oldLoc
      (locationState :: Maybe Routes.RouteState) = hush $ JSON.read location.state
      oldPath = maybe "" (\l -> l.pathname <> l.search) oldLoc

  -- If the location we moved to was previously visited, let's scroll to the last y position it had.
  -- Note that we cannot scroll the page yet, but we need to do it via Mosaico's state
  -- as the content is rendered that way too (the page is empty at this point, or showing
  -- old content)
  let setYPosition { yPositionOnLeave: Just y } = setState _ { scrollToYPosition = Just y }
      setYPosition _ = setState _ { scrollToYPosition = Nothing }
  when (not (isSearch newRoute && maybe false isSearch oldRoute)) $
    foldMap setYPosition locationState

  case newRoute of
    Right path -> do
      setState \s -> s { route = path
                       , prevRoute = Just (Tuple s.route oldPath)
                       , clickedArticle = case path of
                           Routes.ArticlePage articleId
                             | Just articleId /= (_.uuid <$> s.clickedArticle) -> Nothing
                           _ -> s.clickedArticle
                       }
      case path of
        Routes.ArticlePage _ -> pure unit
        _                    -> sendPageView
    Left _     -> pure unit
  where
    routes = Routes.routes $ Map.union c $ categoriesMap [ correctionsCategory ]
    getRoute l = match routes $ Routes.stripFragment $ l.pathname <> l.search
    isSearch (Right (Routes.SearchPage _ _)) = true
    isSearch _ = false

type InitialValues =
  { state :: State
  , components :: Components
  , nav :: PushStateInterface
  , locationState :: LocationState
  , staticPageContent :: Maybe String
  , staticPageScript :: Maybe String
  , catMap :: AVar.AVar Categories
  , cache :: AVar.AVar Cache.Cache
  }

getInitialValues :: Effect InitialValues
getInitialValues = do
  catMap <- AVar.empty
  cache <- AVar.empty
  nav <- makeInterface
  locationState <- nav.locationState
  staticPageContent <- toMaybe <$> getInitialStaticPageContent
  staticPageScript <- toMaybe <$> getInitialStaticPageScript
  sentryDsn <- sentryDsn_
  logger <- Sentry.mkLogger sentryDsn Nothing "mosaico"
  logger.setTag "paper" _mosaicoPaper

  loginModalComponent  <- LoginModal.loginModal
  searchComponent      <- Search.searchComponent
  webviewComponent     <- Webview.webviewComponent
  articleComponent     <- Article.component
  epaperComponent      <- Epaper.component
  advertorialComponent <- Advertorial.component
  headerComponent      <- Header.component
  nagbarComponent      <- Eval.embedNagbar
  pure
    { state:
        { article: Nothing
        , route: Routes.Frontpage
        , prevRoute: Nothing
        , clickedArticle: Nothing
        , modalView: Nothing
        , user: Nothing
        , staticPage: Nothing
        , categoryStructure: []
        , catMap: Map.empty
        , feeds: HashMap.empty
        , ssrPreview: true
        , advertorials: Nothing
        , singleAdvertorial: Nothing
        , logger
        , scrollToYPosition: Nothing
        , starting: true
        , loadingFeed: false
        }
    , components:
        { loginModalComponent
        , searchComponent
        , webviewComponent
        , articleComponent
        , epaperComponent
        , advertorialComponent
        , headerComponent
        , nagbarComponent
        }
    , catMap
    , cache
    , nav
    , locationState
    , staticPageContent
    , staticPageScript
    }

fromJSProps :: JSProps -> Props
fromJSProps jsProps =
  let articleType = readArticleType =<< JSON.toString jsProps.articleType
      article = { articleType: _, article: _ }
                <$> articleType
                <*> (hush $ (case articleType of
                                Just DraftArticle -> parseDraftArticle
                                _                 -> parseArticleWithoutLocalizing
                            ) jsProps.article)
      initialFeeds =
        catMaybes
        [ parseFeed =<< toMaybe jsProps.initialFrontpageFeed
        , Tuple LatestFeed <<< ArticleList <$>
          (map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ JSON.toArray jsProps.latestArticles)
        , Tuple MostReadFeed <<< ArticleList <$>
          (map (mapMaybe (hush <<< parseArticleStubWithoutLocalizing)) $ JSON.toArray jsProps.mostReadArticles)
        , Tuple BreakingNewsFeed <<< Html [] <$> toMaybe jsProps.initialBreakingNews
        ]
      globalDisableAds = fromMaybe false $ JSON.toBoolean jsProps.globalDisableAds
      staticPageName = JSON.toString jsProps.staticPageName
      -- Decoding errors are being hushed here, although if this
      -- comes from `window.categoryStructure`, they should be
      -- valid categories
      categoryStructure = foldMap (mapMaybe (hush <<< decodeJson)) $ JSON.toArray jsProps.categoryStructure
      headless = fromMaybe false $ JSON.toBoolean jsProps.headless
  in { article, initialFeeds, staticPageName, categoryStructure, globalDisableAds, headless }

jsApp :: Effect (React.ReactComponent JSProps)
jsApp = do
  Auth.enableCookieLogin
  setDriver driver
  initialValues <- getInitialValues
  React.reactComponent "Mosaico" $ mosaicoComponent initialValues <<< fromJSProps

render :: Props -> SetState -> State -> Components -> PushStateInterface -> Effect Unit -> JSX
render props setState state components router onPaywallEvent =
  case state.modalView of
    Just LoginModal ->
      components.loginModalComponent
        { onUserFetch: \user ->
           case user of
             Right u -> do
               setState _ { modalView = Nothing, user = Just $ Just u }
               state.logger.setUser $ Just u
               onPaywallEvent
               runEffectFn1 sendTriggerbeeEvent u.email
               now <- JSDate.now
               let isSubscriber = any (isActive) u.subs
                   isActive :: Subscription -> Boolean
                   isActive subscription = case toMaybe subscription.dates.end of
                     Just end -> now <= end
                     Nothing  -> true
               runEffectFn1 addToTriggerbeeObj { isLoggedIn: true, isSubscriber }
               setUserVariable (Just u) isSubscriber
             Left _err -> do
               onPaywallEvent
               -- TODO: Handle properly
               Console.error $ "Login error " <> show _err
        , onClose: setState \s -> s { modalView = Nothing }
        , paper: mosaicoPaper
        }
    _ -> mempty
  <> renderRouteContent state.route
  where
    renderRouteContent = case _ of
       Routes.CategoryPage category limit -> renderCategory category limit
       Routes.ArticlePage articleId
         | Just (Right fullArticle@{ article }) <- state.article -> mosaicoLayoutNoAside $
           if article.uuid == articleId
           -- If we have this article already in `state`, let's render it
           then
             case article.articleType of
               Advertorial -> components.advertorialComponent { article }
               _ -> renderArticle (Right fullArticle)
           else loadingSpinner
         | Just stub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left stub
         | Nothing <- state.article -> mosaicoLayoutNoAside loadingSpinner
         | otherwise -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.Frontpage -> renderFrontpage
       Routes.SearchPage Nothing _ ->
          mosaicoDefaultLayout $ components.searchComponent { query: Nothing, doSearch, searching: false }
       Routes.SearchPage query@(Just queryString) limit ->
          let frontpageArticles = HashMap.lookup (SearchFeed queryString) state.feeds
              content = case frontpageArticles of
                Just (ArticleList articles) -> Just articles
                _                           -> Nothing
              -- Don't use state.loadingFeed for this, this'll show
              -- the existing results while loading more.
              searching = isNothing frontpageArticles
              loadMore = if noResults
                       then Nothing
                       else Just $ doSearchLimit queryString $ increaseSearchLimit limit
              loading = state.loadingFeed
              noResults = case frontpageArticles of
                Just (ArticleList list)
                  | null list -> true
                _             -> false
              searchProps = { query, doSearch, searching }
              header = components.searchComponent searchProps
              label = if noResults
                      then Just "Inga resultat"
                      else Just $ "Sökresultat: " <> queryString
          in mosaicoDefaultLayout $
             header <>
             (Frontpage.render $ Frontpage.List
              { label
              , content
              , loadMore
              , loading
              , onArticleClick
              , onTagClick
              })
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle (Right notFoundArticle)
       Routes.TagPage tag limit ->
          let maybeFeed = HashMap.lookup (TagFeed tag) state.feeds
              tagLabel = Just (CategoryLabel $ unwrap tag)
          in case maybeFeed of
               Just (ArticleList tagFeed)
                 | null tagFeed -> mosaicoDefaultLayout Error.notFoundWithAside
               _                -> frontpageNoHeader tagLabel (Just $ doTagLimit (tagToURIComponent tag) $ increaseSearchLimit limit) maybeFeed
       Routes.MenuPage ->
         flip (mosaicoLayout true) false
         $ Menu.render
             { changeRoute: Routes.changeRoute router
             , categoryStructure: state.categoryStructure
             , onCategoryClick
             , user: state.user
             , onLogin
             , onLogout
             }
       Routes.EpaperPage -> mosaicoLayoutNoAside
         $ components.epaperComponent
             { user: state.user
             , paper: mosaicoPaper
             , onLogin
             }
       Routes.ProfilePage -> mosaicoLayoutNoAside
         $ Profile.render
             { user: state.user
             , onLogin
             , onLogout
             , onStaticPageClick
             }
       Routes.DraftPage ->
         maybe
           (mosaicoLayoutNoAside $ renderArticle $ Right notFoundArticle)
           (renderRouteContent <<< Routes.ArticlePage <<< _.uuid <<< _.article)
           $ join <<< map hush $ state.article
       Routes.StaticPage _ -> mosaicoLayoutNoAside $ case state.staticPage of
         Nothing -> loadingSpinner
         Just (StaticPageResponse page)  ->
           fragment
             [ if page.pageName == "korsord" then components.nagbarComponent { isArticle: false } else mempty
             , DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
             ]
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader Nothing Nothing $ HashMap.lookup (CategoryFeed (CategoryLabel "debug")) state.feeds
       -- NOTE: This should not ever happen, as we always "redirect" to Frontpage route from DeployPreview
       Routes.DeployPreview -> renderFrontpage
    renderFrontpage = maybe mempty (\cat -> renderCategory cat Nothing) $ Map.lookup frontpageCategoryLabel state.catMap

    increaseSearchLimit :: Maybe Int -> Int
    increaseSearchLimit Nothing = 40
    increaseSearchLimit (Just n) = n + 20

    renderCategory :: Category -> Maybe Int -> JSX
    renderCategory category@(Category c) limit =
      let maybeFeed = HashMap.lookup (CategoryFeed c.label) state.feeds
      in case c.type of
        Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
        Link -> mempty -- TODO
        Prerendered -> maybe (mosaicoLayoutNoAside loadingSpinner) (frontpageNoHeader Nothing Nothing <<< Just) maybeFeed
        Feed -> frontpageNoHeader (Just c.label) (Just $ doCategoryLimit (unwrap c.label) $ increaseSearchLimit limit) maybeFeed

    frontpageNoHeader :: Maybe CategoryLabel -> Maybe (Effect Unit) -> Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing <<< map unwrap

    frontpage :: Maybe JSX -> Maybe String -> Maybe (Effect Unit) -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader maybeCategoryLabel loadMore (Just (ArticleList list)) = listFrontpage maybeHeader maybeCategoryLabel loadMore $ Just list
    frontpage maybeHeader _                  _        (Just (Html list html))   = prerenderedFrontpage maybeHeader list $ Just html
    frontpage maybeHeader _                  _        _                         = listFrontpage maybeHeader Nothing Nothing Nothing

    listFrontpage :: Maybe JSX -> Maybe String -> Maybe (Effect Unit) -> Maybe (Array ArticleStub) -> JSX
    listFrontpage maybeHeader label loadMore content = mosaicoDefaultLayout $
      (fromMaybe mempty maybeHeader) <>
      (Frontpage.render $ Frontpage.List
        { label
        , content
        , loadMore
        , loading: state.loadingFeed
        , onArticleClick
        , onTagClick
        })

    prerenderedFrontpage :: Maybe JSX -> Array ArticleStub -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader articles content =
      mosaicoLayout false inner false
      where
        uuidRegex = hush $ Regex.regex "[^/]+$" mempty
        inner =
          (fromMaybe mempty maybeHeader) <>
          (Frontpage.render $ Frontpage.Prerendered
             { content
             , breakingNews
             , hooks
             , onClick: onFrontpageClick $
                \path -> do
                  let clickedArticle = do
                        regex <- uuidRegex
                        uuid <- NonEmptyArray.last =<< Regex.match regex path
                        find ((_ == uuid) <<< _.uuid) articles
                  setState _ { clickedArticle = clickedArticle } *> simpleRoute path
             })

    hooks :: Array Frontpage.Hook
    hooks = [ Frontpage.RemoveTooltips
            , Frontpage.MostRead (foldMap Feed.toList $ HashMap.lookup MostReadFeed state.feeds) onClickHandler
            , Frontpage.Latest (foldMap Feed.toList $ HashMap.lookup LatestFeed state.feeds) onClickHandler
            , Frontpage.ArticleUrltoRelative
            , Frontpage.EpaperBanner
            , Frontpage.Ad "Box Ad 1 DESKTOP"     "mosaico-ad__box1"
            , Frontpage.Ad "Box Ad 2 DESKTOP"     "mosaico-ad__box2"
            , Frontpage.Ad "Box Ad 3 DESKTOP"     "mosaico-ad__box3"
            , Frontpage.Ad "Box Ad 4 DESKTOP"     "mosaico-ad__box4"
            , Frontpage.Ad "Box Ad 5 DESKTOP"     "mosaico-ad__box5"
            , Frontpage.Ad "Ad 1"                 "mosaico-ad__bigbox1"
            , Frontpage.Ad "Ad 2"                 "mosaico-ad__bigbox2"
            , Frontpage.Ad "Ad MOBILE - mobparad" "mosaico-ad__mobparad"
            , Frontpage.Ad "Box Ad 1 MOBILE"      "mosaico-ad__mobbox1"
            , Frontpage.Ad "Box Ad 2 MOBILE"      "mosaico-ad__mobbox2"
            , Frontpage.Ad "Box Ad 3 MOBILE"      "mosaico-ad__mobbox3"
            ]

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout content = mosaicoLayout false content true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside content = mosaicoLayout false content false

    mosaicoLayout :: Boolean -> JSX -> Boolean -> JSX
    mosaicoLayout menuOpen content showAside =
      let header = components.headerComponent
              { changeRoute: Routes.changeRoute router
              , categoryStructure: state.categoryStructure
              , catMap: state.catMap
              , onCategoryClick
              , user: state.user
              , onLogin
              , onProfile
              , onStaticPageClick
              , onMenuClick:
                  case state.route of
                    Routes.MenuPage
                  -- Confused state, got to go to somewhere but
                  -- not to menu again
                      | (fst <$> state.prevRoute) == Just Routes.MenuPage
                      -> Routes.changeRoute router "/"
                  -- Using changeRoute would overwrite the stored Y position
                      | Just _ <- state.prevRoute
                      -> Web.window >>= Web.history >>= Web.back
                  -- Don't know what else to do so might as well
                      | otherwise
                      -> Routes.changeRoute router "/"
                    _ -> Routes.changeRoute router "/meny"
              , showHeading: case state.route of
                  Routes.ArticlePage _ -> false
                  Routes.StaticPage _ -> false
                  _ -> true
              }

          mainContentClassName =
            if menuOpen
            then "md:[grid-column:1/span_2] lg:[grid-column:2/span_3]"
            else mempty

          ifAdvertorial :: forall a. (Article -> a) -> Maybe a
          ifAdvertorial fn = case state.route of
            Routes.ArticlePage _
              | Just (Right { article }) <- state.article ->
                  case article.articleType of
                    Advertorial -> Just $ fn article
                    _ -> Nothing
            _ -> Nothing
          hideAds = not showAds

          advertorialBanner :: JSX
          advertorialBanner = fromMaybe mempty $ ifAdvertorial Advertorial.advertorialTopBanner

          isFullWidth :: Boolean
          isFullWidth = case state.route of
            Routes.ArticlePage _ -> true
            Routes.NotFoundPage _ -> true
            Routes.StaticPage _ -> true
            Routes.EpaperPage -> true
            _ -> false

      in DOM.div
           { id: Paper.toString mosaicoPaper
           , children:
             [ jumpToMainContent
             , Mosaico.ad { contentUnit: "mosaico-ad__top-parade", inBody: false, hideAds }
             , DOM.div
                 { className: "grid mosaico text-aptoma-text-color bg-aptoma-site-background" <> (if menuOpen then " menu-open" else mempty)
                 , children:
                     [ guard (not props.headless) Header.topLine
                     , guard (not props.headless) header
                     , Mosaico.ad { contentUnit: "mosaico-ad__parade", inBody: false, hideAds }
                     , advertorialBanner
                     , mainContent mainContentClassName isFullWidth [content]
                     , guard (not props.headless) (footer mosaicoPaper onCategoryClick onStaticPageClick) --remember to hide footer if headless
                     , guard showAside $ DOM.aside
                         { className: "mosaico--aside"
                         , children:
                             [ Mosaico.ad { contentUnit: "mosaico-ad__box1", inBody: false, hideAds }
                             , MostReadList.render
                                 { mostReadArticles
                                 , onClickHandler
                                 }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box2", inBody: false, hideAds }
                             , LatestList.render
                                 { latestArticles
                                 , onClickHandler
                                 }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box3", inBody: false, hideAds }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box4", inBody: false, hideAds }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box5", inBody: false, hideAds }
                             ]
                         }
                     ]
                 }
             ]
           }

    showAds = not props.globalDisableAds && case state.route of
      Routes.Frontpage -> true
      Routes.TagPage _ _ -> true
      Routes.SearchPage _ _ -> true
      Routes.DraftPage -> false
      Routes.ProfilePage -> false
      Routes.ArticlePage _ -> case state.article of
        Nothing -> true
        Just eitherArticle -> case eitherArticle of
          Right { article: article} ->
            not article.removeAds && not (article.articleType == Advertorial)
          Left _ -> false
      Routes.MenuPage -> false
      Routes.NotFoundPage _ -> false
      Routes.CategoryPage _ _ -> true
      Routes.EpaperPage -> true
      Routes.StaticPage _ -> false
      Routes.DebugPage _ -> false
      Routes.DeployPreview -> false

    renderArticle :: Either ArticleStub FullArticle -> JSX
    renderArticle article =
      components.articleComponent
        { paper: mosaicoPaper
        , article
        , onLogin
        , user: state.user
        , onPaywallEvent
        , onTagClick
        , onArticleClick
        , onAuthorClick
        , mostReadArticles
        , latestArticles
        , advertorial: state.singleAdvertorial
        , breakingNews
        }

    onClickHandler articleStub = capture_ do
      setState _ { clickedArticle = Just articleStub }
      simpleRoute $ "/artikel/" <> articleStub.uuid

    onCategoryClick (Category { type: Webview }) =
      mempty
    onCategoryClick cat@(Category c) =
      case state.route of
        Routes.CategoryPage category _ | category == cat -> mempty
        _ -> capture_ do
          simpleRoute $ "/" <> if c.label == frontpageCategoryLabel then "" else show c.label

    onProfile = capture_ $ simpleRoute "/konto"

    onTagClick tag = capture_ do
      simpleRoute $ "/tagg/" <> tagToURIComponent tag

    onArticleClick article = capture_ $ handleArticleClick article

    onAuthorClick author = capture_ do
      simpleRoute $ "/sök?q=" <> author.byline

    handleArticleClick article = do
      setState _ { clickedArticle = Just article }
      simpleRoute $ "/artikel/" <> article.uuid

    onStaticPageClick link =
      case state.route of
        Routes.StaticPage page | page == link -> mempty
        _ -> capture_ $ Routes.changeRoute router ("/sida/" <> link)

    onLogin = capture_ $ setState \s -> s { modalView = Just LoginModal }

    onLogout = capture_ do
      Aff.launchAff_ $ logout $ const $ pure unit
      setState _ { user = Just Nothing }
      state.logger.setUser Nothing
      onPaywallEvent

    -- Search is done via the router
    doSearch query = Routes.changeRoute router ("/sök?q=" <> query)

    doSearchLimit query limit =
      Routes.changeRoute router ("/sök?q=" <> query <> ("&c=" <> show limit))

    doTagLimit tag limit =
      Routes.changeRoute router ("/tagg/" <> tag <> ("?c=" <> show limit))

    doCategoryLimit category limit =
      Routes.changeRoute router ("/" <> category <> ("?c=" <> show limit))

    simpleRoute = Routes.changeRoute router

    latestArticles = foldMap Feed.toList $ HashMap.lookup LatestFeed state.feeds

    mostReadArticles = foldMap Feed.toList $ HashMap.lookup MostReadFeed state.feeds

    breakingNews = foldMap Feed.toHtml $ HashMap.lookup BreakingNewsFeed state.feeds
