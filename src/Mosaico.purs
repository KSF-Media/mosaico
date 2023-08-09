module Mosaico where

import Prelude

import Data.Array (null)
import Data.Either (Either(..), hush)
import Data.Foldable (fold, foldMap)
import Data.Int (ceil)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import Lettera.Models (ArticleStub, ArticleType(..), Categories, Category(..), CategoryLabel(..), CategoryType(..), FullArticle, frontpageCategoryLabel, notFoundArticle)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Analytics (sendPageView)
import Mosaico.Article.Advertorial as Advertorial
import Mosaico.Article.Paywall as Paywall
import Mosaico.Cache as Cache
import Mosaico.Client.Handlers (Handlers, clientHandlers, nullHandlers)
import Mosaico.Client.Init (Components, JSProps, createPropsMemo, fromJSProps, getComponents, getInitialValues, initialState, staticComponents)
import Mosaico.Client.Models (ModalView(..), Props, State)
import Mosaico.Error as Error
import Mosaico.Eval as Eval
import Mosaico.Feed (ArticleFeed(..), routeFeed)
import Mosaico.Footer (footer)
import Mosaico.Frontpage (Frontpage(..), MosaicoHooks, clientHooks, render, serverHooks) as Frontpage
import Mosaico.Header as Header
import Mosaico.Header.Menu as Menu
import Mosaico.Lists.LatestList as LatestList
import Mosaico.Lists.MostReadList as MostReadList
import Mosaico.MainContent (jumpToMainContent, mainContent)
import Mosaico.Meta as Meta
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Routes as Routes
import Mosaico.StaticPage (StaticPageResponse(..), fetchStaticPage)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Routing.PushState (LocationState, locations)
import Simple.JSON (read) as JSON
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (setTitle) as Web
import Web.HTML.Window (document, scroll) as Web

foreign import refreshAdsImpl :: EffectFn1 (Array String) Unit
foreign import setManualScrollRestoration :: Effect Unit

app :: Effect (React.ReactComponent JSProps)
app = do
  initialValues <- getInitialValues mosaicoPaper
  components <- getComponents initialValues
  setManualScrollRestoration
  propsMemo <- createPropsMemo

  React.reactComponent "Mosaico" $ fromJSProps propsMemo >>> \props -> React.do
    let initialPath = initialValues.locationState.pathname <> initialValues.locationState.search
        initialRoute = Routes.match props.catMap initialPath
    state /\ setState <- useState $ initialState initialValues props initialRoute

    useEffectOnce do
      Cache.setClientFeeds initialValues.cache props.initialFeeds $
        \f -> setState $ \s -> s { feeds = f s.feeds }

      -- Listen for route changes and set state accordingly
      locations (routeListener props.catMap setState) initialValues.nav

    let onPaywallEvent = setState \s -> s { paywallCounter = 1+s.paywallCounter }

    useEffect {route: state.route, title: _.title <$> state.clickedArticle} do
      let setTitle t = Web.setTitle t =<< Web.document =<< Web.window
      setTitle $ Meta.pageTitle state.route state.clickedArticle
      Meta.updateMeta $ Meta.getMeta state.route state.clickedArticle
      pure mempty

    useEffect state.route do
      foldMap (Aff.launchAff_ <<< Cache.requestFeed initialValues.cache) $ routeFeed state.route
      case state.route of
        Routes.StaticPage page
          | Just (StaticPageResponse r) <- state.staticPage
          , r.pageName == page
          -> when (isJust state.prevRoute) do
               foldMap (\p -> Eval.evalStaticPageScripts [Eval.ScriptTag $ "<script>" <> p <> "</script>"]) r.pageScript
          | otherwise ->
            Aff.launchAff_ do
              staticPage <- fetchStaticPage page
              liftEffect $ setState _  { staticPage = Just staticPage }
              case staticPage of
                StaticPageResponse r
                  | Just p <- r.pageScript -> liftEffect $ Eval.evalStaticPageScripts [Eval.ScriptTag $ "<script>" <> p <> "</script>"]
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

    let handlers = clientHandlers initialValues state setState

    pure $ fold
      [ components.userComponent
          { initialArticle: props.article
          , onClose: setState _ { modalView = Nothing }
          , setUser: \u -> setState _ { user = Just u }
          , opened: state.modalView == Just LoginModal
          , paper: mosaicoPaper
          , onPaywallEvent
          }
      , render Frontpage.clientHooks components handlers props state
      ]

routeListener :: Categories -> ((State -> State) -> Effect Unit) -> Maybe LocationState -> LocationState -> Effect Unit
routeListener c setState oldLoc location = do
  runEffectFn1 refreshAdsImpl []
  let newPath = stripFragment location
      newRoute = Routes.match c newPath
      oldRoute = Routes.match c <<< stripFragment <$> oldLoc
      (locationState :: Maybe Routes.RouteState) = hush $ JSON.read location.state
      oldPath = maybe "" (\l -> l.pathname <> l.search) oldLoc

  -- If the location we moved to was previously visited, let's scroll to the last y position it had.
  -- Note that we cannot scroll the page yet, but we need to do it via Mosaico's state
  -- as the content is rendered that way too (the page is empty at this point, or showing
  -- old content)
  let setYPosition { yPositionOnLeave: Just y } = setState _ { scrollToYPosition = Just y }
      setYPosition _ = setState _ { scrollToYPosition = Nothing }
  when (not (isSearch newRoute && maybe false isSearch oldRoute )) $
    foldMap setYPosition locationState
  when (isJust oldRoute) $ setState _ { ssrPreview = false }

  setState \s -> s { route = newRoute
                   , prevRoute = Just (Tuple s.route oldPath)
                   }
  case newRoute of
    Routes.ArticlePage _ -> pure unit
    _                    -> sendPageView

  where
    stripFragment l = Routes.stripFragment $ l.pathname <> l.search
    isSearch (Routes.SearchPage _ _) = true
    isSearch _ = false

serverRender :: Props -> State -> JSX
serverRender = render Frontpage.serverHooks staticComponents nullHandlers

render :: Frontpage.MosaicoHooks -> Components -> Handlers -> Props -> State -> JSX
render hooks components handlers props state =
  renderRouteContent state.route
  where
    renderRouteContent = case _ of
       Routes.CategoryPage category _ -> renderCategory category
       Routes.ArticlePage _
         | Just article <- props.article
         , state.ssrPreview -> mosaicoLayoutNoAside $ renderArticle $ Right article
         | Just articleStub <- state.clickedArticle -> mosaicoLayoutNoAside $ renderArticle $ Left articleStub
       -- Should be impossible
         | otherwise -> mosaicoLayoutNoAside $ renderArticle $ Right notFoundArticle
       Routes.Frontpage -> renderFrontpage
       Routes.SearchPage Nothing _ ->
          mosaicoDefaultLayout $ components.searchComponent { query: Nothing, handlers, searching: false }
       Routes.SearchPage query@(Just queryString) _ ->
          let content = Map.lookup queryString state.feeds.search
              -- Don't use state.loadingFeed for this, this'll show
              -- the existing results while loading more.
              searching = isNothing content
              loading = state.feeds.loading > 0
              noResults = maybe false null content
              searchProps = { query, handlers, searching }
              header = components.searchComponent searchProps
              label = if noResults
                      then Just "Inga resultat"
                      else Just $ "Sökresultat: " <> queryString
          in mosaicoDefaultLayout $
             header <>
             (Frontpage.render $ Frontpage.List
              { label
              , content
              , loading
              , handlers
              , showMoreButton: true
              })
       Routes.NotFoundPage _ -> mosaicoLayoutNoAside $ renderArticle $ Right notFoundArticle
       Routes.TagPage tag _ ->
          let maybeFeed = Map.lookup tag state.feeds.tag
              tagLabel = Just (CategoryLabel $ unwrap tag)
          in if maybe false null maybeFeed
             then mosaicoDefaultLayout Error.notFoundWithAside
             else frontpageNoHeader tagLabel (ArticleList <$> maybeFeed)
       Routes.MenuPage ->
         flip (mosaicoLayout true) false
         $ Menu.render
             { handlers
             , categoryStructure: props.categoryStructure
             , user: state.user
             }
       Routes.EpaperPage -> mosaicoLayoutNoAside
         $ components.epaperComponent
             { user: state.user
             , paper: mosaicoPaper
             , onLogin: handlers.onLogin
             }
       Routes.ProfilePage -> mosaicoLayoutNoAside
         $ Profile.render
             { user: state.user
             , onLogin: handlers.onLogin
             , onLogout: handlers.onLogout
             , onStaticPageClick: handlers.onStaticPageClick
             }
       Routes.DraftPage _
         | Just article <- props.article -> mosaicoLayoutNoAside $ renderArticle $ Right article
         | otherwise -> mosaicoLayoutNoAside $ renderArticle $ Right notFoundArticle
       Routes.KorsordPage -> mosaicoLayoutNoAside $
           fragment
             [ components.nagbarComponent { isArticle: false }
             , DOM.div
                 { className: "mosaico--static-page"
                 , children:
                     [ components.korsordComponent
                         { user: state.user
                         , paper: mosaicoPaper
                         , paywall
                         }
                     ]
                 }
             ]
       Routes.StaticPage _ -> mosaicoLayoutNoAside $ case state.staticPage of
         Nothing -> loadingSpinner
         Just (StaticPageResponse page)  ->
           fragment
             [ if page.pageName == "korsord" then components.nagbarComponent { isArticle: false } else mempty
             , DOM.div { className: "mosaico--static-page", dangerouslySetInnerHTML: { __html: page.pageContent } }
             ]
         Just StaticPageNotFound -> Error.notFoundWithAside
         Just StaticPageOtherError -> Error.somethingWentWrong
       Routes.DebugPage _ -> frontpageNoHeader Nothing $ Map.lookup (CategoryLabel "debug") state.feeds.category
       -- NOTE: This should not ever happen, as we always "redirect" to Frontpage route from DeployPreview
       Routes.DeployPreview -> renderFrontpage
    renderFrontpage = maybe mempty renderCategory $ Map.lookup frontpageCategoryLabel props.catMap

    renderCategory :: Category -> JSX
    renderCategory category@(Category c) =
      let maybeFeed = Map.lookup c.label state.feeds.category
      in case c.type of
        Webview -> mosaicoLayoutNoAside $ components.webviewComponent { category }
        Link -> mempty -- TODO
        Prerendered -> maybe (mosaicoLayoutNoAside loadingSpinner) (frontpageNoHeader Nothing <<< Just) maybeFeed
        Feed -> frontpageNoHeader (Just c.label) maybeFeed
        CategoryTag -> mempty -- Impossible case, it should go as a tag render

    frontpageNoHeader :: Maybe CategoryLabel -> Maybe ArticleFeed -> JSX
    frontpageNoHeader = frontpage Nothing <<< map unwrap

    frontpage :: Maybe JSX -> Maybe String -> Maybe ArticleFeed -> JSX
    frontpage maybeHeader maybeCategoryLabel (Just (ArticleList list)) = listFrontpage maybeHeader maybeCategoryLabel $ Just list
    frontpage maybeHeader _                  (Just (Html list html))   = prerenderedFrontpage maybeHeader list $ Just html
    frontpage maybeHeader _                  _                         = listFrontpage maybeHeader Nothing Nothing

    listFrontpage :: Maybe JSX -> Maybe String -> Maybe (Array ArticleStub) -> JSX
    listFrontpage maybeHeader label content = mosaicoDefaultLayout $
      (fromMaybe mempty maybeHeader) <>
      (Frontpage.render $ Frontpage.List
        { label
        , content
        , loading: state.feeds.loading > 0
        , handlers
        , showMoreButton: true
        })

    prerenderedFrontpage :: Maybe JSX -> Array ArticleStub -> Maybe String -> JSX
    prerenderedFrontpage maybeHeader articles content =
      mosaicoLayout false inner false
      where
        inner =
          (fromMaybe mempty maybeHeader) <>
          (Frontpage.render $ Frontpage.Prerendered
             { content
             , breakingNews
             , hooks: hooks handlers.onArticleClick mostReadArticles latestArticles
             , onClick: handlers.onPrerenderedClick articles
             })

    mosaicoDefaultLayout :: JSX -> JSX
    mosaicoDefaultLayout content = mosaicoLayout false content true

    mosaicoLayoutNoAside :: JSX -> JSX
    mosaicoLayoutNoAside content = mosaicoLayout false content false

    mosaicoLayout :: Boolean -> JSX -> Boolean -> JSX
    mosaicoLayout menuOpen content showAside =
      let header = components.headerComponent
              { categoryStructure: props.categoryStructure
              , user: state.user
              , handlers
              , showHeading: case state.route of
                  Routes.ArticlePage _ -> false
                  Routes.StaticPage _ -> false
                  _ -> true
              }

          mainContentClassName =
            if menuOpen
            then "md:[grid-column:1/span_2] lg:[grid-column:2/span_3]"
            else mempty

          advertorialBanner :: JSX
          advertorialBanner = case state.route of
            Routes.ArticlePage articleId
              | Just article <- state.clickedArticle
              , UUID.parseUUID article.uuid == Just articleId
              , Advertorial <- article.articleType -> Advertorial.advertorialTopBanner article
            _ -> mempty

          isFullWidth :: Boolean
          isFullWidth = case state.route of
            Routes.ArticlePage _ -> true
            Routes.NotFoundPage _ -> true
            Routes.StaticPage _ -> true
            Routes.KorsordPage -> true
            Routes.EpaperPage -> true
            _ -> false

      in DOM.div
           { id: Paper.toString mosaicoPaper
           , children:
             [ jumpToMainContent
             , Mosaico.ad { contentUnit: "mosaico-ad__top-parade", inBody: false, hideAds }
             , DOM.div
                 { className: "grid mosaico text-aptoma-text-color bg-aptoma-site-background" <>
                   (if menuOpen then " menu-open" else mempty) <>
                   (case state.user of
                       (Just (Just user')) | not (null user'.subs) -> " subscriber-view"
                       _ -> mempty)
                 , children:
                     [ guard (not props.headless) Header.topLine
                     , guard (not props.headless) header
                     , Mosaico.ad { contentUnit: "mosaico-ad__parade", inBody: false, hideAds }
                     , advertorialBanner
                     , mainContent mainContentClassName isFullWidth [content]
                     , guard (not props.headless) (footer mosaicoPaper handlers) --remember to hide footer if headless
                     , guard showAside $ DOM.aside
                         { className: "mosaico--aside"
                         , children:
                             [ Mosaico.ad { contentUnit: "mosaico-ad__box1", inBody: false, hideAds }
                             , MostReadList.render
                                 { mostReadArticles
                                 , onArticleClick: handlers.onArticleClick
                                 }
                             , Mosaico.ad { contentUnit: "mosaico-ad__box2", inBody: false, hideAds }
                             , LatestList.render
                                 { latestArticles
                                 , onArticleClick: handlers.onArticleClick
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

    hideAds = props.globalDisableAds || not case state.route of
      Routes.Frontpage -> true
      Routes.TagPage _ _ -> true
      Routes.SearchPage _ _ -> false
      Routes.DraftPage _ -> false
      Routes.ProfilePage -> false
      Routes.ArticlePage _ -> state.articleAllowAds
      Routes.MenuPage -> false
      Routes.NotFoundPage _ -> false
      Routes.CategoryPage _ _ -> true
      Routes.EpaperPage -> true
      Routes.KorsordPage -> false
      Routes.StaticPage _ -> false
      Routes.DebugPage _ -> false
      Routes.DeployPreview -> false

    renderArticle :: Either ArticleStub FullArticle -> JSX
    renderArticle article =
      components.articleComponent
        { paper: mosaicoPaper
        , article
        , handlers
        , user: state.user
        , latestArticles
        , mostReadArticles
        , advertorials: props.advertorials
        , breakingNews
        , paywall
        , paywallCounter: state.paywallCounter
        }

    latestArticles = state.feeds.latestArticles

    mostReadArticles = state.feeds.mostReadArticles

    breakingNews = state.feeds.breakingNews

    paywall = Paywall.paywall components.paywallComponent
      { handlers
      , user: join state.user
      , paper: mosaicoPaper
      }
