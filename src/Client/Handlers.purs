module Mosaico.Client.Handlers where

import Prelude

import Data.Array (find)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (hush)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Regex (match, regex) as Regex
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.User (User, logout)
import Lettera.Models (ArticleStub, Author, Category(..), CategoryType(..), Tag, frontpageCategoryLabel, tagToURIComponent)
import Mosaico.Cache as Cache
import Mosaico.Client.Models (InitialValues, ModalView(..), State)
import Mosaico.Feed (frontpageFeedType)
import Mosaico.Frontpage.Events (onFrontpageClick)
import Mosaico.Routes as Routes
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import Web.HTML (window) as Web
import Web.HTML.History (back) as Web
import Web.HTML.Window (history) as Web

type Handlers =
  { onArticleClick :: ArticleStub -> EventHandler
  , onTagClick :: Tag -> EventHandler
  -- TODO change this to take MosaicoPage
  , onMainClick :: String -> EventHandler
  , onLogin :: EventHandler
  , setUser :: Maybe User -> Effect Unit
  , onMenuClick :: Effect Unit
  , onLogout :: EventHandler
  , onStaticPageClick :: String -> EventHandler
  , onAuthorClick :: Author -> EventHandler
  -- May choose to not capture the event
  , onPrerenderedClick :: Array ArticleStub -> EventHandler
  , onPaywallEvent :: Effect Unit
  , onLogoClick :: EventHandler
  , onCategoryClick :: Category -> EventHandler
  , doSearch :: String -> Effect Unit
  , getMore :: Effect Unit
  , setAllowAds :: Boolean -> Effect Unit
  }

-- Lots of inversion of control involved: Many of the handlers alter
-- the route path, which will trigger actions via routeHandler.
clientHandlers :: InitialValues -> State -> ((State -> State) -> Effect Unit) -> Handlers
clientHandlers initialValues state setState =
  { onArticleClick: capture_ <<< handleArticleClick
  , onTagClick
  , onMainClick: capture_ <<< Routes.changeRoute initialValues.nav
  , onLogin: capture_ $ setState _ { modalView = Just LoginModal }
  , setUser: \user -> setState \s -> s { user = Just user }
  , onLogout: capture_ do
    Aff.launchAff_ $ logout $ const $ pure unit
    setState _ { user = Just Nothing }
    initialValues.logger.setUser Nothing
  , onMenuClick: case state.route of
    Routes.MenuPage
      -- Confused state, got to go to somewhere but
      -- not to menu again
      | (fst <$> state.prevRoute) == Just Routes.MenuPage
      -> Routes.changeRoute initialValues.nav "/"
      -- Using changeRoute would overwrite the stored Y position
      | Just _ <- state.prevRoute
      -> Web.window >>= Web.history >>= Web.back
      -- Don't know what else to do so might as well
      | otherwise
      -> Routes.changeRoute initialValues.nav "/"
    _ -> Routes.changeRoute initialValues.nav "/meny"
  , onStaticPageClick: \link -> case state.route of
    Routes.StaticPage page | page == link -> mempty
    _ -> capture_ $ Routes.changeRoute initialValues.nav ("/sida/" <> link)
  , onAuthorClick: \author -> capture_ $ simpleRoute $ "/sök?q=" <> author.byline
  , onPrerenderedClick: \articles -> onFrontpageClick $ \path -> do
        let uuidRegex = hush $ Regex.regex "[^/]+$" mempty
            clickedArticle = do
              regex <- uuidRegex
              uuid <- NonEmptyArray.last =<< Regex.match regex path
              find ((_ == uuid) <<< _.uuid) articles
        foldMap handleArticleClick clickedArticle
        pure $ isJust clickedArticle
  , onLogoClick: capture_ $ Aff.launchAff_ do
    Cache.invalidateFeed initialValues.cache frontpageFeedType
    liftEffect $ simpleRoute "/"
  , onCategoryClick: \category@(Category c) ->
    case c of
      { type: Webview } -> mempty
      { tag: Just tag } -> onTagClick tag
      _ -> case state.route of
        Routes.CategoryPage ct _ | ct == category -> mempty
        _ -> capture_ do
          simpleRoute $ "/" <> if c.label == frontpageCategoryLabel then "" else show c.label
  , onPaywallEvent: setState \s -> s { paywallCounter = s.paywallCounter + 1 }
  , doSearch: \query -> Routes.changeRoute initialValues.nav ("/sök?q=" <> query)
  , getMore: case state.route of
      Routes.CategoryPage ((Category c@{type: Feed})) limit ->
        simpleRoute $ "/" <> (if c.label == frontpageCategoryLabel then "" else show c.label) <>
          ("?c=" <> show (raiseLimit limit))
      Routes.SearchPage (Just q) limit -> simpleRoute $ "/sök?q=" <> q <> ("&c=" <> show (raiseLimit limit))
      Routes.TagPage tag limit -> simpleRoute $ "/tagg/" <> tagToURIComponent tag <> ("?c=" <> show (raiseLimit limit))
      _ -> mempty
  , setAllowAds: \allow -> setState _ { articleAllowAds = allow }
  }
  where
    simpleRoute = Routes.changeRoute initialValues.nav
    onTagClick tag = capture_ do
       simpleRoute $ "/tagg/" <> tagToURIComponent tag
    raiseLimit Nothing = 40
    raiseLimit (Just limit) = limit + 20
    handleArticleClick article = do
      setState _ { clickedArticle = Just article }
      simpleRoute $ "/artikel/" <> article.uuid

-- Convenience over propagating Maybe all around.  It'd be nice to use
-- mempty instead but that'd require promoting Handlers as a newtype
-- or data and that'd lose the convenient record access syntax.
nullHandlers :: Handlers
nullHandlers =
  { onArticleClick: const mempty
  , onTagClick: const mempty
  , onMainClick: const mempty
  , onMenuClick: mempty
  , onLogin: mempty
  , setUser: const mempty
  , onLogout: mempty
  , onStaticPageClick: const mempty
  , onAuthorClick: const mempty
  , onPrerenderedClick: const mempty
  , onCategoryClick: const mempty
  , onLogoClick: mempty
  , onPaywallEvent: mempty
  , doSearch: const mempty
  , getMore: mempty
  , setAllowAds: const mempty
  }
