module Mosaico.Frontpage.Models where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), isJust)
import Data.String (drop, length, take)
import KSF.HtmlRenderer.Models as HtmlRenderer
import KSF.Paper (homepage)
import Lettera.Models (ArticleStub)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.EpaperBanner as EpaperBanner
import Mosaico.Lists.LatestList as LatestList
import Mosaico.Lists.MostReadList as MostReadList
import Mosaico.Paper (mosaicoPaper)
import React.Basic.Events (EventHandler)

data Hook
  = MostRead (Array ArticleStub) (ArticleStub -> EventHandler)
  | Latest (Array ArticleStub) (ArticleStub -> EventHandler)
  | Ad String String
  | ArticleUrltoRelative
  | EpaperBanner (String -> EventHandler)
  | RemoveTooltips

toHookRep :: Hook -> HtmlRenderer.HookRep
toHookRep (MostRead articles onArticleClick) = mostReadHook { articles, onArticleClick }
toHookRep (Latest articles onArticleClick)   = latestHook { articles, onArticleClick }
toHookRep ArticleUrltoRelative               = articleUrltoRelativeHook
toHookRep RemoveTooltips                     = removeTooltipsHook
toHookRep (EpaperBanner onMainClick)         = epaperBannerHook onMainClick
toHookRep (Ad placeholderText targetId)      = adHook { placeholderText, targetId }

mostReadHook
  :: { articles :: Array ArticleStub
     , onArticleClick :: ArticleStub -> EventHandler
     }
     -> HtmlRenderer.HookRep
mostReadHook { articles, onArticleClick } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                className <- HtmlRenderer.getStringAttrib "class" n
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Andra läser DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ MostReadList.render
                                     { mostReadArticles: articles
                                     , onArticleClick
                                     }
                 )
  }

latestHook
  :: { articles :: Array ArticleStub
     , onArticleClick :: ArticleStub -> EventHandler
     }
     -> HtmlRenderer.HookRep
latestHook { articles, onArticleClick } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                className <- HtmlRenderer.getStringAttrib "class" n
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "Senaste nytt DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ LatestList.render
                                     { latestArticles: articles
                                     , onArticleClick
                                     }
                 )
  }

adHook
  :: { placeholderText :: String
     , targetId :: String
     }
     -> HtmlRenderer.HookRep
adHook { placeholderText, targetId } = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                className <- HtmlRenderer.getStringAttrib "class" n
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == placeholderText -> true
                            _                                   -> false
                       )
  , processNode: (\_ _ _ -> pure $ Mosaico.ad
                                       { contentUnit: targetId
                                       , inBody: false
                                       , hideAds: false -- we shouldn't ever have to hide ads in the frontpage
                                       }
                 )
  }

epaperBannerHook :: (String -> EventHandler) -> HtmlRenderer.HookRep
epaperBannerHook onMainClick = HtmlRenderer.replacingHook
  { shouldProcessNode: (\n ->
                          let info = do
                                name      <- HtmlRenderer.getName n
                                className <- HtmlRenderer.getStringAttrib "class" n
                                children  <- HtmlRenderer.getChildren n
                                textChild <- head children
                                text      <- HtmlRenderer.getData textChild
                                pure { name, className, text }
                          in case info of
                            Just { name, className, text }
                              | name      == "div"
                              , className == "dre-item__title"
                              , text      == "E-tidningen DESKTOP" -> true
                            _                                      -> false
                       )
  , processNode: (\_ _ _ -> pure $ EpaperBanner.render { onMainClick }
                 )
  }

-- TODO: Do this in Lettera?
articleUrltoRelativeHook :: HtmlRenderer.HookRep
articleUrltoRelativeHook = HtmlRenderer.modifyingHook
  { shouldProcessNode: (\n ->
                          let info =
                                { name:_, href:_ }
                                  <$> HtmlRenderer.getName n
                                  <*> HtmlRenderer.getStringAttrib "href" n
                          in case info of
                            Just {name, href} -> name == "a" && take siteLen href == site
                            _                 -> false
                       )
  , processNode: (\n _ -> do
                    pure $ case HtmlRenderer.getStringAttrib "href" n of
                      Just href -> HtmlRenderer.setStringAttrib "href" (drop (siteLen - 1) href) n
                      _         -> n
                 )
  }
  where
    -- Slight abuse of the function but close enough
    site = homepage mosaicoPaper
    siteLen = length site

removeTooltipsHook :: HtmlRenderer.HookRep
removeTooltipsHook = HtmlRenderer.modifyingHook
  { shouldProcessNode: isJust <<< HtmlRenderer.getStringAttrib "title"
  , processNode: \n _ -> do
    pure $ HtmlRenderer.removeAttrib "title" n
  }
