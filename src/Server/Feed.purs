module Mosaico.Server.Feed where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Array (head, fromFoldable, null)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.String.Regex (match, regex) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import KSF.Paper as Paper
import KSF.Random (randomString)
import Lettera as Lettera
import Lettera.Models (Category(..), CategoryType(..), frontpageCategoryLabel, uriComponentToTag)
import Mosaico.Error (notFoundWithAside)
import Mosaico.Cache as Cache
import Mosaico.Cache (parallelWithCommonLists)
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), mkArticleFeed)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Frontpage.Models (Hook(..)) as Frontpage
import Mosaico.Meta as Meta
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Search as Search
import Mosaico.Server.Article (notFound)
import Mosaico.Server.Env (Env, stdVars)
import Mosaico.Server.Output (MainContentType(..), htmlContent, renderToString)
import Mosaico.Server.Template (appendHead, appendMosaico, appendVars, cloneTemplate, makeTitle, mkWindowVariables, renderTemplateHtml)
import Mosaico.Webview as Webview
import Payload.ResponseTypes (Response, ResponseBody(..))
import Payload.Server.Response as Response
import React.Basic.DOM (div, text) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup) as DOM

frontpage :: Env -> { guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
frontpage env {} = do
  case head env.categoryStructure of
    Just frontpageCategory -> renderCategoryPage env Nothing frontpageCategory
    _ -> pure $ Response.internalError $ StringBody "no categorystructure defined"

tagList :: Env -> { params :: { tag :: String }, query :: { limit :: Maybe Int }, guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
tagList env { params: { tag }, query: { limit } } = do
  let tag' = uriComponentToTag tag
      htmlTemplate = cloneTemplate env.htmlTemplate
  { pageContent: articles, breakingNews, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $ Cache.getByTag env.cache tag' limit
  if null $ Cache.getContent articles
    then notFound
          env
          { type: TagListContent tag', content: notFoundWithAside }
          (Just $ Cache.getContent mostReadArticles)
          (Just $ Cache.getContent latestArticles)
    else do
    let mosaicoString = renderContent tag' <$> articles <*> mostReadArticles <*> latestArticles
    html <- liftEffect do
              let windowVars =
                    stdVars env (Just breakingNews) mostReadArticles latestArticles
                    <> mkArticleFeed (TagFeed tag') (ArticleList (Cache.getContent articles))
              appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
                appendVars (mkWindowVariables windowVars) >>=
                appendHead (makeTitle $ unwrap tag')
    now <- liftEffect nowDateTime
    pure $ Cache.addHeader now mosaicoString $
      htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent tag' articles mostReadArticles latestArticles =
      renderToString
      { mainContent:
          { type: TagListContent tag'
          , content: Frontpage.render $ Frontpage.List
              { label: Just $ unwrap tag'
              , content: Just articles
              , loadMore: Just $ pure unit
              , loading: false
              , onArticleClick: const mempty
              , onTagClick: const mempty
              }
          }
      , categoryStructure: env.categoryStructure
      , headless: false
      , mostReadArticles
      , latestArticles
      , article: Nothing
      , isFullWidth: false
      }

categoryPage
  :: Env
  -> { params :: { categoryName :: String }
     , guards :: { category :: Category, logger :: Unit }
     , query :: { limit :: Maybe Int }
     }
  -> Aff (Response ResponseBody)
categoryPage env { guards: { category }, query: { limit }} = do
  renderCategoryPage env limit category

renderCategoryPage :: Env -> Maybe Int -> Category -> Aff (Response ResponseBody)
renderCategoryPage env limit (Category category@{ label, type: categoryType, url}) = do
  { feed, mainContent, breakingNews, mostReadArticles, latestArticles } <- do
    case categoryType of
      Feed -> do
        { pageContent, breakingNews, mostReadArticles, latestArticles } <-
          parallelWithCommonLists env.cache $ Cache.getFrontpage env.cache limit label
        pure { feed: Just $ ArticleList $ Cache.getContent pageContent
             , mainContent:
                 (\articles ->
                     { type: FrontpageContent
                     , content: Frontpage.render $ Frontpage.List
                                  { label: Just $ unwrap label
                                  , content: Just articles
                                  , loadMore: Just $ pure unit
                                  , loading: false
                                  , onArticleClick: const mempty
                                  , onTagClick: const mempty
                                  }
                     }) <$> pageContent
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      Prerendered -> do
        { pageContent, breakingNews, mostReadArticles, latestArticles, articleStubs } <- sequential $
          { pageContent: _, breakingNews: _, mostReadArticles: _, latestArticles: _, articleStubs: _ }
          <$> parallel (Cache.getFrontpageHtml env.cache label)
          <*> parallel (Cache.getBreakingNewsHtml env.cache)
          <*> parallel (Cache.getMostRead env.cache)
          <*> parallel (Cache.getLatest env.cache)
          <*> parallel (Cache.getFrontpage env.cache limit label)
        let hooks = [ Frontpage.RemoveTooltips
                    , Frontpage.MostRead (Cache.getContent mostReadArticles) (const mempty)
                    , Frontpage.Latest (Cache.getContent latestArticles) (const mempty)
                    , Frontpage.ArticleUrltoRelative
                    , Frontpage.EpaperBanner
                    ]
        pure { feed: Html (Cache.getContent articleStubs) <$> Cache.getContent pageContent
             , mainContent: articleStubs *>
                 ((\html ->
                     { type: HtmlFrontpageContent
                     , content: Frontpage.render $ Frontpage.Prerendered
                                  { content: html
                                  , breakingNews: Cache.getContent breakingNews
                                  , hooks
                                  , onClick: mempty
                                  }
                     }) <$> pageContent)
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      Link -> dummyPage "link"
      Webview -> do
        initialRandom <- liftEffect $ randomString 10
        { mostReadArticles, latestArticles, breakingNews } <- parallelWithCommonLists env.cache $ pure unit
        -- video.js fails if it tries to initialize an M3U8 stream for a second time.
        -- If it had the webview component rendered on server side, React's hydrate
        -- would count as a second initialization.
        let streamURL = Webview.streamURL <$> url
            streamType = streamURL >>= Webview.parseStreamType
            content = case streamType of
              Just Webview.M3U8 -> DOM.div
                                     { className: "mosaico-webview"
                                     , children: [ DOM.text "Laddar..." ]
                                     }
              _                 -> Webview.render streamType (fromMaybe "" streamURL) initialRandom
            mainContent = mostReadArticles *> latestArticles $>
                          { type: WebviewContent
                          , content
                          }
        pure { feed: Nothing
             , mainContent
             , mostReadArticles
             , latestArticles
             , breakingNews
             }
      CategoryTag -> dummyPage "tag"

  -- Fallback to using list feed style
  let fallbackToList = renderCategoryPage env Nothing $ Category $ category { type = Feed }
  case categoryType of
    Prerendered
      | Nothing <- feed -> fallbackToList
      -- Sanity check
      | Just (Html _ html) <- feed
      , Right regex <- emptyRegex
      , Just _ <- Regex.match regex html -> fallbackToList
    _ -> do
      let htmlTemplate = cloneTemplate env.htmlTemplate
          mosaicoString = renderContent <$> mainContent <*> mostReadArticles <*> latestArticles
          windowVars = stdVars env (Just breakingNews) mostReadArticles latestArticles
                       <> foldMap (mkArticleFeed $ CategoryFeed label) feed
      html <- liftEffect $ appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              maybe pure appendHead (guard (label == frontpageCategoryLabel) $ Just startpageMeta) >>=
              appendHead (makeTitle title)
      let rendered = renderTemplateHtml html
      Cache.saveCategoryRender env.cache label $ mosaicoString $> rendered
      now <- liftEffect nowDateTime
      pure $ Cache.addHeader now mosaicoString $
        htmlContent $ Response.ok $ StringBody rendered
  where
    emptyRegex = Regex.regex "^\\s*$" Regex.noFlags
    renderContent mainContent mostReadArticles latestArticles =
      renderToString
          { mainContent
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , headless: false
          , article: Nothing
          , isFullWidth: false
          }
    title = if label == frontpageCategoryLabel then (Paper.paperName mosaicoPaper) else unwrap label
    startpageMeta = DOM.renderToStaticMarkup $ Meta.defaultMeta $ Just title
    -- Used for categories which have no native renders but are used
    -- just for links.  This code shouldn't be called ever but it's
    -- for aligning the types.
    dummyPage desc = do
      { mostReadArticles, latestArticles, breakingNews } <- parallelWithCommonLists env.cache $ pure unit
      pure { feed: Nothing
           , mainContent: mostReadArticles *> latestArticles $>
                          { type: StaticPageContent desc
                          , content: mempty
                          }
           , mostReadArticles
           , latestArticles
           , breakingNews
           }

searchPage :: Env -> { query :: { search :: Maybe String, limit :: Maybe Int }, guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
searchPage env { query: { search, limit } } = do
  let query = if (trim <$> search) == Just "" then Nothing else search
  searchComponent <- liftEffect Search.searchComponent
  { pageContent: articles, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $
    maybe (pure mempty) (pure <<< join <<< fromFoldable <=< Lettera.search 0 (fromMaybe 20 limit) mosaicoPaper) query
  let htmlTemplate = cloneTemplate env.htmlTemplate
      noResults = isJust query && null articles
      mosaicoString = renderToString
                          { mainContent:
                              { type: FrontpageContent
                              , content:
                                  searchComponent { query
                                                  , doSearch: const $ pure unit
                                                  , searching: false
                                                  } <>
                                  (guard (not $ null articles) $
                                   Frontpage.render $ Frontpage.List
                                   { label: if noResults
                                            then Just "Inga resultat"
                                            else ("Sökresultat: " <> _) <$> query
                                   , content: Just articles
                                   , loadMore: Just $ pure unit
                                   , loading: false
                                   , onArticleClick: const mempty
                                   , onTagClick: const mempty
                                   })
                              }
                          , mostReadArticles: Cache.getContent mostReadArticles
                          , latestArticles: Cache.getContent latestArticles
                          , categoryStructure: env.categoryStructure
                          , headless: false
                          , article: Nothing
                          , isFullWidth: false
                          }
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
                  <> mkArticleFeed (SearchFeed (fromMaybe "" query)) (ArticleList articles)
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "Sök")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
