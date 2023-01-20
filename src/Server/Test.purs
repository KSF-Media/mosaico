module Mosaico.Server.Test where

import Prelude

import Data.Array (fromFoldable)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Lettera as Lettera
import Lettera.Models (CategoryLabel(..))
import Mosaico.Cache (getContent, parallelWithCommonLists)
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), mkArticleFeed)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.Server.Env (Env, stdVars)
import Mosaico.Server.Output (MainContentType(..), htmlContent, renderToString)
import Mosaico.Server.Template (appendMosaico, appendVars, cloneTemplate, mkWindowVariables, renderTemplateHtml)
import Payload.ResponseTypes (Response, ResponseBody(..))
import Payload.Server.Response as Response

debugList :: Env -> { params :: { uuid :: String } } -> Aff (Response ResponseBody)
debugList env { params: { uuid } } = do
  { pageContent: article, mostReadArticles, latestArticles } <-
    parallelWithCommonLists env.cache $
    maybe (pure Nothing) (map hush <<< Lettera.getArticleStub) (UUID.parseUUID uuid)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoContent = renderContent article <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
                  <> mkArticleFeed (CategoryFeed $ CategoryLabel "debug") (ArticleList $ fromFoldable article)
            appendMosaico (getContent mosaicoContent) htmlTemplate >>= appendVars (mkWindowVariables windowVars)
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent article mostReadArticles latestArticles =
      renderToString
        { mainContent:
            { type: FrontpageContent
            , content: Frontpage.render $ Frontpage.List
                { label: mempty
                , content: pure <$> article
                , footer: mempty
                , onArticleClick: const mempty
                , onTagClick: const mempty
                }
            }
        , mostReadArticles
        , latestArticles
        , categoryStructure: env.categoryStructure
        , headless: false
        , article: Nothing
        , isFullWidth: false
        }
