module Mosaico.Server.Site where

import Prelude

import Data.Argonaut as JSON
import Data.Either (Either(..), either, hush)
import Data.Formatter.DateTime (format)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import KSF.Helpers (rfc1123Formatter)
import Lettera as Lettera
import Lettera.Models as Lettera.Models
import Lettera.Models (MosaicoArticleType(..), articleToArticleStub, articleToJson, notFoundArticle)
import Mosaico as Mosaico
import Mosaico.Article as Article
import Mosaico.Cache as Cache
import Mosaico.Feed (feedsFromInitial, routeFeed, serializeFeed)
import Mosaico.Meta (getMeta, pageTitle)
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Routes (MosaicoPage(..), routes)
import Mosaico.Server.Env (Env)
import Mosaico.Server.StaticPage (loadStaticPage)
import Mosaico.Server.Output (htmlContent)
import Mosaico.Server.Template (appendMosaico, appendHead, appendVars, cloneTemplate, globalDisableAds, makeTitle, mkWindowVariables, renderTemplateHtml)
import Node.HTTP as HTTP
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..), ResponseBody(..))
import Payload.Server.Response as Response
import React.Basic.DOM.Server as DOM.Server
import Routing (match)

data RenderInstruction = SlugNotFound

getClientRoute :: Env -> HTTP.Request -> Aff MosaicoPage
getClientRoute env req = do
  let url = HTTP.requestURL req
      route = match (routes env.catMap) url
  pure $ either (const $ NotFoundPage url) identity route

redir :: String -> Response ResponseBody
redir to = (\(Response r) -> Response $ r { headers = Headers.set "Location" to r.headers }) $
           Response.found EmptyBody

type HandlerParams a =
  { guards :: { logger :: Unit
              , clientip :: Maybe String
              , clientRoute :: MosaicoPage
              , redirection :: Maybe String
              }
  , query :: { headless :: Maybe Boolean }
  | a}

defaultHandler :: forall a. Env -> Maybe RenderInstruction -> HandlerParams a -> Aff (Response ResponseBody)
defaultHandler _ _ { guards: { redirection: Just url } } = do
  pure $ redir url

defaultHandler env Nothing params@{ guards: { clientRoute: NotFoundPage page }}
  | {before, after: slug} <- String.splitAt 9 page, before == "/artikel/" = do
  eitherArticle <- Lettera.getArticleWithSlug slug mosaicoPaper Nothing Nothing
  case eitherArticle of
    Right article ->
      pure $ redir $ "/artikel/" <> article.article.uuid
    Left _ -> defaultHandler env (Just SlugNotFound) params

defaultHandler env _ { guards: { clientRoute: route, clientip }, query} = do
  let htmlTemplate = cloneTemplate env.htmlTemplate
      headless = case route of
        ArticlePage _ -> fromMaybe false query.headless
        _ -> false

  staticPage <- case route of
    StaticPage pageName -> liftEffect $ loadStaticPage pageName
    _ -> pure Nothing
  article <- case route of
    ArticlePage uuid ->
      maybe (Just notFoundArticle) Just <<< hush <$>
      Lettera.getArticle uuid mosaicoPaper Nothing clientip
    DraftPage (Left _) -> pure $ Just draftTestArticle
    DraftPage (Right (Tuple aptomaId draftParams)) ->
      maybe (Just notFoundArticle) Just <<< hush <$>
      Lettera.getDraftArticle (show aptomaId) draftParams
    StaticPage _ | isNothing staticPage -> pure $ Just notFoundArticle
    _ -> pure Nothing
  let articleStub = articleToArticleStub <<< _.article <$> article

  Cache.Stamped {content: initialFeeds, validUntil} <-
    Cache.loadFeeds env.cache $ _.feedType <$> routeFeed route

  let state =
        { route
        , prevRoute: Nothing
        , clickedArticle: article >>= \a ->
          if a.articleType /= ErrorArticle then Just $ articleToArticleStub a.article else Nothing
        , modalView: Nothing
        , user: Nothing
        , staticPage
        , feeds: feedsFromInitial initialFeeds
        , ssrPreview: true
        , scrollToYPosition: Nothing
        , starting: true
        , articleAllowAds: maybe true Article.adsAllowed article
        , paywallCounter: 0
        }
      props =
        { article
        , categoryStructure: env.categoryStructure
        , catMap: env.catMap
        -- Render without and let client side add them
        , advertorials: Nothing
        , globalDisableAds
        , initialFeeds
        , headless
        }

      content = Mosaico.serverRender props state
      mosaicoString = DOM.Server.renderToString content

  let windowVars =
        [ Tuple "feeds" $ JSON.fromArray $ map serializeFeed initialFeeds
        , Tuple "categoryStructure" env.categoryStructureJson
        ]
        <> (maybe [] (pure <<< Tuple "articleType" <<< JSON.fromString <<< show <<< _.articleType) article)
        <> case article of
          Just {articleType: ErrorArticle} -> []
          Just {article: a} -> [ Tuple "article" $ JSON.encodeJson $ articleToJson a
                               , Tuple "headless" $ JSON.fromBoolean headless
                               ]
          _ -> []
      title = pageTitle route articleStub
      meta = getMeta route articleStub

  html <- liftEffect $
          appendMosaico mosaicoString htmlTemplate >>=
          appendVars (mkWindowVariables windowVars) >>=
          appendHead (makeTitle title <> DOM.Server.renderToStaticMarkup meta)

  pure $ expires validUntil $ htmlContent $
    (case route of
        NotFoundPage _ -> Response.notFound
        ArticlePage _ | maybe true (\x -> x.articleType == ErrorArticle) article -> Response.notFound
        _ -> Response.ok) $
    StringBody $ renderTemplateHtml html
  where
    draftTestArticle =
      { articleType: DraftArticle
      , article: notFoundArticle.article
          { title = "Draft title"
          , body = [ Lettera.Models.Html "Just some draft content" ]
          , authors = [{ byline: "Test test", image: Nothing, email: Nothing }]
          }
      }
    expires validUntil (Response r) =
      Response $ r { headers = Headers.set "Expires" (format rfc1123Formatter validUntil) r.headers }