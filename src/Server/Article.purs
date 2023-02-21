module Mosaico.Server.Article where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Lettera.Models (ArticleStub, ArticleType(..), FullArticle, articleStubToJson, articleToJson, notFoundArticle)
import Mosaico.Article as Article
import Mosaico.Article.Advertorial as Advertorial
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Feed (ArticleFeed(..), ArticleFeedType(..), mkArticleFeed)
import Mosaico.Meta as Meta
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Server.Env (Env)
import Mosaico.Server.Output (MainContent, MainContentType(..), htmlContent, renderToString)
import Mosaico.Server.Template (appendHead, appendMosaico, appendVars, cloneTemplate, makeTitle, mkWindowVariables, renderTemplateHtml)
import Payload.ResponseTypes (Response, ResponseBody(..))
import Payload.Server.Response as Response
import React.Basic (JSX)
import React.Basic.DOM.Server (renderToStaticMarkup) as DOM

renderArticle
  :: Env
  -> Either String FullArticle
  -> Array ArticleStub
  -> Array ArticleStub
  -> Boolean
  -> Aff (Response ResponseBody)
renderArticle env fullArticle mostReadArticles latestArticles headless = do
  let htmlTemplate = cloneTemplate env.htmlTemplate
  case fullArticle of
    Right a@{ article } -> do
      let articleJSX =
            case article.articleType of
              Advertorial -> renderWithComponents Advertorial.render { article }
              _ ->
                renderWithComponents (Article.render (\_ -> mempty))
                  { paper: mosaicoPaper
                  , article: Right a
                  , onLogin: mempty
                  , user: Nothing
                  , onPaywallEvent: pure unit
                  , onTagClick: const mempty
                  , onArticleClick: const mempty
                  , onAuthorClick: const mempty
                  , mostReadArticles
                  , latestArticles
                  , advertorial: Nothing
                  , breakingNews: mempty
                  }
          renderWithComponents :: forall a. ((Image.Props -> JSX) -> (Box.Props -> JSX) -> a -> JSX) -> a -> JSX
          renderWithComponents f = f (Image.render mempty) (Box.render mempty)
          mosaicoString = renderToString
                            { mainContent: { type: ArticleContent, content: articleJSX }
                            , mostReadArticles
                            , latestArticles
                            , categoryStructure: env.categoryStructure
                            , headless
                            , article: Just article
                            , isFullWidth: true
                            }

      html <- liftEffect do
        let windowVars =
              [ "article"           /\ (encodeJson $ articleToJson a.article)
              , "articleType"       /\ (JSON.fromString $ show a.articleType)
              , "mostReadArticles"  /\ (encodeJson $ map articleStubToJson mostReadArticles)
              , "latestArticles"    /\ (JSON.fromArray $ map articleStubToJson latestArticles)
              , "categoryStructure" /\ encodeJson env.categoryStructure
              , "breakingNews"      /\ JSON.jsonNull
              , "headless"          /\ JSON.fromBoolean headless
              ]
            metaTags = DOM.renderToStaticMarkup $ Meta.articleMeta article

        appendMosaico mosaicoString htmlTemplate >>= appendVars (mkWindowVariables windowVars) >>= appendHead metaTags

      pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Left _ ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env notFoundArticleContent maybeMostRead maybeLatest

notFoundArticleContent :: MainContent
notFoundArticleContent =
  { type: ArticleContent
  , content: Article.render (\_ -> mempty) (Image.render mempty) (Box.render mempty)
    { paper: mosaicoPaper
    , article: Right notFoundArticle
    , onLogin: mempty
    , user: Nothing
    , onPaywallEvent: pure unit
    , onTagClick: const mempty
    , onArticleClick: const mempty
    , onAuthorClick: const mempty
    , mostReadArticles: mempty
    , latestArticles: mempty
    , advertorial: Nothing
    , breakingNews: mempty
    }
  }

notFound
  :: Env
  -> MainContent
  -> Maybe (Array ArticleStub)
  -> Maybe (Array ArticleStub)
  -> Aff (Response ResponseBody)
notFound env mainContent maybeMostReadArticles maybeLatestArticles = do
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderToString
                        { mainContent
                        , mostReadArticles: fromMaybe [] maybeMostReadArticles
                        , latestArticles: fromMaybe [] maybeLatestArticles
                        , categoryStructure: env.categoryStructure
                        , headless: false
                        , article: Nothing
                        , isFullWidth: true
                        }
  html <- liftEffect $ do
    let windowVars =
          [ "categoryStructure" /\ encodeJson env.categoryStructure
          , "breakingNews" /\ (encodeJson $ JSON.jsonNull)
          ]
          <> foldMap (pure <<< Tuple "mostReadArticles" <<< JSON.fromArray <<< map articleStubToJson) maybeMostReadArticles
          <> foldMap (pure <<< Tuple "latestArticles" <<< JSON.fromArray <<< map articleStubToJson) maybeLatestArticles
          <> (case mainContent.type of
                 ArticleContent -> [ "article" /\ (encodeJson $ articleToJson notFoundArticle.article) ]
                 TagListContent tag -> mkArticleFeed (TagFeed tag) (ArticleList [])
                 StaticPageContent pageName -> [ "staticPageName" /\ JSON.fromString pageName ]
                 _ -> mempty
             )
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Oj... 404")
  pure $ htmlContent $ Response.notFound $ StringBody $ renderTemplateHtml html
