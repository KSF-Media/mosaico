module Main where

import Prelude

import Control.Parallel.Class (parallel, sequential)
import Data.Argonaut.Core as JSON
import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, find, foldl, head, null)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.HashMap as HashMap
import Data.Map as Map
import Data.List (List (..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.String as String
import Data.String.Regex (match) as Regex
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Foreign.Object (Object, lookup)
import JSURI as URI
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import Lettera as Lettera
import Lettera.Models (Category(..), CategoryLabel(..), DraftParams, correctionsCategory)
import Mosaico.Cache (parallelWithCommonLists)
import Mosaico.Cache as Cache
import Mosaico.Epaper as Epaper
import Mosaico.Error (notFoundWithAside)
import Mosaico.Header.Menu as Menu
import Mosaico.Korsord as Korsord
import Mosaico.Meta as Meta
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Profile as Profile
import Mosaico.Routes as Routes
import Mosaico.Server.Article (renderArticle, notFound, notFoundArticleContent)
import Mosaico.Server.Env (Env, newEnv, stdVars)
import Mosaico.Server.Feed as Server.Feed
import Mosaico.Server.Output (MainContentType(..), htmlContent, jsContent, renderToString)
import Mosaico.Server.Template (appendHead, appendMosaico, appendVars, cloneTemplate, makeTitle, mkWindowVariables, renderTemplateHtml)
import Mosaico.Server.Test as Server.Test
import Mosaico.Version (mosaicoVersion)
import Node.HTTP as HTTP
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure(..), Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response as Response
import Payload.Server.Status as Status
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)
import React.Basic (fragment) as DOM
import React.Basic.DOM (div, script) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup) as DOM
import React.Basic.Events (handler_)

foreign import log :: forall a . a -> Effect Unit
foreign import serverPort :: Int

spec ::
  Spec
    { routes ::
         { getHealthz ::
              GET "/healthz"
                { response :: String
                , guards :: Guards ("clientip" : Nil)
                }
         , googleSiteVerification ::
              GET "/google8c22fe93f3684c84.html"
                { response :: File }
         , ssoCallbackReceiver ::
              GET "/xd_receiver.html"
                { response :: File }
         , setTriggerbeeCookies ::
              GET "/api/triggerbee-cookies"
                { response :: ResponseBody
                , guards :: Guards ("triggerbee" : Nil)
                }
         , getDraftArticleTest ::
              GET "/artikel/draft/test"
                { response :: ResponseBody }
         , getDraftArticle ::
              GET "/artikel/draft/<aptomaId>/?dp-time=<time>&publicationId=<publication>&user=<user>&hash=<hash>"
                { response :: ResponseBody
                , params :: { aptomaId :: String }
                , query :: DraftParams
                }
         , getArticle ::
              -- headless parameter to render article body only
              GET "/artikel/<..uuidOrSlug>?headless=<headless>"
                { response :: ResponseBody
                , params :: { uuidOrSlug :: List String }
                , guards :: Guards ("clientip" : Nil)
                , query :: { headless :: Maybe Boolean }
                }
         , adsTxt ::
              GET "/ads.txt"
                { response :: File }
         , adnamiDomainEnabler ::
              GET "/adnami/adnm.html"
                { response :: File }
         , assets ::
              GET "/assets/<..path>"
                { params :: { path :: List String }
                , response :: File
                }
         , tagList ::
              GET "/tagg/<tag>/?c=<limit>"
                { response :: ResponseBody
                , params :: { tag :: String }
                , query :: { limit :: Maybe Int }
                }
         , frontpage ::
              GET "/"
                { response :: ResponseBody }
         , menu ::
              GET "/meny"
                { response :: ResponseBody }
         , profilePage ::
              GET "/konto"
                { response :: ResponseBody
                }
         , korsordPage ::
              GET "/sida/korsord"
                { response :: ResponseBody
                }
         , staticPage ::
              GET "/sida/<pageName>"
                { response :: ResponseBody
                , params :: { pageName :: String }
                }
         , epaperPage ::
              GET "/epaper/<..path>?<..query>"
                { response :: ResponseBody
                , params :: { path :: List String }
                , query :: { query :: Object (Array String) }
                , guards :: Guards ("epaper" : Nil)
                }
         , debugList ::
              GET "/debug/<uuid>"
                { response :: ResponseBody
                , params :: { uuid :: String }
                }
         , categoryPage ::
              GET "/<categoryName>/?c=<limit>"
                { response :: ResponseBody
                , params :: { categoryName :: String }
                , guards :: Guards ("category" : Nil)
                , query :: { limit :: Maybe Int }
                }
         , searchPage ::
              GET "/sök?q=<search>&c=<limit>"
                { response :: ResponseBody
                , query :: { search :: Maybe String
                           , limit :: Maybe Int
                           }
                }
         , notFoundPage ::
              GET "/<..path>"
                { response :: ResponseBody
                , params :: { path :: List String}
                }
         , corsProxy ::
              GET "/corsProxy?url=<url>"
                { response :: ResponseBody
                , query :: { url :: String }
                }
         , guards :: Guards ( "logger" : Nil )
         }
    , guards ::
         { category :: Category
         , clientip :: Maybe String
         , epaper :: Unit
         , triggerbee :: Maybe TriggerbeeCookies
         , logger :: Unit
         }
    }
spec = Spec

main :: Effect Unit
main = do
  Aff.launchAff_ do
    env <- newEnv
    let handlers =
          { getHealthz
          , googleSiteVerification: staticAsset GoogleSiteVerification
          , ssoCallbackReceiver: staticAsset SSOReceiver
          , setTriggerbeeCookies
          , getDraftArticleTest: Server.Test.getDraftArticleTest env
          , getDraftArticle: getDraftArticle env
          , getArticle: getArticle env
          , assets
          , frontpage: Server.Feed.frontpage env
          , tagList: Server.Feed.tagList env
          , korsordPage: korsordPage env
          , staticPage: staticPage env
          , epaperPage: epaperPage env
          , debugList: Server.Test.debugList env
          , categoryPage: Server.Feed.categoryPage env
          , searchPage: Server.Feed.searchPage env
          , notFoundPage: notFoundPage env
          , profilePage: profilePage env
          , menu: menu env
          , adsTxt: staticAsset AdsTXT
          , adnamiDomainEnabler: staticAsset AdnamiDomainEnabler
          , corsProxy: corsProxyPage env
          }
        guards =
          { category: parseCategory env
          , clientip: getClientIP
          , epaper: epaperGuard
          , triggerbee
          , logger: loggerGuard
          }
    void $ Payload.startGuarded (Payload.defaultOpts { port = serverPort }) spec { handlers, guards }
    pure unit

loggerGuard :: HTTP.Request -> Aff (Either Failure Unit)
loggerGuard req = do
  let url = HTTP.requestURL req
      reqId = lookup "x-request-id" $ HTTP.requestHeaders req
  liftEffect $ log { url: url, requestId: toNullable reqId }
  pure $ Right unit

getHealthz :: {guards :: {clientip :: Maybe String, logger :: Unit}} -> Aff String
getHealthz {guards: {clientip}} =
  pure $ "OK " <> fromMaybe "" clientip <> "\n" <> mosaicoVersion

getClientIP :: HTTP.Request -> Aff (Maybe String)
getClientIP req =
   pure $ lookup "x-real-ip" hdr
   where
    hdr = HTTP.requestHeaders req

getDraftArticle
  :: Env
  -> { params :: { aptomaId :: String }
     , query :: DraftParams
     , guards :: { logger :: Unit }
     }
  -> Aff (Response ResponseBody)
getDraftArticle env { params: {aptomaId}, query } = do
  article <- Lettera.getDraftArticle aptomaId query
  renderArticle env article mempty mempty false

getArticle
  :: Env
  -> { params :: { uuidOrSlug :: List String }, guards :: { clientip :: Maybe String, logger :: Unit }, query :: { headless :: Maybe Boolean } }
  -> Aff (Response ResponseBody)
getArticle env { params: { uuidOrSlug: (uuid : _) }, guards: { clientip }, query: { headless } }
  | Just articleId <- UUID.parseUUID uuid = do
      { pageContent: article, mostReadArticles, latestArticles } <-
        parallelWithCommonLists env.cache $ Lettera.getArticle articleId mosaicoPaper Nothing clientip
      Cache.addHeaderAge 60 <$>
        renderArticle env article (Cache.getContent mostReadArticles) (Cache.getContent latestArticles) (fromMaybe false headless)
getArticle env { params: { uuidOrSlug: path }, guards: { clientip } }
  | (slug : _) <- path
  , not $ String.null slug
  = do
      article <- Lettera.getArticleWithSlug slug mosaicoPaper Nothing clientip
      case article of
        Right a -> do
          pure $ Response
            { status: Status.found
            , body: EmptyBody
            , headers: Headers.fromFoldable [ Tuple "Location" $ "/artikel/" <> a.article.uuid ]
            }
        Left _ -> renderNotFound env
   | otherwise = renderNotFound env

renderNotFound :: Env -> Aff (Response ResponseBody)
renderNotFound env = do
  feeds <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let maybeMostRead = if null feeds.mostReadArticles then Nothing else Just feeds.mostReadArticles
      maybeLatest   = if null feeds.latestArticles then Nothing else Just feeds.latestArticles
  notFound env notFoundArticleContent maybeMostRead maybeLatest

assets :: { params :: { path :: List String }, guards :: { logger :: Unit } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/assets" path

data StaticAsset = AdsTXT | AdnamiDomainEnabler | GoogleSiteVerification | SSOReceiver

staticAsset :: forall r. StaticAsset -> { | r} -> Aff File
staticAsset asset = Handlers.file $ case asset of
  AdsTXT -> "dist/assets/ads.txt"
  AdnamiDomainEnabler -> "dist/assets/adnami/adnm.html"
  GoogleSiteVerification -> "dist/assets/google8c22fe93f3684c84.html"
  SSOReceiver -> "dist/assets/xd_receiver.html"

type TriggerbeeCookies =
  { mtruid :: String
  , smtruid :: String
  }

triggerbee :: HTTP.Request -> Aff (Maybe TriggerbeeCookies)
triggerbee req = do
  cookies <- Guards.cookies req
  pure do
    mtruid <- Map.lookup "_mtruid" cookies
    smtruid <- Map.lookup "_smtruid" cookies
    pure { mtruid, smtruid }

setTriggerbeeCookies :: { guards :: { triggerbee :: Maybe TriggerbeeCookies, logger :: Unit } } -> Aff (Response ResponseBody)
setTriggerbeeCookies { guards: { triggerbee: Nothing } } = pure $ Response.ok EmptyBody
setTriggerbeeCookies { guards: { triggerbee: Just cookies } } = do
  let domain = fromMaybe "" $ head $ String.split (String.Pattern "/") $ String.drop 12 $ Paper.homepage mosaicoPaper
      cookieAttribs = "; secure; samesite=strict; max-age: 1314000; domain=." <> domain
      setMtruid = Headers.setCookie "_mtruid" $ cookies.mtruid <> cookieAttribs
      setSmtruid = Headers.setCookie "_smtruid" $ cookies.smtruid <> cookieAttribs
      setCookies (Response response) =
        Response $ response { headers = setMtruid $ setSmtruid $ response.headers }
  pure $ setCookies $ Response.ok EmptyBody

menu :: Env -> { guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
menu env _ = do
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString =
        renderToString
          { mainContent:
              { type: MenuContent
              , content: Menu.render
                  { categoryStructure: env.categoryStructure
                  , onCategoryClick: const $ handler_ $ pure unit
                  , user: Nothing
                  , onLogin: mempty
                  , onLogout: mempty
                  , changeRoute: const mempty
                  }
              }
            , mostReadArticles: mempty
            , latestArticles: mempty
            , categoryStructure: env.categoryStructure
            , headless: false
            , article: Nothing
            , isFullWidth: false
          }
  html <- liftEffect do
            let windowVars =
                  [ "categoryStructure" /\ encodeJson env.categoryStructure
                  ]
            appendMosaico mosaicoString htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle (Paper.paperName mosaicoPaper))
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

epaperGuard :: HTTP.Request -> Aff (Either Failure Unit)
epaperGuard req = do
  let url = HTTP.requestURL req
  pure $ if url == "/epaper/" || String.take 9 url == "/epaper/?"
         then Right unit else Left (Forward "not exact match with epaper page")

epaperPage :: Env -> { params :: { path :: List String }, query :: { query :: Object (Array String) }, guards :: { epaper :: Unit, logger :: Unit } } -> Aff (Response ResponseBody)
epaperPage env {} = do
  { mostReadArticles, latestArticles } <- parallelWithCommonLists env.cache $ pure unit
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderContent <$> mostReadArticles <*> latestArticles
  html <- liftEffect do
            let windowVars =
                  stdVars env Nothing mostReadArticles latestArticles
            appendMosaico (Cache.getContent mosaicoString) htmlTemplate >>=
              appendVars (mkWindowVariables windowVars) >>=
              appendHead (makeTitle "E-Tidningen")
  now <- liftEffect nowDateTime
  pure $ Cache.addHeader now mosaicoString $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    renderContent mostReadArticles latestArticles =
      renderToString
          { mainContent:
              { type: EpaperContent
              , content: Epaper.render mempty mosaicoPaper true Nothing Nothing
              }
          , categoryStructure: env.categoryStructure
          , mostReadArticles
          , latestArticles
          , headless: false
          , article: Nothing
          , isFullWidth: true
          }

korsordPage :: Env -> { guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
korsordPage env {} = do
  { mostReadArticles, latestArticles } <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString =
        renderToString
          { mainContent:
              { type: Korsord
              , content: DOM.div
                  { className: "mosaico--static-page"
                  , children: [ Korsord.spinner ]
                  }
              }
          , mostReadArticles
          , latestArticles
          , categoryStructure: env.categoryStructure
          , headless: false
          , article: Nothing
          , isFullWidth: true
          }
  html <- liftEffect do
    let windowVars =
          [ "staticPageName" /\ JSON.fromString "korsord"
          , "categoryStructure" /\ encodeJson env.categoryStructure
          ]
    appendMosaico mosaicoString htmlTemplate
      >>= appendVars (mkWindowVariables windowVars)
      >>= appendHead (makeTitle title <> renderedMeta)
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
  where
    title = Meta.pageTitle Routes.KorsordPage Nothing

    renderedMeta = DOM.renderToStaticMarkup $ Meta.staticPageMeta "korsord"

staticPage :: Env -> { params :: { pageName :: String }, guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
staticPage env { params: { pageName } } = do
  { mostReadArticles, latestArticles } <- sequential $
    { mostReadArticles: _, latestArticles: _ }
    <$> parallel (Cache.getContent <$> Cache.getMostRead env.cache)
    <*> parallel (Cache.getContent <$> Cache.getLatest env.cache)
  case HashMap.lookup (pageName <> ".html") env.staticPages of
    Just staticPageContent -> do
      let staticPageScript = HashMap.lookup (pageName <> ".js") env.staticPages
          htmlTemplate = cloneTemplate env.htmlTemplate
          staticPageJsx = DOM.fragment
            [ DOM.div
                { className: "mosaico--static-page"
                , children:
                    [ DOM.div { dangerouslySetInnerHTML: { __html: staticPageContent } }
                    , foldMap (\script -> DOM.script { className: "mosaico--static-page_script", dangerouslySetInnerHTML: { __html: script } }) staticPageScript
                    ]
                }
            ]
      let mosaicoString =
            renderToString
              { mainContent:
                  { type: StaticPageContent pageName
                  , content: staticPageJsx
                  }
              , mostReadArticles
              , latestArticles
              , categoryStructure: env.categoryStructure
              , headless: false
              , article: Nothing
              , isFullWidth: true
              }
      html <- liftEffect do
        let windowVars =
              [ "staticPageName" /\ JSON.fromString pageName
              , "categoryStructure" /\ encodeJson env.categoryStructure
              ]
        appendMosaico mosaicoString htmlTemplate
          >>= appendVars (mkWindowVariables windowVars)
          >>= appendHead (makeTitle title <> renderedMeta)
      pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html
    Nothing ->
      let maybeMostRead = if null mostReadArticles then Nothing else Just mostReadArticles
          maybeLatest = if null latestArticles then Nothing else Just latestArticles
      in notFound env { type: StaticPageContent pageName, content: notFoundWithAside } maybeMostRead maybeLatest
  where
    title = Meta.pageTitle (Routes.StaticPage pageName) Nothing
    renderedMeta = DOM.renderToStaticMarkup $ Meta.staticPageMeta pageName

profilePage :: Env -> { guards :: { logger :: Unit } } -> Aff (Response ResponseBody)
profilePage env {} = do
  { mostReadArticles, latestArticles } <- parallelWithCommonLists env.cache $ pure unit
  let htmlTemplate = cloneTemplate env.htmlTemplate
      mosaicoString = renderToString
                          { mainContent:
                              { type: ProfileContent
                              , content:
                                  Profile.render
                                    { user: Nothing
                                    , onLogin: mempty
                                    , onLogout: mempty
                                    , onStaticPageClick: const mempty
                                    }
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
    appendMosaico mosaicoString htmlTemplate >>=
      appendVars (mkWindowVariables windowVars) >>=
      appendHead (makeTitle "Min profil")
  pure $ htmlContent $ Response.ok $ StringBody $ renderTemplateHtml html

notFoundPage
  :: Env
  -> { params :: { path :: List String }, guards :: { logger :: Unit } }
  -> Aff (Response ResponseBody)
notFoundPage env { params: { path } } = do
  -- TODO move redirect logic behind its own guard and route
  let redir to = pure $
        (\(Response r) -> Response $ r { headers = Headers.set "Location" to r.headers }) $
        Response.found EmptyBody
      pass = notFound env notFoundArticleContent mempty mempty
  case path of
    (route : Nil)      | Just destination <- Map.lookup (route /\ mosaicoPaper) env.redirects -> redir destination
    -- Same route with trailing slash
    (route : "" : Nil) | Just destination <- Map.lookup (route /\ mosaicoPaper) env.redirects -> redir destination
    -- Any other route with trailing slash: Redirect to the same without
    xs | List.last xs == Just ""                                                              ->
      redir $ ("/" <> _) $ List.intercalate "/" $ List.filter (_ /= "") xs
    _                                                                                         -> pass

corsProxyPage
  :: Env
  -> { query :: { url :: String }, guards :: { logger :: Unit } }
  -> Aff (Response ResponseBody)
corsProxyPage env { query: { url } } = do
  result <- Cache.readCorsResult env.cache url
  case result of
    Left err ->
      -- Whatever we get as an error, wrap that into console.warn
      -- so we can get some hint on why that embed failed.
      -- This is executed on the client side code.
      let errorMsg = JSON.stringify $ JSON.fromString err
          errorLog = "console.warn(" <> errorMsg <> ")"
      in  pure $ jsContent $ Response.unauthorized $ StringBody errorLog
    Right content -> pure $ jsContent $ Response.ok $ StringBody $ Cache.getContent content

parseCategory :: Env -> HTTP.Request -> Aff (Either Failure Category)
parseCategory { categoryRegex, categoryStructure } req = do
  let url = HTTP.requestURL req
      urlDecoded = fromMaybe url $ URI.decodeURIComponent url
      categoryRoute = CategoryLabel $ fold $ NonEmptyArray.last =<< Regex.match categoryRegex urlDecoded
      -- Flatten out categories from the category structure
      categories = correctionsCategory `cons`
                   foldl (\acc (Category c) -> acc <> [Category c] <> c.subCategories) [] categoryStructure
  case find ((_ == categoryRoute) <<< _.label <<< unwrap) categories of
    Just c -> pure $ Right c
    _ -> pure $ Left (Forward "Did not match category")
