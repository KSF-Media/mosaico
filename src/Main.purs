module Main where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Foreign.Object (lookup)
import KSF.Paper as Paper
import Mosaico.Cache as Cache
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Routes as Routes
import Mosaico.Server.Env (Env, newEnv)
import Mosaico.Server.Output (jsContent)
import Mosaico.Server.Site (defaultHandler, getClientRoute)
import Mosaico.Version (mosaicoVersion)
import Node.HTTP as HTTP
import Payload.Headers as Headers
import Payload.ResponseTypes (Failure, Response(..), ResponseBody(..))
import Payload.Server as Payload
import Payload.Server.Guards as Guards
import Payload.Server.Handlers (File)
import Payload.Server.Handlers as Handlers
import Payload.Server.Response as Response
import Payload.Spec (type (:), GET, Guards, Spec(Spec), Nil)

foreign import log :: forall a . a -> Effect Unit
foreign import logPretty :: forall a . a -> Effect Unit
foreign import logJsEnv :: Unit -> Effect Unit
foreign import serverPort :: Int
foreign import decodeURIComponent_ :: String -> String

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
         , robotsTxt ::
              GET "/robots.txt"
                { response :: File }
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
         , corsProxy ::
              GET "/corsProxy?url=<url>"
                { response :: ResponseBody
                , query :: { url :: String }
                }
         , defaultHandler ::
              GET "/<..path>?headless=<headless>"
                { response :: ResponseBody
                  -- Discarded but required to make a catch all route
                , params :: { path :: List String }
                , query :: { headless :: Maybe Boolean }
                , guards :: Guards ("redirection" : "clientip" : "clientRoute" : Nil)
                }
         , rootHandler ::
              GET "/?headless=<headless>"
                { response :: ResponseBody
                , query :: { headless :: Maybe Boolean }
                , guards :: Guards ("redirection" : "clientip" : "clientRoute" : Nil)
                }
         , guards :: Guards ( "logger" : Nil )
         }
    , guards ::
         { clientip :: Maybe String
         , triggerbee :: Maybe TriggerbeeCookies
         , logger :: Unit
         , clientRoute :: Routes.MosaicoPage
         , redirection :: Maybe String
         }
    }
spec = Spec

main :: Effect Unit
main = do
  Aff.launchAff_ do
    liftEffect $ logJsEnv unit
    env <- newEnv
    liftEffect $ logPretty {env}
    let handlers =
          { getHealthz
          , googleSiteVerification: staticAsset GoogleSiteVerification
          , ssoCallbackReceiver: staticAsset SSOReceiver
          , setTriggerbeeCookies
          , assets
          , defaultHandler: defaultHandler env Nothing
          , rootHandler: defaultHandler env Nothing
          , adsTxt: staticAsset AdsTXT
          , adnamiDomainEnabler: staticAsset AdnamiDomainEnabler
          , corsProxy: corsProxyPage env
          , robotsTxt: staticAsset RobotsTXT
          }
        guards =
          { clientip: getClientIP
          , triggerbee
          , logger: loggerGuard
          , clientRoute: getClientRoute env
          , redirection: resolveRedir env
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

resolveRedir :: Env -> HTTP.Request -> Aff (Maybe String)
resolveRedir env req = pure $
  -- HTTP.requestURL returns the path, not the URL
  -- eg "/kontakt" and not "https://www.hbl.fi/kontakt"
  flip Map.lookup env.redirects $
  (_ /\ mosaicoPaper) $
  decodeURIComponent_ $
  intercalate "/" $
  filter (_ /= "") $
  String.split (String.Pattern "/") $
  HTTP.requestURL req

assets :: { params :: { path :: List String }, guards :: { logger :: Unit } } -> Aff (Either Failure File)
assets { params: { path } } = Handlers.directory "dist/assets" path

data StaticAsset = AdsTXT | AdnamiDomainEnabler | GoogleSiteVerification | SSOReceiver | RobotsTXT

staticAsset :: forall r. StaticAsset -> { | r} -> Aff File
staticAsset asset = Handlers.file $ case asset of
  AdsTXT -> "dist/assets/ads.txt"
  AdnamiDomainEnabler -> "dist/assets/adnami/adnm.html"
  GoogleSiteVerification -> "dist/assets/google8c22fe93f3684c84.html"
  SSOReceiver -> "dist/assets/xd_receiver.html"
  RobotsTXT -> "dist/assets/robots.txt"

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
