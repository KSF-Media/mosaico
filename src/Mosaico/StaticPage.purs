module Mosaico.StaticPage where

import Prelude

import Affjax (get) as AX
import Affjax.ResponseFormat (string) as AX
import Affjax.StatusCode (StatusCode(..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Nullable (Nullable)
import Data.String (toLower)
import Data.String.Regex (regex, test) as Regex
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import KSF.Driver (getDriver)
import KSF.Paper as Paper
import Mosaico.Paper (mosaicoPaper)

foreign import getInitialStaticPageContent :: Effect (Nullable String)
foreign import getInitialStaticPageScript :: Effect (Nullable String)

data StaticPageResponse
  = StaticPageResponse StaticPage
  | StaticPageNotFound
  | StaticPageOtherError

type StaticPage =
  { pageName :: String
  , pageContent :: String
  , pageScript :: Maybe String
  }

isValidPageName :: String -> Boolean
isValidPageName pageName =
  let regex = hush $ Regex.regex "^[a-z-]+$" mempty
  in fromMaybe false do
    r <- regex
    pure $ Regex.test r pageName

fetchStaticPage :: String -> Aff StaticPageResponse
fetchStaticPage pageName = do
  --Static pages' html files can have different content depending on the paper, therefore they are found in paper specific directory.
  --The js files have the same content regardless of paper, therefore they are found directly in the static directory
  let staticPageUrl = "/assets/" <> (toLower $ Paper.toString mosaicoPaper) <> "/" <> pageName <> ".html"
  let staticPageJsUrl = "/assets/" <> pageName <> ".js"
  driver <- liftEffect getDriver
  resPage <- AX.get driver AX.string staticPageUrl
  resJs <- AX.get driver AX.string staticPageJsUrl
  let jsContent eitherJs = do
          jsRes <- hush eitherJs
          guard (jsRes.status == StatusCode 200) Just jsRes.body
  pure case resPage of
    Right pageContentResponse ->
      case pageContentResponse.status of
        StatusCode 200 -> StaticPageResponse { pageName, pageContent: pageContentResponse.body, pageScript: jsContent resJs }
        StatusCode 404 -> StaticPageNotFound
        _ -> StaticPageOtherError
    Left _err -> StaticPageOtherError
