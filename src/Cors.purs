module Mosaico.Cors where

import Prelude

import Affjax (get, printError) as AX
import Affjax.ResponseFormat (string) as AX
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import KSF.Driver (getDriver)
import Affjax.StatusCode (StatusCode(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)

protocolRegex :: Regex
protocolRegex = unsafePartial $ fromJust $ hush $ regex """^(https?:)?//""" noFlags

toHttps :: String -> String
toHttps url = urlAsHttps
  where urlWithoutProtocol = replace protocolRegex "" url
        urlAsHttps = "https://" <> urlWithoutProtocol

fetchUrl :: String -> Aff (Either String String)
fetchUrl url = do
    driver <- liftEffect getDriver
    corsResponse <- AX.get driver AX.string (toHttps url)
    case corsResponse of
        Left err -> pure $ Left $ "Failed to fetch a resource through the CORS proxy" <> AX.printError err
        Right response
            | (StatusCode 200) <- response.status -> pure $ Right response.body
            | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s
