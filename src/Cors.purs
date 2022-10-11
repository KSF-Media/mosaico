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

isWhitelisted :: String -> Maybe String
isWhitelisted url = case urlWithoutProtocol of
    "www.riddle.com/embed/files/js/embed.js" -> Just urlAsHttps
    "platform.instagram.com/en_US/embeds.js" -> Just urlAsHttps
    "www.instagram.com/embed.js" -> Just urlAsHttps
    "platform.twitter.com/widgets.js" -> Just urlAsHttps
    "embed.tt.se/v10/tt-widget.js" -> Just urlAsHttps
    _ -> Nothing
  where urlWithoutProtocol = replace protocolRegex "" url
        urlAsHttps = "https://" <> urlWithoutProtocol

fetchUrl :: String -> Aff (Either String String)
fetchUrl resource = case isWhitelisted resource of
    Just url -> do
        driver <- liftEffect getDriver
        corsResponse <- AX.get driver AX.string url
        case corsResponse of
            Left err -> pure $ Left $ "Failed to fetch a resource through the CORS proxy" <> AX.printError err
            Right response
                | (StatusCode 200) <- response.status -> pure $ Right response.body
                | (StatusCode s) <- response.status -> pure $ Left $ "Unexpected HTTP status: " <> show s
    Nothing -> pure $ Left "URL not whitelisted"
