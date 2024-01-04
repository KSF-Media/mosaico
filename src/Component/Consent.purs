module Mosaico.Component.Consent where

import Prelude

import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe)
import Data.Nullable as Nullable
import Data.String as String
import Data.String.Regex as Regex
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (Component, useEffectOnce)
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as Web
import Web.HTML.Location as Location
import Web.HTML.Window (location, toEventTarget) as Web
import Debug

type Consent =
  { necessary :: Boolean
  , preferences :: Boolean
  , statistics :: Boolean
  , marketing :: Boolean
  }

type CookieBot =
  { consent :: Consent
  , consented :: Boolean
  , declined :: Boolean
  , hasResponse :: Boolean
  , doNotTrack :: Boolean
  }

foreign import readCookieBotProperties_ :: Effect (Nullable.Nullable CookieBot)

readCookieBot :: Effect (Maybe CookieBot)
readCookieBot = Nullable.toMaybe <$> readCookieBotProperties_

foreign import showCookieBot :: CookieBot -> Effect Unit

openConsent :: Effect Boolean
openConsent = readCookieBot >>= maybe (pure false) (\bot -> showCookieBot bot *> pure true)

type Props =
  { setConsent :: Boolean -> Effect Unit
  }

-- Does not render any JSX by itself
component :: Component Props
component = do
  window <- Web.window
  location <- Web.location window
  host <- Location.hostname location
  pathname <- Location.pathname location
  -- DrPublish preview uses these hosts
  let rg = Regex.regex "/^mosaico-(.*).api/" mempty
      override = host == "localhost" || String.take 15 pathname == "/artikel/draft/" ||
                 either (const false) (flip Regex.test host) rg
  React.component "Consent" $ \props -> React.do
    useEffectOnce do
      if override then props.setConsent true *> pure mempty else do
        let et = Web.toEventTarget window
            eventTypes =
              [ EventType "CookiebotOnAccept"
              , EventType "CookiebotOnDecline"
              , EventType "CookiebotOnConsentReady"
              , EventType "CookiebotOnDialogInit"
              ]
            testType = EventType "TestCookiebotOverride"
        listener <- eventListener $ const $
          foldMap (props.setConsent <<< _.marketing <<< _.consent) =<< readCookieBot
        testListener <- eventListener $ const do
          props.setConsent true
        for_ eventTypes \eType -> addEventListener eType listener false et
        addEventListener testType testListener false et
        pure do
          for_ eventTypes \eType -> removeEventListener eType listener false et
          removeEventListener testType testListener false et
    pure mempty
