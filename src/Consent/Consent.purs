module Consent.Consent where

import Prelude

import Effect (Effect)

foreign import startConsentCookieSetupJS :: Effect Unit

startConsentCookieSetup :: Effect Unit
startConsentCookieSetup = startConsentCookieSetupJS