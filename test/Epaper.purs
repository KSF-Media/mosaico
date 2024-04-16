module Mosaico.Test.Epaper where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT, lift)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import KSF.Paper (Paper(..))
import KSF.Puppeteer as Chrome
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Test (Test, assertNonEmpty, log, site, sub, reload)
import Mosaico.Test.Account as Account
import Test.Unit.Assert as Assert

data TestType = Navigation | Direct | SSO

testEpaper :: TestType -> String -> String -> Test
testEpaper testType user password page = do
  let paywallLogin = do
        let loginButton = (Chrome.Selector "*[data-test-epaperbody] *[data-test-primarybutton]")
        Chrome.waitFor_ loginButton page
        Chrome.click loginButton page
        Chrome.waitFor_ (Chrome.Selector ".mosaico--login-modal") page
        Account.login user password (Chrome.Selector ".mosaico--login-modal") 2 page

  case testType of
    Navigation -> do
      Chrome.goto (Chrome.URL site) page
      let epaperLink = Chrome.Selector "a[href='/epaper']"
      Chrome.waitFor_ epaperLink page
      Chrome.click epaperLink page
      paywallLogin
    Direct -> do
      Chrome.goto (Chrome.URL $ site <> "epaper") page
      paywallLogin
    SSO -> do
      Chrome.goto (Chrome.URL $ site <> "epaper") page
      paywallLogin
      reload page

  let epaperButton = Chrome.Selector "*[data-test-epaperbody] *[data-test-primarylink]"
  Chrome.waitFor_ epaperButton page
  epaperButtonText <- Chrome.getContent epaperButton page
  Assert.assert "Epaper button has logged in text" $ epaperButtonText == "Öppna det senaste numret"
