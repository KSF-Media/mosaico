module Mosaico.Test.Korsord where

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

testKorsord :: TestType -> String -> String -> Test
testKorsord testType user password page = do
  let paywallLogin = do
        Chrome.waitFor_ (Chrome.Selector ".vetrina--login-callback") page
        Account.login user password (Chrome.Selector ".vetrina--login-callback") 2 page

  case testType of
    Navigation -> do
      Chrome.goto (Chrome.URL site) page
      let korsordLink = Chrome.Selector "a[href='/korsord']"
      Chrome.waitFor_ korsordLink page
      Chrome.click korsordLink page
      paywallLogin
    Direct -> do
      Chrome.goto (Chrome.URL $ site <> "korsord") page
      paywallLogin
    SSO -> do
      Chrome.goto (Chrome.URL $ site <> "korsord") page
      paywallLogin
      reload page

  let puzzlePortal = Chrome.Selector "#puzzle-portal"
  Chrome.waitFor_ puzzlePortal page
  width <- Chrome.getWidth puzzlePortal page
  log $ "check korsord element opens in full size " <> show width
  Assert.assert "Korsord not full width" $ width > 1800
