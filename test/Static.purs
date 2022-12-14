module Mosaico.Test.Static where

import Prelude hiding (sub)

import Data.Array.Partial (head)
import Partial.Unsafe (unsafePartial)
import Mosaico.Test (Test, log, site)
import KSF.Puppeteer as Chrome
import Toppokki (waitForSelector) as Chrome
import Test.Unit.Assert as Assert

testNavigateToStatic :: Test
testNavigateToStatic page = do
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ (Chrome.Selector "footer") page
  -- Bruksvillkor
  log "Navigate to bruksvillkor"
  bruksvillkorLinks <- Chrome.findByText "a" "Bruksvillkor" page
  Chrome.clickElement (unsafePartial $ head bruksvillkorLinks)
  Chrome.waitFor_ (Chrome.Selector ".terms__text") page
  log "Navigate to front page"
  Chrome.click (Chrome.Selector ".mosaico-header__logo") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-main") page
  Chrome.back page
  Chrome.waitFor_ (Chrome.Selector ".terms__text") page

testStaticEmbeds :: Test
testStaticEmbeds page = do
  Chrome.goto' (Chrome.URL $ site <> "sida/fragor-och-svar") page
  testQuestion
  log "Navigate to front page and back"
  Chrome.click (Chrome.Selector ".mosaico-header__logo") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-main") page
  Chrome.back page
  testQuestion
  where
    testQuestion = do
      log "Questions start as unopened"
      Chrome.waitFor_ (Chrome.Selector ".faq__question") page
      void $ Chrome.waitForSelector (Chrome.Selector ".faq__answer") {visible: false, timeout: 30000} page
      log "Open first question"
      Chrome.click (Chrome.Selector ".faq__question") page
      Chrome.waitFor_ (Chrome.Selector ".faq__answer") page
      log "Clicking a question again will close it"
      Chrome.click (Chrome.Selector ".faq__question") page
      void $ Chrome.waitForSelector (Chrome.Selector ".faq__answer") {visible: false, timeout: 30000} page
