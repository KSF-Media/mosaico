module Mosaico.Test.Embeds where

import Prelude hiding (sub)

import Mosaico.Test (Test, log, site)
import KSF.Puppeteer as Chrome
import Toppokki (unsafeEvaluateStringFunction) as Chrome

exampleArticle :: String
exampleArticle = "d175946e-ce35-448f-ab81-d93a5f2d8ea0"

testEmbedNavigation :: Test
testEmbedNavigation page = do
  log "Get debug list with example article"
  Chrome.goto (Chrome.URL $ site <> "debug/" <> exampleArticle) page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  log "Navigate to test article"
  Chrome.click (Chrome.Selector ".mosaico--article-list article") page
  log "Wait for article to load"
  Chrome.waitFor_ (Chrome.Selector "h1.mosaico-article__headline") page
  log "Accept cookies"
  _ <- Chrome.unsafeEvaluateStringFunction "window.consentToEmbeddedScripts(true)" page
  log "Wait for embed content"
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page

testEmbedServerRender :: Test
testEmbedServerRender page = do
  log "Go directly to embed article"
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> exampleArticle) page
  log "Wait for article to load"
  Chrome.waitFor_ (Chrome.Selector "h1.mosaico-article__headline") page
  log "Accept cookies"
  _ <- Chrome.unsafeEvaluateStringFunction "window.consentToEmbeddedScripts(true)" page
  log "Wait for embed content"
  Chrome.waitFor_ (Chrome.Selector "div.article-element__html iframe") page
