module Mosaico.Test.Search where

import Prelude hiding (sub)

import Mosaico.Test (Test, assertNonEmpty, listArticle, log, site, sub)
import KSF.Puppeteer as Chrome

exampleSearch :: String
exampleSearch = "hbl"

-- Just something that never should match any article
exampleNegativeSearch :: String
exampleNegativeSearch = "vsdlkjfdskfajadskfjaoiefjlksadfjlkdsafjdaslakjf"

buttonField :: Chrome.Selector
buttonField = Chrome.Selector ".mosaico-search button"

searchField :: Chrome.Selector
searchField = Chrome.Selector ".mosaico-search input"

pageTitle :: Chrome.Selector
pageTitle = Chrome.Selector ".mosaico--article-list > h2"

testSearchNavigation :: Test
testSearchNavigation page = do
  let searchButton = Chrome.Selector ".maskimage-search"
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ searchButton page
  Chrome.click searchButton page
  testExampleSearch page

testSearchServerRender :: Test
testSearchServerRender page = do
  Chrome.goto (Chrome.URL $ site <> "sök") page
  testExampleSearch page

testExampleSearch :: Test
testExampleSearch page = do
  log "Search is disabled in initial state"
  Chrome.waitFor_ (sub "[disabled]" buttonField) page
  Chrome.type_ (Chrome.Selector ".mosaico-search input") exampleSearch page
  Chrome.assertNotFound (sub "[disabled]" buttonField) page
  Chrome.click buttonField page
  log "Wait for search results"
  _ <- assertNonEmpty "Couldn't find expected search page header" <$> Chrome.findByText "h2" ("Sökresultat: " <> exampleSearch) page
  Chrome.waitFor_ listArticle page

testFailingSearch :: Test
testFailingSearch page = do
  Chrome.goto (Chrome.URL $ site <> "sök") page
  Chrome.waitFor_ searchField page
  Chrome.type_ (Chrome.Selector ".mosaico-search input") exampleNegativeSearch page
  Chrome.click buttonField page
  _ <- assertNonEmpty "Couldn't find expected search page header" <$> Chrome.findByText "h2" "Inga resultat" page
  Chrome.assertNotFound listArticle page
