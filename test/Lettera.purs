-- Tests which compare rendered data to what's got from Lettera
module Mosaico.Test.Lettera where

import Prelude hiding (sub)

import Data.Array (head)
import Data.Either (Either(..))
import Data.Foldable (any, traverse_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), stripPrefix, toUpper)
import Data.UUID (UUID)
import Data.UUID as UUID
import KSF.Paper (Paper(..))
import Lettera as Lettera
import Lettera.Models (Category(..), CategoryType(..), Platform(Desktop))
import Lettera.Models as Models
import Mosaico.Test (Test, listArticle, log, matchTagList, site, sub, tagListWithSelector, assertNonEmpty)
import Partial.Unsafe (unsafePartial)
import KSF.Puppeteer as Chrome
import Test.Unit as Unit
import Test.Unit.Assert as Assert

listTitleExample :: UUID
listTitleExample = unsafePartial $ fromJust $ UUID.parseUUID "6555d4ff-cbd0-4a85-ac60-4967166704a4"

missingListTitleExample :: UUID
missingListTitleExample = unsafePartial $ fromJust $ UUID.parseUUID "f4b6ca52-6078-43c8-b31f-9df85087dbff"

testListTitle :: Test
testListTitle page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> UUID.toString listTitleExample) page
  maybeStub <- Lettera.getArticleStub listTitleExample
  case maybeStub of
    Left err ->
      Unit.failure $ "Fetching example article stub failed: " <> err
    Right stub -> do
      Chrome.waitFor_ listArticle page
      log "List uses listTitle"
      -- The example should have a listTitle
      Chrome.assertContent (sub " h3" listArticle) (fromMaybe "☃INVALID" stub.listTitle) page
      Chrome.click listArticle page
      log "Article uses title"
      Chrome.waitFor_ (Chrome.Selector "article.mosaico-article") page
      Chrome.assertContent (Chrome.Selector "article.mosaico-article h1.mosaico-article__headline") stub.title page
      log "Article uses title after full load"
      Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .mosaico-article__body *[class^='article-element']") page
      Chrome.assertContent (Chrome.Selector "article.mosaico-article h1.mosaico-article__headline") stub.title page

testDefaultListTitle :: Test
testDefaultListTitle page = do
  Chrome.goto (Chrome.URL $ site <> "debug/" <> UUID.toString missingListTitleExample) page
  maybeStub <- Lettera.getArticleStub missingListTitleExample
  case maybeStub of
    Left err ->
      Unit.failure $ "Fetching example article stub failed: " <> err
    Right stub -> do
      Chrome.waitFor_ listArticle page
      log "List defaults to using title"
      Chrome.assertContent (sub " h3" listArticle) stub.title page

-- TODO do this for other papers as well

-- TODO there is a (very small?) chance that the feed would be changed
-- between the render and Lettera fetch.  If that becomes an issue run
-- this test twice if it fails once
testCategoryLists :: Test
testCategoryLists page = do
  Chrome.goto (Chrome.URL $ site <> "meny") page
  categories <- Lettera.getCategoryStructure (Just Desktop) HBL
  traverse_ testCategory categories
  where
    testCategory (Category c)
      | c.type == Feed = do
        log $ "test feed category " <> show c.label
        letteraArticles <- fromMaybe [] <<< Lettera.responseBody <$>
                           Lettera.getFrontpage HBL Nothing Nothing (Just $ show c.label) Nothing
        catElements <- Chrome.findByText "a" (toUpper $ show c.label) page
        catElement <- assertNonEmpty "Did not find link for the category" catElements

        Chrome.clickElement catElement
        Chrome.waitFor_ listArticle page
        uuids <- tagListWithSelector 10 $ \i ->
          fromMaybe "" <<< stripPrefix (Pattern "/artikel/")
          <$> Chrome.getHref (sub (":nth-child(" <> show i <> ") a[href^='/artikel/']") listArticle) page
        Assert.assert "No UUIDs found" $ any (_ /= "") $ map _.content uuids
        let matching = matchTagList uuids letteraArticles $ \uuid stub -> uuid == stub.uuid
        case head matching of
          Nothing ->
            Unit.failure "No common articles found in Lettera's and Mosaico's category page"
          Just {i, match} -> do
            let title = fromMaybe match.title match.listTitle
            Chrome.assertContent (sub (":nth-child(" <> show i <> ") h3") listArticle) title page
        Chrome.goto (Chrome.URL $ site <> "meny") page
      | otherwise = do
        log $ "No test for category type " <> Models.toString c.type <> " (" <> show c.label <> ")"
