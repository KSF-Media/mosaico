module Mosaico.Test.Article where

import Prelude hiding (sub)

import Control.Alternative (guard)
import Control.Monad.Maybe.Trans (runMaybeT, lift)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import KSF.Paper (Paper(..))
import KSF.Puppeteer as Chrome
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Test (Test, assertNonEmpty, forward, log, site, sub, reload)
import Mosaico.Test.Account as Account
import Test.Unit.Assert as Assert

type PageIds =
  { articleId :: Maybe String
  , premiumArticleId :: Maybe String
  }

-- TODO find examples for all papers
relatedExample :: Maybe String
relatedExample = case mosaicoPaper of
  HBL -> Just "3152880e-3dcb-48d6-af4f-dc3d16379d78"
  VN -> Nothing
  ON -> Nothing
  _ -> Nothing

navigateToNews :: Chrome.Page -> Aff Unit
navigateToNews page = do
  Chrome.click (Chrome.Selector ".maskimage-menu") page
  links <- Chrome.findByText "a" "NYHETER" page
  link <- assertNonEmpty "Did not find link for the NYHETER category" links
  Chrome.clickElement link

testNewsPage :: Chrome.Page -> Aff PageIds
testNewsPage page = do
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  let articleList = Chrome.Selector ".mosaico--article-list"
  Chrome.waitFor_ articleList page
  nPremium <- Chrome.countElements articleList
              (Chrome.Selector "article[data-premium='1']") page
  nFree <- Chrome.countElements articleList
           (Chrome.Selector "article[data-premium='0']") page
  -- TODO It would be very unusual to have all articles on the news
  -- page either premium or free.  But allow for it in the test.
  -- Error if we have neither.
  Assert.assert "At least one article on front page list" $ nPremium + nFree > 0
  let article = Chrome.Selector "article.mosaico-article"
  premiumArticleId <- runMaybeT do
    guard $ nPremium > 0
    lift do
      let item = Chrome.Selector "article[data-premium='1']"
      log "Get first premium article from list"
      uuid <- Chrome.getData item "uuid" page
      articleListTest item
      premiumArticleTest article page
      log "Click to news page"
      -- Return to news page
      navigateToNews page
      Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
      pure uuid
  articleId <-runMaybeT do
    guard $ nFree > 0
    lift do
      let item = Chrome.Selector "article[data-premium='0']"
      log "Get first free article from list"
      uuid <- Chrome.getData item "uuid" page
      articleListTest item
      -- Test for lack of premium badge
      Chrome.assertNotFound (sub " .mosaico-article__tag-n-share .premium-badge" article) page
      -- Return to front page
      navigateToNews page
      Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
      pure uuid
  pure { premiumArticleId, articleId }
  where
    -- Navigates to the article and leaves page there
    articleListTest :: Chrome.Selector -> Aff Unit
    articleListTest sel = do
      tag <- Chrome.getContent (sub " .mosaico-article__tag" sel) page
      Chrome.click sel page
      let article = Chrome.Selector "article.mosaico-article"
      Chrome.waitFor_ article page
      -- Tag content should match
      Chrome.assertContent (sub " .mosaico-article__tag" article) tag page

-- Test direct loading of an article
testFreeArticle :: String -> Test
testFreeArticle uuid page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> uuid) page
  let article = Chrome.Selector "article.mosaico-article"
  Chrome.waitFor_ article page
  Chrome.waitFor_ (sub " .mosaico-article__main" article) page
  Chrome.assertNotFound (sub " .mosaico-article__tag-n-share .premium-badge" article) page

-- Tests for non-privileged/not logged in user
premiumArticleTest :: Chrome.Selector -> Test
premiumArticleTest sel page = do
  -- Test that we see paywall
  Chrome.waitFor_ (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" sel) page
  -- Test for premium badge
  Chrome.waitFor_ (sub " .mosaico-article__tag-n-share .premium-badge" sel) page
  -- Test that there's at most three elements of content
  let article = Chrome.Selector "article.mosaico-article"
  elementCount <- Chrome.countElements article (Chrome.Selector ".mosaico-article__body .article-element") page
  Assert.assert "Shouldn't see premium content without being logged in" $ elementCount < 4

navigateTo :: String -> Chrome.Page -> Aff Unit
navigateTo uuid page = do
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  Chrome.waitFor_ (Chrome.Selector ".mosaico--article-list") page
  let item = Chrome.Selector $ "article[data-uuid='" <> uuid <> "']"
  Chrome.click item page

testPaywallLogin :: Boolean -> String -> String -> String -> (Chrome.Selector -> Int -> Test) -> Test
testPaywallLogin loadDirect uuid user password f page = do
  log $ "Paywall login, direct " <> show loadDirect <> " article " <> uuid
  if loadDirect
    then Chrome.goto (Chrome.URL $ site <> "artikel/" <> uuid) page
    else navigateTo uuid page
  let article = Chrome.Selector "article.mosaico-article"
  premiumArticleTest article page
  -- Edge case: If premium article has only one body element, it
  -- renders with no body elements with paywall
  originalBlocks <- Chrome.countElements article (Chrome.Selector ".mosaico-article__body .article-element") page
  Assert.assert "Maximum of three content blocks shown in paywall" $ originalBlocks < 4
  -- Test login
  Account.login user password (sub " .vetrina--login-callback" article) 2 page
  f article originalBlocks page

testPaywallOpen :: Chrome.Selector -> Int -> Test
testPaywallOpen article originalBlocks page = do
  log "Check article has more content after login"
  Chrome.waitFor_ (sub (" .mosaico-article__body *:nth-of-type("<> show (originalBlocks+1) <>")") article) page
  Chrome.waitFor_ (sub (" .premium-only") article) page
  log "Check that Vetrina is gone"
  Chrome.assertNotFound (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" article) page
  log "Test that opening premium article with the same session via a list shows content"
  navigateToNews page
  navigateToPremium
  -- Same test via loading front page directly
  log "Test opening premium article with new session"
  Chrome.goto (Chrome.URL $ site <> "nyheter") page
  -- Flaky, disable for now
  -- This ends up going to a non-premium article and the tests fails because of it
  -- navigateToPremium
  where
    navigateToPremium = do
      log "Navigate to premium"
      let articleList = Chrome.Selector ".mosaico--article-list"
          item = (sub " article[data-premium='1']" articleList)
      Chrome.waitFor_ articleList page
      -- Without this, the click to open article seems to sometimes
      -- pick the first one.  Dunno why.
      Chrome.waitFor_ (sub " .mosaico-article__tag" articleList) page
      nPremium <- Chrome.countElements articleList item page
      if nPremium == 0 then log "No premium articles on news page, skip navigation test"
        else do
        Chrome.click item page
        -- Test for premium badge
        Chrome.waitFor_ (sub " .mosaico-article__tag-n-share .premium-badge" article) page
        Chrome.assertNotFound (sub " .vetrina--container" article) page

testPaywallHolds :: Chrome.Selector -> Int -> Test
testPaywallHolds article originalBlocks page = do
  Chrome.waitFor_ (Chrome.Selector ".mosaico-article__body .article-element") page
  paywallBlocks <- Chrome.countElements article (Chrome.Selector ".mosaico-article__body .article-element") page
  Assert.assert "Login without entitlements gives displays the same content" $ paywallBlocks == originalBlocks
  Chrome.waitFor_ (sub " .mosaico-article__main .mosaico-article__body .vetrina--container" article) page

testPaywallSSO :: Chrome.Selector -> Int -> Test
testPaywallSSO article _ page = do
  Chrome.waitFor_ (Chrome.Selector ".mosaico-article__body .article-element") page
  log "Reload page to trigger SSO"
  reload page
  Chrome.waitFor_ (sub " .premium-only" article) page

testRelated :: Test
testRelated =
  maybe (const $ log "No example related defined for paper, skip") testRelatedWith relatedExample

testRelatedWith :: String -> Test
testRelatedWith example page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/" <> example) page
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .article-element__related") page
  originalTitle <- Chrome.getContent (Chrome.Selector "article.mosaico-article .mosaico-article__headline") page
  Chrome.click (Chrome.Selector "article.mosaico-article .article-element__related a") page
  Chrome.waitFor_ (Chrome.Selector "article.mosaico-article .article-element__html") page
  newTitle <- Chrome.getContent (Chrome.Selector "article.mosaico-article .mosaico-article__headline") page
  Assert.assert "Article title changes after clicking on a related article" $ originalTitle /= newTitle

testDraftArticle :: Test
testDraftArticle page = do
  Chrome.goto (Chrome.URL $ site <> "artikel/draft/test") page
  -- Ensure that React hydrate has been run
  Chrome.waitFor_ (Chrome.Selector ".maskimage-login") page
  let article = Chrome.Selector "article.mosaico-article"
  draftContent <- Chrome.getContent (sub " .article-element__html" article) page
  Assert.assert "Draft content matches with test data" $ draftContent == "Just some draft content"

testNavigationLoads :: Chrome.Page -> Aff Unit
testNavigationLoads page = do
  let getHeadline = do
        Chrome.waitFor_ (Chrome.Selector ".mosaico-article__body") page
        Chrome.getContent (Chrome.Selector ".mosaico-article__headline") page
  Chrome.goto (Chrome.URL site) page
  Chrome.waitFor_ (Chrome.Selector ".mosaico-asidelist__latest .list-article-liftup") page
  Chrome.click (Chrome.Selector ".mosaico-asidelist__latest li:nth-child(1) a") page
  headline1 <- getHeadline
  Chrome.click (Chrome.Selector ".mosaico-asidelist__latest li:nth-child(2) a") page
  headline2 <- getHeadline
  if headline1 == headline2
    then
      -- It's no use trying to code defensively against this with
      -- content checks.  Just don't do it.
      log "The last two articles in the latest list have identical headlines.  It's not an error as such but this test is aborted because of that."
    else do
      Chrome.back page
      headline1' <- getHeadline
      Assert.assert "Article headlines match after back navigation" $ headline1 == headline1'
      forward page
      headline2' <- getHeadline
      Assert.assert "Article headlines match after forward navigation" $ headline2 == headline2'
