module Test.Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class.Console (log)
import Mosaico.Test.Account as Account
import Mosaico.Test.Article as Article
import Mosaico.Test.Embeds as Embeds
import Mosaico.Test.Frontpage as Frontpage
import Mosaico.Test.Korsord as Korsord
import Mosaico.Test.Layout as Layout
import Mosaico.Test.Lettera as Lettera
import Mosaico.Test.Redir as Redir
import Mosaico.Test.Search as Search
import Mosaico.Test.Static as Static
import Mosaico.Test.Tags as Tags
import KSF.Puppeteer as Chrome

foreign import testUser :: String
foreign import testPassword :: String
foreign import entitledUser :: String
foreign import entitledPassword :: String

defaultArticleId :: String
defaultArticleId = "df6e4abe-a43c-4a75-af04-52a09eb5e335"

defaultPremiumArticleId :: String
defaultPremiumArticleId = "cf100445-d2d8-418a-b190-79d0937bf7fe"

main :: Effect Unit
main = launchAff_ do
  log "Test redirect"
  withBrowserPage Redir.testRedir

  log "Test draft article"
  withBrowserPage $ Article.testDraftArticle

  -- Output test users.  Will be printed to Gitlab CI logs but it
  -- scrubs passwords from the output.
  log "--- Test Accounts ---"
  log ("TEST_USER: " <> testUser)
  log ("TEST_PASSWORD: " <> testPassword)
  log ("ENTITLED_USER: " <> entitledUser)
  log ("ENTITLED_PASSWORD: " <> entitledPassword)
  log "--- --- ----- --- ---"

  let loginTestUser = if testUser == "" then entitledUser else testUser
      loginTestPassword = if testPassword == "" then entitledPassword else testPassword
  if loginTestUser == "" || loginTestPassword == ""
    then log "skip login and logout test, user or password not set"
    else do
    log "Test login and logout"
    withBrowserPage $ Account.loginLogout loginTestUser loginTestPassword

  log "Test news page and get free article and premium article"
  { articleId, premiumArticleId } <- withBrowserPage Article.testNewsPage
  log $ "Free article " <> show articleId <> " premium " <> show premiumArticleId
  let premiumUuid = fromMaybe defaultPremiumArticleId premiumArticleId
  log $ "Using premium " <> premiumUuid

  log "Test free article"
  withBrowserPage $ Article.testFreeArticle (fromMaybe defaultArticleId articleId)

  if testUser == "" || testPassword == ""
    then log "skip unentitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> do
        withBrowserPage $
          Article.testPaywallLogin false uuid testUser testPassword Article.testPaywallHolds
      _ -> log "Skip paywall hold test via navigation"
    log "Test paywall holds, direct"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid testUser testPassword Article.testPaywallHolds

  if entitledUser == "" || entitledPassword == ""
    then log "skip entitled paywall test, user or password not set"
    else do
    case premiumArticleId of
      Just uuid -> do
        log "Test paywall opens, navigation"
        withDesktopBrowserPage $
          Article.testPaywallLogin false uuid entitledUser entitledPassword Article.testPaywallOpen
      _ -> log "Skip paywall open test via navigation"
    log "Test paywall opens, direct"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid entitledUser entitledPassword Article.testPaywallOpen
    log "Test paywall opens, SSO"
    withBrowserPage $ Article.testPaywallLogin true premiumUuid entitledUser entitledPassword Article.testPaywallSSO

    log "Test korsord, navigation"
    withDesktopBrowserPage $ Korsord.testKorsord Korsord.Navigation entitledUser entitledPassword
    log "Test korsord, direct"
    withDesktopBrowserPage $ Korsord.testKorsord Korsord.Direct entitledUser entitledPassword
    log "Test korsord, SSO"
    withDesktopBrowserPage $ Korsord.testKorsord Korsord.SSO entitledUser entitledPassword

  log "Test related article links"
  withBrowserPage Article.testRelated

  log "Test article loads after forward/back navigation"
  -- Desktop layout to have the latest list visible
  withDesktopBrowserPage Article.testNavigationLoads

  log "Test CSS has loaded"
  withBrowserPage Layout.testLayout
  log "Test front page embedded HTML"
  withBrowserPage Frontpage.testHtmlEmbed
  withBrowserPage Frontpage.testHtmlEmbedNavigation
  -- We are sharing the same GA endpoint for staging and production,
  -- and the read amounts from prod trump over staging. This means that
  -- usually we get something between 0 and 2 articles in the most read
  -- list in staging, which would fail this test.
  --log "Test most read list"
  --withDesktopBrowserPage $ Frontpage.testMostRead
  log "Test embed render via navigation"
  withBrowserPage Embeds.testEmbedNavigation
  log "Test embed render, direct"
  withBrowserPage Embeds.testEmbedServerRender
  log "Test search via navigation"
  withBrowserPage Search.testSearchNavigation
  log "Test search direct"
  withBrowserPage Search.testSearchServerRender
  log "Search with a not found search word"
  withBrowserPage Search.testFailingSearch
  log "Test tag list"
  withBrowserPage Tags.testTagList
  log "Test static pages"
  withBrowserPage Static.testNavigateToStatic
  withBrowserPage Static.testStaticEmbeds
  log "Test listTitle field"
  withBrowserPage Lettera.testListTitle
  withBrowserPage Lettera.testDefaultListTitle
  log "Test categories"
  withBrowserPage Lettera.testCategoryLists
  where
    withBrowser :: forall a. Aff Chrome.Browser -> (Chrome.Browser -> Aff a) -> Aff a
    withBrowser = flip bracket Chrome.close

    withBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withBrowserPage f = withBrowser Chrome.launch (f <=< Chrome.newPage)

    withDesktopBrowserPage :: forall a. (Chrome.Page -> Aff a) -> Aff a
    withDesktopBrowserPage f = withBrowser Chrome.launchDesktop (f <=< Chrome.newPage)
