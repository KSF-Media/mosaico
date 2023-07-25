module Mosaico.Feed where

import Prelude

import Data.Argonaut as JSON
import Data.Argonaut.Core (Json, fromArray)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (findMap, mapMaybe)
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Foreign.Object as Object
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), CategoryType(..), Tag(..), articleStubToJson, parseArticleStubWithoutLocalizing, frontpageCategoryLabel)
import Mosaico.Routes as Routes

parseFeed :: Json -> Maybe (Tuple ArticleFeedType ArticleFeed)
parseFeed = JSON.toObject >=> \feed -> do
  let lookup k = Object.lookup k feed >>= JSON.toString
      feedPage = lookup "feedPage"
  feedType <- do
    f <- lookup "feedType"
    case String.toLower f of
      "categoryfeed"  ->
        CategoryFeed
          <$> (hush <<< decodeJson =<< Object.lookup "categoryType" feed)
          <*> (CategoryLabel <$> feedPage)
      "tagfeed"      -> map (TagFeed <<< Tag) feedPage
      "searchfeed"   -> SearchFeed <$> feedPage
      "latestfeed"   -> Just LatestFeed
      "mostreadfeed" -> Just MostReadFeed
      "breakingnews" -> Just BreakingNewsFeed
      "advertorials" -> Just AdvertorialsFeed
      "debug"        -> DebugFeed <$> (UUID.parseUUID =<< feedPage)
      _              -> Nothing
  feedContent <- do
    content <- Object.lookup "feedContent" feed
    contentType <- lookup "feedContentType"
    case contentType of
      "articleList" -> do
        list <- hush $ decodeJson content
        pure $ ArticleList $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list
      "html" -> do
        obj <- hush $ decodeJson content
        list <- hush <<< decodeJson =<< Object.lookup "list" obj
        Html
          <$> (pure $ mapMaybe (hush <<< parseArticleStubWithoutLocalizing) list)
          <*> (hush <<< decodeJson =<< Object.lookup "html" obj)
      _ ->
        Nothing
  pure $ Tuple feedType feedContent

serializeFeed :: Tuple ArticleFeedType ArticleFeed -> Json
serializeFeed (Tuple feedDefinition feed) =
  encodeJson $ case feedDefinition of
    (CategoryFeed categoryType _) ->
      encodeJson { categoryType, feedType, feedPage, feedContent, feedContentType }
    _ ->
      encodeJson { feedType, feedPage, feedContent, feedContentType }
  where
    fromArticles = fromArray <<< map articleStubToJson
    (Tuple feedContentType feedContent) = case feed of
      ArticleList list -> Tuple "articleList" $ fromArticles list
      -- The prerendered HTML is rather large (at some 150kB) so it
      -- would be nice to have a way to not have it second time in a
      -- variable, but there's just no easy way around it.  React
      -- doesn't really have the option of just using the existing
      -- HTML on the page.  Hydrate can take over an existing DOM tree
      -- but it still expects to know how to render the same tree from
      -- its internal state.  Getting the HTML from the generated
      -- DOM's containing innerHTML would be ideal but it'd be a
      -- challenge.
      Html list html -> Tuple "html" $ encodeJson {html, list: fromArticles list}
    (Tuple feedType feedPage) = case feedDefinition of
      CategoryFeed _ page -> Tuple "categoryfeed" $ unwrap page
      TagFeed tag         -> Tuple "tagfeed" $ unwrap tag
      SearchFeed query    -> Tuple "searchfeed" query
      LatestFeed          -> Tuple "latestfeed" ""
      MostReadFeed        -> Tuple "mostreadfeed" ""
      BreakingNewsFeed    -> Tuple "breakingnews" ""
      AdvertorialsFeed    -> Tuple "advertorials" ""
      DebugFeed uuid      -> Tuple "debug" $ UUID.toString uuid

data ArticleFeed
  = ArticleList (Array ArticleStub)
  | Html (Array ArticleStub) String

data ArticleFeedType
  -- TODO using CategoryType as is is a bit awkward since the only
  -- values it can have here are Feed or Prerendered.
  = CategoryFeed CategoryType CategoryLabel
  | TagFeed Tag
  | SearchFeed String
  | LatestFeed
  | MostReadFeed
  | BreakingNewsFeed
  | AdvertorialsFeed
  | DebugFeed UUID
derive instance eqArticleFeedType :: Eq ArticleFeedType
derive instance ordArticleFeedType :: Ord ArticleFeedType

toList :: ArticleFeed -> Array ArticleStub
toList (ArticleList list) = list
toList _ = []

toHtml :: ArticleFeed -> String
toHtml (Html _ html) = html
toHtml _ = ""

-- A collection to spare downstream users from caring about
-- ArticleFeedType type.
type Feeds =
  { category :: Map CategoryLabel ArticleFeed
  , tag :: Map Tag (Array ArticleStub)
  , search :: Map String (Array ArticleStub)
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , breakingNews :: String
  -- Client side state
  , loading :: Int
  }

feedsFromInitial :: Array (Tuple ArticleFeedType ArticleFeed) -> Feeds
feedsFromInitial ini =
  { category: Map.fromFoldable $ mapMaybe
      (\a -> case a of
          (Tuple (CategoryFeed _ l) xs) -> Just $ Tuple l xs
          (Tuple (DebugFeed _) xs) -> Just $ Tuple (CategoryLabel "debug") xs
          _ -> Nothing) ini
  , tag: Map.fromFoldable $ mapMaybe
      (\a -> case a of
          Tuple (TagFeed t) (ArticleList xs) -> Just $ Tuple t xs
          _ -> Nothing) ini
  , search: Map.fromFoldable $ mapMaybe
      (\a -> case a of
          Tuple (SearchFeed q) (ArticleList xs) -> Just $ Tuple q xs
          _ -> Nothing) ini
  , mostReadArticles: fromMaybe [] $ findMap
      (\a -> case a of
          (Tuple MostReadFeed (ArticleList xs)) -> Just xs
          _ -> Nothing) ini
  , latestArticles: fromMaybe [] $ findMap
      (\a -> case a of
          (Tuple LatestFeed (ArticleList xs)) -> Just xs
          _ -> Nothing) ini
  , breakingNews: fromMaybe "" $ findMap
      (\a -> case a of
          (Tuple BreakingNewsFeed (Html _ x)) -> Just x
          _ -> Nothing) ini
  , loading: 0
  }

type RouteFeed =
  { feedType :: ArticleFeedType
  , limit :: Maybe Int
  }

routeFeed :: Routes.MosaicoPage -> Maybe RouteFeed
routeFeed Routes.Frontpage =
  Just { limit: Nothing, feedType: CategoryFeed Prerendered frontpageCategoryLabel }
routeFeed (Routes.CategoryPage (Category c) limit) =
  Just { limit, feedType: CategoryFeed c.type c.label }
routeFeed (Routes.TagPage tag limit) =
  Just { limit, feedType: TagFeed tag }
routeFeed (Routes.SearchPage (Just query) limit) =
  Just { limit, feedType: SearchFeed query }
routeFeed (Routes.DebugPage uuid) =
  Just { limit: Nothing, feedType: DebugFeed uuid }
routeFeed _ = Nothing

frontpageFeedType :: ArticleFeedType
frontpageFeedType = CategoryFeed Prerendered frontpageCategoryLabel
