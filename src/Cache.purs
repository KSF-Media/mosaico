module Mosaico.Cache where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Rec.Class (untilJust)
import Control.Parallel.Class (parallel, sequential)
import Control.Plus (class Plus, empty)
import Data.Array (cons, filter, fromFoldable, length, mapMaybe, take)
import Data.DateTime (DateTime, adjust, diff)
import Data.Either (Either(..), hush)
import Data.Foldable (foldr)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (toNumber, floor)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Profunctor (class Profunctor, rmap)
import Data.String (null)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (class Foldable, class Traversable, foldMap, sequence, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.AVar as Effect.AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Now (nowDateTime)
import Lettera (LetteraResponse(..))
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category(..), CategoryLabel(..), CategoryType(Feed, Prerendered), Tag)
import Mosaico.Cors as Cors
import Mosaico.Feed (ArticleFeedType(..), ArticleFeed(..), RouteFeed, Feeds)
import Mosaico.Paper (mosaicoPaper)
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))

data Stamped a = Stamped
  { validUntil :: DateTime
    -- | Checked in off band cache set to guard against stale content
  , age :: DateTime
  , content :: a
  }

-- Always expired
emptyStamp :: forall m a. Plus m => Stamped (m a)
emptyStamp =
  Stamped { validUntil: bottom, age: bottom, content: empty }

-- Always valid
eotStamp :: forall m a. Plus m => Stamped (m a)
eotStamp =
  Stamped { validUntil: top, age: bottom, content: empty }

instance functorStamped :: Functor Stamped where
  map f (Stamped s@{ content }) = Stamped $ s { content = f content }

instance applyStamped :: Apply Stamped where
  apply (Stamped { validUntil: t1, age: a1, content: f}) (Stamped { validUntil: t2, age: a2, content: x}) =
    Stamped { validUntil: min t1 t2, age: max a1 a2, content: f x }

instance foldableStamped :: Foldable Stamped where
  foldMap f (Stamped { content }) = f content
  foldr f x (Stamped { content }) = content `f` x
  foldl f x (Stamped { content }) = x `f` content

instance traversableStamped :: Traversable Stamped where
  traverse f (Stamped s@{ content }) = Stamped <<< (s { content = _ }) <$> f content
  sequence (Stamped s@{ content }) = Stamped <<< (s { content = _ }) <$> content

getContent :: forall a. Stamped a -> a
getContent (Stamped { content }) = content

toValidUntil :: DateTime -> Maybe Int -> Maybe DateTime
toValidUntil now maxAge =
  -- Sanity check, allow at most 1 minute
  flip adjust now <<< Seconds <<< toNumber =<< max 60 <$> maxAge

data Controls a b = Controls
  { reset :: Aff Unit
  , set :: DateTime -> LetteraResponse a -> Aff Boolean
  , content :: Aff (Stamped b)
  }

instance profunctorControls :: Profunctor Controls where
  dimap f g (Controls c@{ content}) = Controls $ c
    { content = (map <<< map) g content
    , set = \a x -> c.set a $ f <$> x
    }

-- Reset handle and a value that's being kept updated
type UpdateWatch a = Controls a a

data CacheMode = Client | Server

-- prerendered, mainCategoryFeed and mostRead are updated eagerly from
-- Lettera.  byTag and subCategoryFeed is only updated on request.
type Cache =
  { prerendered :: Map CategoryLabel (Controls String (Maybe String))
  , mainCategoryFeed :: Map CategoryLabel (UpdateWatch (Array ArticleStub))
  , mostRead :: Aff (Stamped (Array ArticleStub))
  , latest :: Aff (Stamped (Array ArticleStub))
  -- Only used by server
  , advertorials :: Aff (Stamped (Array ArticleStub))
  , feedList :: AVar (Map ArticleFeedType (Stamped (Array ArticleStub)))
  , feedString :: AVar (Map ArticleFeedType (Stamped String))
  , mode :: CacheMode
  , setFeeds :: AVar ((Feeds -> Feeds) -> Effect Unit)
  -- Not using UpdateWatch for this, this is controlled by Main.
  , storedCategoryRender :: AVar (Map CategoryLabel (Stamped String))
  , corsProxyCache :: AVar (Map String (Stamped String))
  }

startUpdates :: forall a. (Maybe String -> Aff (LetteraResponse a)) -> Effect (Controls a (Maybe a))
startUpdates fetch = do
  -- Var is empty only if the update thread is not running.
  var <- Effect.AVar.empty
  resetLock <- Effect.AVar.new unit
  updateLock <- Effect.AVar.new unit
  updateDone <- Effect.AVar.empty
  updateStop <- Effect.AVar.empty
  offbandUpdate <- Effect.AVar.empty
  let withLock :: forall b. AVar Unit -> Aff b -> Aff b
      withLock lock = Aff.bracket (AVar.take lock) (flip AVar.put lock) <<< const
      update = untilJust do
        offband <- AVar.tryTake offbandUpdate
        LetteraResponse response <-
          maybe (fetch =<< hush <<< formatDateTime "YYYYMMDDHHmmss"
                 <$> liftEffect nowDateTime) (pure <<< _.offband) offband
        _ <- AVar.tryTake var
        age <- maybe (liftEffect nowDateTime) (pure <<< _.age) offband
        let validUntil = fromMaybe age $ toValidUntil age response.maxAge
        case response.body of
          Right content -> do
            AVar.put (Stamped { validUntil, age, content }) var
          Left err -> do
            Console.warn $ show err
        -- Tell the possible caller to continue, we either have a
        -- value now or they'll just have to do without.
        _ <- AVar.tryTake updateDone
        case response.maxAge of
          -- Quit repeating on error.  Start again on the next request
          -- for the cached resource.
          Nothing -> pure $ Just unit
          Just maxAge -> do
            delay <- Aff.forkAff do
              Aff.delay $ Aff.Milliseconds $ (_ * 1000.0) $ toNumber maxAge
              withLock resetLock $ AVar.put unit updateStop
            AVar.take updateStop
            Aff.killFiber (Exception.error "stop update") delay
            _ <- AVar.tryTake updateStop
            pure Nothing
      start = withLock updateLock do
        val <- AVar.tryRead var
        case val of
          Just v -> pure $ map Just v
          Nothing -> do
            AVar.put unit updateDone
            _ <- Aff.forkAff update
            -- This will pass when the update worker has completed one
            -- access to the resource.
            AVar.put unit updateDone
            -- Leave it empty for the next caller.
            AVar.take updateDone
            maybe emptyStamp (map Just) <$> AVar.tryRead var
      reset = withLock resetLock do
        AVar.put unit updateStop
      set newAge offband = withLock resetLock do
        Stamped {age} <- AVar.read var
        if age >= newAge then pure false else do
          AVar.put {offband, age: newAge} offbandUpdate
          AVar.put unit updateStop
          pure true

  Aff.launchAff_ $ void start
  pure $ Controls { reset, set, content: start }

withCat :: forall a m. Monad m => (String -> m a) -> Category -> m (Tuple CategoryLabel a)
withCat f (Category cat) =
  Tuple cat.label <$> f (show cat.label)

setClientFeeds :: Cache -> Array (Tuple ArticleFeedType ArticleFeed) -> ((Feeds -> Feeds) -> Effect Unit) -> Effect Unit
setClientFeeds cache initial sf = do
  now <- nowDateTime
  let validUntil = fromMaybe now $ toValidUntil now (Just 60)
      getList (ArticleList content) = Just $ Stamped { validUntil, age: now, content }
      getList _ = Nothing
      getHtml (Html _ content) = Just $ Stamped {validUntil, age: now, content }
      getHtml _ = Nothing
      initFeed :: forall a. (ArticleFeed -> Maybe a) -> AVar (Map ArticleFeedType a) -> Effect Unit
      initFeed f v = void $ Effect.AVar.tryPut (Map.fromFoldable $ mapMaybe (traverse f) initial) v
  void $ Effect.AVar.tryTake cache.feedList
  void $ Effect.AVar.tryTake cache.feedString
  void $ Effect.AVar.tryTake cache.setFeeds
  initFeed getList cache.feedList
  initFeed getHtml cache.feedString
  void $ Effect.AVar.tryPut sf cache.setFeeds

initClientCache :: Effect Cache
initClientCache = do
  storedCategoryRender <- Effect.AVar.new Map.empty
  corsProxyCache <- Effect.AVar.new Map.empty
  feedList <- Effect.AVar.new Map.empty
  feedString <- Effect.AVar.new Map.empty
  sf <- Effect.AVar.new $ const $ pure unit
  let static = pure emptyStamp
  pure
    { prerendered: Map.empty
    , mainCategoryFeed: Map.empty
    , mostRead: static
    , latest: static
    , advertorials: static
    , feedList
    , feedString
    , setFeeds: sf
    , mode: Client
    , storedCategoryRender
    , corsProxyCache
    }

initServerCache :: Array Category -> Effect Cache
initServerCache categoryStructure = do
  cache <- initClientCache
  let prerenderedCategories = filter (\(Category c) -> c.type == Prerendered) categoryStructure
  mainCategoryFeed <-
    (map <<< map <<< rmap) (join <<< fromFoldable)
    Map.fromFoldable
    <$> traverse (withCat $ startUpdates <<< Lettera.getFrontpage mosaicoPaper Nothing Nothing <<< Just) categoryStructure
  prerendered <- Map.fromFoldable <$> traverse
                 (withCat $ startUpdates <<< Lettera.getFrontpageHtml mosaicoPaper) prerenderedCategories
  mostRead <- (map <<< map <<< map) (join <<< fromFoldable) $
              -- Ditch reset handle for mostRead
              map (\(Controls x) -> x.content) $
              -- const to ditch cache stamp from the Lettera call
              startUpdates $ const $ Lettera.getMostRead 0 10 Nothing mosaicoPaper false

  latest <- (map <<< map <<< map) (join <<< fromFoldable) $
              map (\(Controls x) -> x.content) $
              startUpdates $ const $ Lettera.getLatest 0 10 mosaicoPaper

  advertorials <-
    (map <<< map <<< map) (join <<< fromFoldable) $
    map (\(Controls x) -> x.content) $
    startUpdates $ const $ Lettera.getAdvertorials mosaicoPaper

  pure $ cache
    { prerendered = prerendered
    , mainCategoryFeed = mainCategoryFeed
    , mostRead = mostRead
    , latest = latest
    , advertorials = advertorials
    , mode = Server
    }

getUsingCache
  :: forall k m. Ord k
  => Monoid m => AVar (Map k (Stamped m))
  -> k
  -> Aff (LetteraResponse m)
  -> Aff (Stamped m)
getUsingCache var key action = do
  store <- AVar.read var
  let fetch = do
        LetteraResponse response <- action
        now <- liftEffect nowDateTime
        pure $ case (\content validUntil -> Stamped { content, age: now, validUntil })
                    <$> hush response.body
                    <*> toValidUntil now response.maxAge of
          Just value ->
            { value, newStore: Just $ Map.insert key value }
          _ ->
            { value: Stamped { validUntil: now, age: now, content: mempty }, newStore: Just $ Map.delete key }
  { value, newStore } <-
    case Map.lookup key store of
      Just value@(Stamped { validUntil }) -> do
        now <- liftEffect nowDateTime
        if now <= validUntil
        then pure { value, newStore: Nothing }
        else fetch
      Nothing -> fetch
  -- TODO: This is good enough locking to allow concurrent requests to
  -- different resources, but better yet would be to let at most one
  -- concurrent fetch for each.
  foldMap (\f -> do
              oldStore <- AVar.take var
              AVar.put (f oldStore) var) newStore
  pure value

getListUsingCache
  :: forall k a. Ord k
  => AVar (Map k (Stamped (Array a)))
  -> k
  -> Maybe Int
  -> (Int -> Int -> Aff (LetteraResponse (Array a)))
  -> Aff (Stamped (Array a))
getListUsingCache var key limit action = do
  store <- AVar.read var
  now <- liftEffect nowDateTime
  let oldContent = do
        c@(Stamped {validUntil}) <- Map.lookup key store
        guard $ validUntil >= now
        pure c
  let start = maybe 0 (\(Stamped {content}) -> length content) oldContent
      toFetch = fromMaybe 20 limit - start
  if toFetch <= 0 then pure $ fromMaybe eotStamp oldContent else do
    LetteraResponse response <- action start toFetch
    let newContent = case (\content validUntil -> Stamped { content, age: now, validUntil })
                          <$> hush response.body
                          -- Don't discard new data if it's missing max-age
                          <*> pure (fromMaybe now (toValidUntil now response.maxAge)) of
                       Just value -> Just $ (\old new -> old <> new)
                                     <$> fromMaybe eotStamp oldContent
                                     <*> value
                       -- TODO error message
                       _ -> oldContent
    oldStore <- AVar.take var
    let newStore = case newContent of
                        Just value -> Map.insert key value oldStore
                        _ -> Map.delete key oldStore
    AVar.put newStore var
    pure $ fromMaybe emptyStamp newContent

search :: Cache -> String -> Maybe Int -> Aff (Stamped (Array ArticleStub))
search cache query limit = getListUsingCache cache.feedList (SearchFeed query) limit $ \s l -> Lettera.search s l mosaicoPaper query

-- Only main category prerendered contents are actively cached, others
-- will be fetched on demand.
getFrontpageHtml :: Cache -> CategoryLabel -> Aff (Stamped (Maybe String))
getFrontpageHtml cache category = do
  maybe useFeed (\(Controls c) -> c.content) $ Map.lookup category cache.prerendered
  where
    useFeed =
      (map <<< map) (\h -> if null h then Nothing else Just h) $
      getUsingCache cache.feedString (CategoryFeed Prerendered category) $
      Lettera.getFrontpageHtml mosaicoPaper (show category) Nothing

getBreakingNewsHtml :: Cache -> Aff (Stamped String)
getBreakingNewsHtml cache =
  getUsingCache cache.feedString BreakingNewsFeed $
    Lettera.getBreakingNewsHtml mosaicoPaper Nothing

getFrontpage :: Cache -> Maybe Int -> CategoryLabel -> Aff (Stamped (Array ArticleStub))
getFrontpage cache count category = do
  case Map.lookup category cache.mainCategoryFeed of
    Just c -> do
      Stamped {content, age, validUntil} <- (\(Controls x) -> x.content) c
      maybe
        useFeed
        (\newContent -> pure $ Stamped { content: newContent, age, validUntil } )
        (case count of
          Nothing -> Just content
          Just n -> if length content >= n then Just (take n content) else Nothing)
    Nothing -> useFeed

  where
    useFeed =
      getListUsingCache cache.feedList (CategoryFeed Feed category) count $
        \s l -> Lettera.getFrontpage mosaicoPaper (Just s) (Just l) (Just $ show category) Nothing

getMostRead :: Cache -> Aff (Stamped (Array ArticleStub))
getMostRead cache@{ mode: Client } =
  getUsingCache cache.feedList MostReadFeed $
  Lettera.getMostRead 0 10 Nothing mosaicoPaper false
getMostRead cache = cache.mostRead

getLatest :: Cache -> Aff (Stamped (Array ArticleStub))
getLatest cache@{ mode: Client } =
  getUsingCache cache.feedList LatestFeed $
  Lettera.getLatest 0 10 mosaicoPaper
getLatest cache = cache.latest

getAdvertorials :: Cache -> Aff (Stamped (Array ArticleStub))
getAdvertorials { mode: Client } = pure eotStamp
getAdvertorials cache = cache.advertorials

getByTag :: Cache -> Tag -> Maybe Int -> Aff (Stamped (Array ArticleStub))
getByTag cache tag count =
  getListUsingCache cache.feedList (TagFeed tag) count $
    \s l -> Lettera.getByTag s l tag mosaicoPaper

resetCategory :: Cache -> CategoryLabel -> Aff Unit
resetCategory cache category = do
  removeFromActive cache.prerendered
  removeFromActive cache.mainCategoryFeed
  removeFromPassive Prerendered cache.feedString
  removeFromPassive Feed cache.feedList
  where
    removeFromActive :: forall a b. Map CategoryLabel (Controls a b) -> Aff Unit
    removeFromActive = Map.lookup category >>> maybe (pure unit) (\(Controls c) -> c.reset)
    removeFromPassive :: forall a. CategoryType -> (AVar (Map ArticleFeedType a)) -> Aff Unit
    removeFromPassive feedType c =
      flip AVar.put c =<< Map.delete (CategoryFeed feedType category) <$> AVar.take c

saveCategoryRender :: Cache -> CategoryLabel -> Stamped String -> Aff Unit
saveCategoryRender cache category content = do
  let store = cache.storedCategoryRender
  flip AVar.put store =<< Map.insert category content <$> AVar.take store

readCategoryRender :: Cache -> CategoryLabel -> Aff (Maybe (Stamped String))
readCategoryRender cache category = do
  let store = cache.storedCategoryRender
  cached <- Map.lookup category <$> AVar.read store
  case cached of
    Nothing -> pure Nothing
    Just value@(Stamped {validUntil}) -> do
      now <- liftEffect nowDateTime
      if now > validUntil
        then do
        flip AVar.put store =<< Map.delete category <$> AVar.take store
        pure Nothing
        else pure $ Just value

saveCorsResult :: Cache -> String -> Stamped String -> Aff (Stamped String)
saveCorsResult cache url content = do
  let store = cache.corsProxyCache
  flip AVar.put store =<< Map.insert url content <$> AVar.take store
  pure content

readCorsResult :: Cache -> String -> Aff (Either String (Stamped String))
readCorsResult cache url = do
  let store = cache.corsProxyCache
  cached <- Map.lookup url <$> AVar.read store
  now <- liftEffect nowDateTime
  let maxAge = fromMaybe now $ adjust (Seconds $ toNumber 300) now
  case cached of
    Nothing -> do
      result <- Cors.fetchUrl url
      case result of
        Left err -> pure $ Left err
        Right content -> do
          stamped <- saveCorsResult cache url $ Stamped { validUntil: maxAge, age: now, content }
          pure $ Right stamped
    Just value@(Stamped {validUntil}) -> do
      if now > validUntil
      then do
        flip AVar.put store =<< Map.delete url <$> AVar.take store
        result <- Cors.fetchUrl url
        case result of
          Left err -> pure $ Left err
          Right content -> do
            stamped <- saveCorsResult cache url $ Stamped { validUntil: maxAge, age: now, content }
            pure $ Right stamped
      else pure $ Right value

addHeader :: forall a b. DateTime -> Stamped b -> Response a -> Response a
addHeader now (Stamped { validUntil }) =
  if maxAge <= 0 then identity
  else addHeaderAge maxAge
  where
    maxAge = floor $ un Seconds $ diff validUntil now

addHeaderAge :: forall a. Int -> Response a -> Response a
addHeaderAge maxAge (Response response) =
  Response $ response { headers = Headers.set "cache-control" control response.headers }
  where
    control = "max-age=" <> show maxAge

type CommonLists a =
  { pageContent :: a
  , breakingNews :: Stamped String
  , mostReadArticles :: Stamped (Array ArticleStub)
  , latestArticles :: Stamped (Array ArticleStub)
  }

parallelWithCommonLists :: forall a. Cache -> Aff a -> Aff (CommonLists a)
parallelWithCommonLists cache f =
  sequential $ { pageContent: _, breakingNews: _, mostReadArticles: _, latestArticles: _ }
  <$> parallel f
  <*> parallel (getBreakingNewsHtml cache)
  <*> parallel (getMostRead cache)
  <*> parallel (getLatest cache)

setFeeds :: Cache -> (Feeds -> Feeds) -> Aff Unit
setFeeds cache feeds = do
  setter <- AVar.take cache.setFeeds
  liftEffect $ setter feeds
  AVar.put setter cache.setFeeds

setCommonFeeds :: forall a. CommonLists a -> Feeds -> Feeds
setCommonFeeds { breakingNews, mostReadArticles, latestArticles } s =
  s { mostReadArticles = getContent mostReadArticles
    , latestArticles = getContent latestArticles
    , breakingNews = getContent breakingNews
    , loading = s.loading - 1
    }

-- Client side feed fetching.
requestFeed :: Cache -> RouteFeed -> Aff Unit
requestFeed cache {limit, feedType: (TagFeed t)} = do
  setFeeds cache \s -> s { loading = s.loading + 1 }
  cl <- parallelWithCommonLists cache $ getContent <$> getByTag cache t limit
  setFeeds cache $ setCommonFeeds cl <<<
    \s -> s { tag = Map.insert t cl.pageContent s.tag }
requestFeed cache {feedType: (CategoryFeed Prerendered c)} = do
  setFeeds cache \s -> s { loading = s.loading + 1 }
  cl@{pageContent: {list, html}} <-
    parallelWithCommonLists cache $ sequential $
    {list: _, html: _}
    <$> parallel (getContent <$> getFrontpage cache Nothing c)
    <*> parallel (getContent <$> getFrontpageHtml cache c)
  let prerendered = Html list <$> html
  setFeeds cache $ setCommonFeeds cl <<<
    \s -> s { category = Map.insert c (fromMaybe (ArticleList list) prerendered) s.category }
requestFeed cache {limit, feedType: (CategoryFeed Feed c)} = do
  setFeeds cache \s -> s { loading = s.loading + 1 }
  cl <- parallelWithCommonLists cache $ getContent <$> getFrontpage cache limit c
  setFeeds cache $ setCommonFeeds cl <<<
    \s -> s { category = Map.insert c (ArticleList cl.pageContent) s.category }
requestFeed _ {feedType: (CategoryFeed _ _)} = pure unit
requestFeed cache {limit, feedType: (SearchFeed query)} = do
  setFeeds cache \s -> s { loading = s.loading + 1 }
  cl <- parallelWithCommonLists cache $ getContent <$> search cache query limit
  setFeeds cache $ setCommonFeeds cl <<< \s -> s { search = Map.insert query cl.pageContent s.search }
requestFeed cache {feedType: (DebugFeed uuid)} = do
  setFeeds cache \s -> s { loading = s.loading + 1 }
  cl <- parallelWithCommonLists cache $ Lettera.getArticleStub uuid
  case cl.pageContent of
    Right article ->
      setFeeds cache $ setCommonFeeds cl <<< \s -> s { category = Map.insert (CategoryLabel "debug") (ArticleList [article]) s.category }
    Left _ -> pure unit
requestFeed _ _ = pure unit

-- Client side.  Doesn't trigger immediate loads but marks the
-- resource stale so that it will be reloaded.
invalidateFeed :: Cache -> ArticleFeedType -> Aff Unit
invalidateFeed cache feed =
  let invalidate :: forall a. AVar (Map ArticleFeedType (Stamped a)) -> Aff Unit
      invalidate feedVar = do
        store <- AVar.take feedVar
        flip AVar.put feedVar $
          Map.update (\v -> Just $ (emptyStamp :: Stamped (Array Unit)) *> v) feed store
  in case feed of
    (CategoryFeed Prerendered _) -> do
      invalidate cache.feedList
      invalidate cache.feedString
    BreakingNewsFeed -> invalidate cache.feedString
    _ -> invalidate cache.feedList

-- Server side feed fetching.  Limit param is unused in this context
-- and every feed just uses the default length.
loadFeeds :: Cache -> Maybe ArticleFeedType -> Aff (Stamped (Array (Tuple ArticleFeedType ArticleFeed)))
loadFeeds cache feedType = do
  { breakingNews, mostReadArticles, latestArticles, pageContent } <-
    parallelWithCommonLists cache $ case feedType of
      Just (TagFeed tag) ->
        Just <<< map ArticleList <$> getByTag cache tag Nothing
      Just (CategoryFeed Feed c) ->
        Just <<< map ArticleList <$> getFrontpage cache Nothing c
      Just (CategoryFeed Prerendered c) -> do
        {list, html} <- {list: _, html: _}
          <$> getFrontpage cache Nothing c
          <*> getFrontpageHtml cache c
        pure $ Just $ maybe (ArticleList <$> list) (Html <$> list <*> _) $ sequence html
      Just (SearchFeed q) ->
        Just <<< map ArticleList <$> search cache q Nothing
      -- Used for testing only
      Just (DebugFeed uuid) -> do
        map ((_ <$ (eotStamp :: Stamped (Array Unit))) <<< ArticleList <<< pure) <<< hush
        <$> Lettera.getArticleStub uuid
      _ -> pure Nothing
  advertorials <- getAdvertorials cache

  pure $ foldr mergeStamps eotStamp $
    [ Tuple BreakingNewsFeed <<< Html [] <$> breakingNews
    , Tuple MostReadFeed <<< ArticleList <$> mostReadArticles
    , Tuple LatestFeed <<< ArticleList <$> latestArticles
    , Tuple AdvertorialsFeed <<< ArticleList <$> advertorials
    ] <> maybe [] (pure <<< sequenceStamp) (Tuple <$> feedType <*> pageContent)
  where
    mergeStamps a@(Stamped {content}) b = cons content <$> (a *> b)
    sequenceStamp (Tuple t s@(Stamped {content})) = s $> Tuple t content
