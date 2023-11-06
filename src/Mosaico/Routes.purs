module Mosaico.Routes where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Data.Date (Date, Month, Year, canonicalDate)
import Data.Either (Either(..), hush)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Foldable (oneOf)
import Data.Int as Int
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Semiring.Free (free)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.Regex (Regex, regex, test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Data.UUID as UUID
import Lettera.Models (Categories, Category, CategoryLabel(..), DraftParams, Tag, uriComponentToTag)
import Routing as Routing
import Routing.Match (Match(..), eitherMatch, end, int, lit, optionalMatch, param, params, root, str)
import Routing.Match.Error (MatchError(..))
import Routing.PushState (PushStateInterface)
import Routing.Types (RoutePart(..))
import Simple.JSON (write)
import Web.HTML (window)
import Web.HTML.Window (scrollY)

data ArchivePageView
  = MonthSelection
  | DateSelection Year Month
  | ArticleSelection Date
derive instance eqArchivePageView :: Eq ArchivePageView

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage (Either Unit (Tuple Int DraftParams))
  | EpaperPage
  | ProfilePage
  | ArticlePage UUID.UUID (Maybe String)
  | NotFoundPage String
  | KorsordPage
  -- TODO Use a newtype wrapper that prohibits slashes in it?
  | StaticPage String
  | CategoryPage Category (Maybe Int)
  | TagPage Tag (Maybe Int)
  | SearchPage (Maybe String) (Maybe Int)
  | DebugPage UUID.UUID -- Used for testing
  | DeployPreview -- Used for deploy previews only
  | MenuPage
  | ArchivePage ArchivePageView
derive instance eqMosaicoPage :: Eq MosaicoPage

-- The URL given from Mosaico module shouldn't have the fragment
-- already, but in case it happens anyway, use this function
stripFragment :: String -> String
stripFragment = String.takeWhile (_ /= codePointFromChar '#')

-- | A regular expression that accepts a 40 character hexadecimal number
--   (without the `0x` prefix), such as an SHA1 checksum.
hashRegex :: Maybe Regex.Regex
hashRegex = hush $ Regex.regex "^[a-fA-F0-9]{40}$" Regex.noFlags

routes :: Categories -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage (Left unit) <$ (lit "artikel" *> lit "draft" *> lit "test")
  , DraftPage <<< Right <$> (lit "artikel" *> lit "draft" *>
                             (Tuple
                              <$> int
                              <*> ({time:_, publication:_, user:_, hash:_}
                                   <$> param "dp-time"
                                   <*> param "publicationId"
                                   <*> param "user"
                                   <*> param "hash")
                            ) <* optionalMatch params)
  , ArticlePage <$> (lit "artikel" *> uuid) <*> (sanitizeHash <$> optionalMatch (param "freePassHash"))
  , KorsordPage <$ lit "korsord"
  , StaticPage <$> (lit "sida" *> str)
  , EpaperPage <$ lit "epaper"
  , ProfilePage <$ lit "konto"
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str) <*> ((Int.fromString =<< _) <$> optionalMatch (param "c"))
  , MenuPage <$ lit "meny"
  , SearchPage <$> (lit "sök" *> optionalMatch (param "q")) <*> ((Int.fromString =<< _) <$> optionalMatch (param "c"))
  , DebugPage <$> (lit "debug" *> uuid)
  , DeployPreview <$ str <* lit "mosaico" <* lit "index.html"
  , CategoryPage <$> categoryRoute <*> ((Int.fromString =<< _) <$> optionalMatch (param "c"))
  , Frontpage <$ optionalMatch params <* end
  -- Since Routing has no way to match with the rest of the URL (all
  -- path components included) leave NotFoundPage to caller and the
  -- Left response.
  , CategoryPage <$> categoryRoute <*> ((Int.fromString =<< _) <$> optionalMatch (param "c")) <* end
  , ArchivePage <$> (ArticleSelection <$> (canonicalDate <$> (lit "arkiv" *> enum) <*> enum <*> enum))
  , ArchivePage <$> (DateSelection <$> (lit "arkiv" *> enum) <*> enum)
  , ArchivePage MonthSelection <$ lit "arkiv" <* end
  , NotFoundPage <$> str
  ] <* optionalMatch params <* end
  where
    sanitizeHash :: Maybe String -> Maybe String
    sanitizeHash s = do
      t <- s
      regex <- hashRegex
      guard (Regex.test regex t) s
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Just category <- Map.lookup (CategoryLabel categoryRouteName) categories
            = pure $ Tuple rs category
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute

enum :: forall a. BoundedEnum a => Match a
enum = eitherMatch (maybe (Left "Invalid enum") Right <<< toEnum <$> int)

match :: Categories -> String -> MosaicoPage
match catMap path = case Routing.match (routes catMap) path of
  Right x -> x
  Left _ -> NotFoundPage path

type RouteState = { yPositionOnLeave :: Maybe Number }

-- TODO change route param to MosaicoPage.  It'd be an error to link
-- to a page that'd resolve to NotFoundPage.
changeRoute :: PushStateInterface -> String -> Effect Unit
changeRoute router route = do
  currentRoute <- (\l -> l.pathname <> l.search) <$> router.locationState
  currentY     <- scrollY =<< window
  -- Before changing the route, let's write the y scroll position to the state of the current
  -- location, as this is needed for recovery if users go back in browser history
  runRouteEffect router.replaceState { yPositionOnLeave: Just currentY } currentRoute
  runRouteEffect router.pushState { yPositionOnLeave: Nothing } route

runRouteEffect :: (Foreign -> String -> Effect Unit) -> RouteState -> String -> Effect Unit
runRouteEffect f state route = f (write state) route

uuid :: Match UUID.UUID
uuid = Match \route ->
  case route of
    Cons (Path input) rs | Just u <- UUID.parseUUID input ->
      pure $ Tuple rs u
    _ ->
      invalid $ free $ Fail "Expected UUID"
