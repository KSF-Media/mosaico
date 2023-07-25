module Mosaico.Routes where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semiring.Free (free)
import Data.Map as Map
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import Data.Validation.Semiring (invalid)
import Data.UUID as UUID
import Lettera.Models (Categories, Category, CategoryLabel(..), DraftParams, Tag, uriComponentToTag)
import Routing as Routing
import Routing.Match (Match(..), end, int, lit, optionalMatch, param, params, root, str)
import Routing.PushState (PushStateInterface)
import Routing.Match.Error (MatchError(..))
import Routing.Types (RoutePart(..))
import Web.HTML (window)
import Web.HTML.Window (scrollY)

import Simple.JSON (write)

data MosaicoPage
  = Frontpage -- Should take Paper as parameter
  | DraftPage (Either Unit (Tuple Int DraftParams))
  | EpaperPage
  | ProfilePage
  | ArticlePage UUID.UUID
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
derive instance eqMosaicoPage :: Eq MosaicoPage

-- The URL given from Mosaico module shouldn't have the fragment
-- already, but in case it happens anyway, use this function
stripFragment :: String -> String
stripFragment = String.takeWhile (_ /= codePointFromChar '#')

routes :: Categories -> Match MosaicoPage
routes categories = root *> oneOf
  [ DraftPage (Left unit) <$ (lit "artikel" *> lit "draft" *> lit "test" *> end)
  , DraftPage <<< Right <$> (lit "artikel" *> lit "draft" *>
                             (Tuple
                              <$> int
                              <*> ({time:_, publication:_, user:_, hash:_}
                                   <$> param "time"
                                   <*> param "publication"
                                   <*> param "user"
                                   <*> param "hash")
                            ))
  , ArticlePage <$> (lit "artikel" *> uuid)
  , KorsordPage <$ (lit "korsord" *> end)
  , StaticPage <$> (lit "sida" *> str)
  , EpaperPage <$ (lit "epaper" *> optionalMatch params *> end)
  , ProfilePage <$ (lit "konto" *> end)
  , TagPage <<< uriComponentToTag <$> (lit "tagg" *> str) <*> ((Int.fromString =<< _) <$> optionalMatch (param "c")) <* end
  , Frontpage <$ end
  , MenuPage <$ lit "meny"
  , SearchPage <$> (lit "sök" *> optionalMatch (param "q")) <*> ((Int.fromString =<< _) <$> optionalMatch (param "c")) <* end
  , DebugPage <$> (lit "debug" *> uuid)
  , DeployPreview <$ str <* lit "mosaico" <* lit "index.html" <* end
  , CategoryPage <$> categoryRoute <*> ((Int.fromString =<< _) <$> optionalMatch (param "c")) <* end
  -- Since Routing has no way to match with the rest of the URL (all
  -- path components included) leave NotFoundPage to caller and the
  -- Left response.
  ]
  where
    categoryRoute =
      let matchRoute route
            | Cons (Path categoryRouteName) rs <- route
            , Just category <- Map.lookup (CategoryLabel categoryRouteName) categories
            = pure $ Tuple rs category
            | otherwise = invalid $ free $ Fail "Not a category"
      in Match matchRoute

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
