module Mosaico.Server.Env where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (cons, foldl)
import Data.Either (Either(..))
import Data.List as List
import Data.List (List, union, snoc, (:))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models (Category, Categories, Platform(Desktop), categoriesMap, correctionsCategory)
import Mosaico.Cache as Cache
import Mosaico.Cache.Pubsub as Pubsub
import Mosaico.Server.Template (TemplateMaster, parseTemplate)
import Mosaico.Paper (mosaicoPaper)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir, stat) as FS
import Node.FS.Stats (isFile) as FS
import Simple.JSON (readJSON)

indexHtmlFileLocation :: String
indexHtmlFileLocation = "./dist/index.html"

type Redirect =
  { paper :: Maybe Paper.Paper
  , route :: String
  , destination :: String
  }

type Env =
  { htmlTemplate :: TemplateMaster
  , categoryStructure :: Array Category
  , catMap :: Categories
  , categoryStructureJson :: Json
  , cache :: Cache.Cache
  , redirects :: Map (Tuple String Paper.Paper) String
  }

readDir :: String -> Effect (List String)
readDir dir = do
  let go :: List String -> List String -> Effect (List String)
      go acc List.Nil = pure acc
      go acc (c : tail) = do
        let fullFilePath = dir <> "/" <> c
        fileStats <- FS.stat fullFilePath
        if FS.isFile fileStats
        then go (acc `snoc` fullFilePath) tail
        else do
           contents <- readDir fullFilePath
           go (acc `union` contents) tail

  contents <- FS.readdir dir
  go mempty $ List.fromFoldable contents

newEnv :: Aff Env
newEnv = do
  htmlTemplate <- liftEffect $ parseTemplate <$> FS.readTextFile UTF8 indexHtmlFileLocation
  categoryStructure <- Lettera.getCategoryStructure (Just Desktop) mosaicoPaper
  let catMap = categoriesMap $ correctionsCategory `cons` categoryStructure
      categoryStructureJson = encodeJson categoryStructure
  cache <- liftEffect $ Cache.initServerCache $ correctionsCategory `cons` categoryStructure
  liftEffect $ Pubsub.startListen cache
  -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
  redirects <- liftEffect do
    -- The redirects are read from a JSON file for now,
    -- but it's easy to change this to read whatever
    -- bucket or source we want to have it in the future
    redirJson <- FS.readTextFile UTF8 "./dist/redir.json"
    case readJSON redirJson of
      Right (redirs :: Array Redirect) ->
        let mkRedir acc { route, paper, destination } =
              case paper of
                Just p -> Map.insert (route /\ p) destination acc
                -- If it's paper agnostic, let's create
                -- a redirect rule for all our three mosaico papers
                Nothing ->
                  Map.empty
                  # Map.insert (route /\ Paper.HBL) destination
                  # Map.insert (route /\ Paper.VN)  destination
                  # Map.insert (route /\ Paper.ON)  destination
                  # Map.union acc
        in pure $ foldl mkRedir Map.empty redirs
      Left _err -> throw "Could not parse redir.json! Check the file and try again"
  pure { htmlTemplate, categoryStructure, categoryStructureJson, catMap, cache, redirects }
