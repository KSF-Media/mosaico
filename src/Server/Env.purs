module Mosaico.Server.Env where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Array (cons, foldl)
import Data.Either (Either(..), hush)
import Data.Foldable (foldM)
import Data.HashMap as HashMap
import Data.List as List
import Data.List (List, union, snoc, (:))
import Data.Map as Map
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex (regex, test) as Regex
import Data.String.Regex.Flags (ignoreCase, noFlags) as Regex
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models (ArticleStub, Category, articleStubToJson, correctionsCategory)
import Mosaico.Cache as Cache
import Mosaico.Cache (Stamped)
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
  , categoryRegex :: Regex
  , staticPages :: HashMap.HashMap String String
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
  staticPages  <- liftEffect do
    let pageMatch =
          maybe (const false) Regex.test $ hush $
          Regex.regex ("^\\./dist/static/(" <> (String.toLower $ Paper.toString mosaicoPaper) <> "/[^/]+\\.html|[^/]+\\.js)$")
          Regex.noFlags
    staticPageNames <- List.filter pageMatch <$> readDir "./dist/static"
    let makeMap acc staticPagePath = do
          case Array.last $ String.split (String.Pattern "/") staticPagePath of
            Just fileName -> do
              pageContent <- FS.readTextFile UTF8 staticPagePath
              pure $ HashMap.insert fileName pageContent acc
            _ -> pure acc
    foldM makeMap HashMap.empty staticPageNames
  htmlTemplate <- liftEffect $ parseTemplate <$> FS.readTextFile UTF8 indexHtmlFileLocation
  categoryStructure <- Lettera.getCategoryStructure mosaicoPaper
  cache <- liftEffect $ Cache.initServerCache $ correctionsCategory `cons` categoryStructure
  liftEffect $ Pubsub.startListen cache
  -- This is used for matching a category label from a route, such as "/nyheter" or "/norden-och-världen"
  categoryRegex <- case Regex.regex "^\\/([\\w|ä|ö|å|-]+)\\b" Regex.ignoreCase of
    Right r -> pure r
    Left _  -> liftEffect $ throw "I have a very safe regex to parse, yet somehow I didn't know how to parse it. Fix it please. Exploding now, goodbye."
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
  pure { htmlTemplate, categoryStructure, categoryRegex, staticPages, cache, redirects }

stdVars :: Env -> Maybe (Stamped String) -> Stamped (Array ArticleStub) -> Stamped (Array ArticleStub) -> Array (Tuple String Json)
stdVars env initialBreakingNews mostReadArticles latestArticles =
  [ "mostReadArticles"  /\ JSON.fromArray (map articleStubToJson (Cache.getContent mostReadArticles))
  , "latestArticles"    /\ JSON.fromArray (map articleStubToJson (Cache.getContent latestArticles))
  , "categoryStructure" /\ encodeJson env.categoryStructure
  , "breakingNews"      /\ maybe JSON.jsonNull JSON.fromString (Cache.getContent <$> initialBreakingNews)
  ]

