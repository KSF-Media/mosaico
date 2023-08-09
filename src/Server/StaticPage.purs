module Mosaico.Server.StaticPage where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import KSF.Paper as Paper
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, stat, exists) as FS
import Node.FS.Stats (isFile) as FS
import Node.Path (FilePath)
import Mosaico.Paper (mosaicoPaper)
import Mosaico.StaticPage (StaticPageResponse(..), isValidPageName)

loadStaticPage :: String -> Effect (Maybe StaticPageResponse)
loadStaticPage pageName | isValidPageName pageName = do
  let filePath = "./dist/static/" <> (String.toLower $ Paper.toString mosaicoPaper) <> "/" <>
                 pageName <> ".html"
      scriptPath = "./dist/static/" <> pageName <> ".js"
  fileOK <- existsAndIsFile filePath
  if fileOK then do
    pageContent <- FS.readTextFile UTF8 filePath
    scriptOK <- existsAndIsFile scriptPath
    pageScript <- if scriptOK then Just <$> FS.readTextFile UTF8 scriptPath else pure Nothing
    pure $ Just $ StaticPageResponse { pageName, pageContent, pageScript }
    else pure Nothing
  where
    existsAndIsFile :: FilePath -> Effect Boolean
    existsAndIsFile path = do
      exists <- FS.exists path
      if exists then FS.isFile <$> FS.stat path else pure false
loadStaticPage _ = pure Nothing
