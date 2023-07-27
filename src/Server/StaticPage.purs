module Mosaico.Server.StaticPage where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import KSF.Paper as Paper
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, stat) as FS
import Node.FS.Stats (isFile) as FS
import Mosaico.Paper (mosaicoPaper)
import Mosaico.StaticPage (StaticPageResponse(..), isValidPageName)

loadStaticPage :: String -> Effect (Maybe StaticPageResponse)
loadStaticPage pageName | isValidPageName pageName = do
  let filePath = "./dist/static/" <> (String.toLower $ Paper.toString mosaicoPaper) <> "/" <>
                 pageName <> ".html"
      scriptPath = "./dist/static/" <> pageName <> ".js"
  fileOK <- FS.isFile <$> FS.stat filePath
  if fileOK then do
    pageContent <- FS.readTextFile UTF8 filePath
    scriptOK <- FS.isFile <$> FS.stat scriptPath
    pageScript <- if scriptOK then Just <$> FS.readTextFile UTF8 scriptPath else pure Nothing
    pure $ Just $ StaticPageResponse { pageName, pageContent, pageScript }
    else pure Nothing
loadStaticPage _ = pure Nothing
