module Mosaico.Cache.Purge where

import Prelude

import Affjax as AX
import Affjax.Node (driver)
import Affjax.ResponseFormat as ResponseFormat
import Data.HTTP.Method as Method
import Effect.Aff (Aff)
import KSF.Paper (Paper(..), homepage)
import Lettera.Models (CategoryLabel, frontpageCategoryLabel)
import Mosaico.Paper (mosaicoPaper)

foreign import isProd :: Boolean

domain :: String
domain = if isProd then homepage mosaicoPaper else case mosaicoPaper of
  HBL -> "https://mosaico-hbl.staging.ksfmedia.fi/"
  ON  -> "https://mosaico-on.staging.ksfmedia.fi/"
  VN  -> "https://mosaico-vn.staging.ksfmedia.fi/"
  _   -> ""

purge :: String -> Aff Unit
purge url = do
  let request = AX.defaultRequest
        { url = url
        , method = Method.fromString "PURGE"
        , responseFormat = ResponseFormat.ignore
        }
  void $ AX.request driver request

purgeCategory :: CategoryLabel -> Aff Unit
purgeCategory c | c == frontpageCategoryLabel = do
  purge domain
purgeCategory category = do
  purge $ domain <> show category
