module Mosaico.Test.Redir where

import Prelude

import Data.Either (Either(..))
import Data.String.Regex as Regex
import Effect.Aff (Aff)
import Mosaico.Test (site)
import Test.Unit (failure)
import Test.Unit.Assert as Assert
import Toppokki (getLocationRef)
import KSF.Puppeteer as Chrome

testRedir :: Chrome.Page -> Aff Unit
testRedir page = do
  let eitherRegex = Regex.regex "privat-hbl.cargoselfservice" mempty
  Chrome.goto (Chrome.URL $ site <> "annonskiosken") page
  loc <- getLocationRef page
  case eitherRegex of
    Left _ -> failure "Internal error: failed to construct regex"
    Right regex ->
      Assert.assert "annonskiosken URL redirects to cargoselfservice URL" $ Regex.test regex loc
