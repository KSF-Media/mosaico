module Mosaico.Client.Loader where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Lettera as Lettera
import Lettera.Models (articleStubToJson)
import Mosaico.Paper (mosaicoPaper)
import Promise.Aff (Promise, fromAff)

-- In full SPA mode, window variables are unset.  Some of them can be
-- loaded at run time with no trouble but some are presumed to be
-- always present in main Mosaico component's props.
loadMosaicoVars :: Effect (Promise Json)
loadMosaicoVars = fromAff do
  categoryStructure <- Lettera.getCategoryStructure Nothing mosaicoPaper
  advertorials <- map articleStubToJson <<< fromMaybe [] <$> Lettera.responseBody <$> Lettera.getAdvertorials mosaicoPaper
  -- fromJSProps will shortly decode them back but it's okay since
  -- this is just used for dev.
  pure $ encodeJson
    { categoryStructure
    , advertorials
    }
