module Mosaico.Cache.Pubsub where

-- This module is server only.

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Lettera.Models (CategoryLabel(..))
import Lettera (LetteraResponse(..))
import Mosaico.Cache as Cache
import Mosaico.Cache.Purge as Purge
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(..))

foreign import data Message :: Type

foreign import subscribeImpl :: (Message -> Effect Unit) -> Effect (Promise Unit)
foreign import enabled :: Boolean
foreign import categoryImpl :: Message -> Nullable String
foreign import maxAge :: Message -> Int
foreign import content :: Message -> Buffer

subscribe :: (Message -> Effect Unit) -> Aff Unit
subscribe = subscribeImpl >>> toAffE

messageCategory :: Message -> Maybe CategoryLabel
messageCategory = categoryImpl >>> toMaybe >>> map CategoryLabel

toLetteraResponse :: Message -> Effect (LetteraResponse String)
toLetteraResponse msg = do
  body <- pure <$> toString UTF8 (content msg)
  pure $ LetteraResponse
    { maxAge: pure (maxAge msg)
    , body
    }

startListen :: Cache.Cache -> Effect Unit
startListen cache = do
  when enabled $ Aff.launchAff_ $ subscribe $ \msg -> do
    let category = messageCategory msg
    case flip Map.lookup cache.prerendered =<< category of
      Just (Cache.Controls c) -> do
        c.set =<< toLetteraResponse msg
        maybe (pure unit) (Aff.launchAff_ <<< Purge.purgeCategory) category
      _ -> pure unit
