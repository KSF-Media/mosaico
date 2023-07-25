module Mosaico.Cache.Pubsub where

-- This module is server only.

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Data.DateTime (DateTime)
import Data.JSDate (JSDate, toDateTime)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Lettera.Models (CategoryLabel(..))
import Lettera (LetteraResponse(..))
import Mosaico.Cache as Cache
import Mosaico.Cache.Purge as Purge
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(..))
import Promise.Aff (Promise, toAffE)

foreign import data Message :: Type

foreign import subscribeImpl :: (Message -> Effect Unit) -> Effect (Promise Unit)
foreign import enabled :: Boolean
foreign import categoryImpl :: Message -> Nullable String
foreign import maxAge :: Message -> Int
foreign import content :: Message -> Buffer
foreign import stampImpl :: Message -> Nullable JSDate

subscribe :: (Message -> Effect Unit) -> Aff Unit
subscribe = subscribeImpl >>> toAffE

messageCategory :: Message -> Maybe CategoryLabel
messageCategory = categoryImpl >>> toMaybe >>> map CategoryLabel

stamp :: Message -> Maybe DateTime
stamp = stampImpl >>> toMaybe >=> toDateTime

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
        let maybeStamp = stamp msg
        case maybeStamp of
          Nothing -> pure unit
          Just s -> Aff.launchAff_ do
            updated <- c.set s =<< liftEffect (toLetteraResponse msg)
            when updated $
              maybe (pure unit) Purge.purgeCategory category
      _ -> pure unit
