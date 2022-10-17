module Mosaico.Triggerbee where

import Prelude

import Effect.Uncurried (EffectFn1)

foreign import sendTriggerbeeEvent :: EffectFn1 String Unit

foreign import addToTriggerbeeObj :: EffectFn1 (User) Unit

type User = { isLoggedIn :: Boolean, isSubscriber :: Boolean }
