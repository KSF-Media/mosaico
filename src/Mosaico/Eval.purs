module Mosaico.Eval where

import Prelude

import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit
foreign import consentedToEmbeddedScripts :: Promise Boolean

evalExternalScripts :: Array ScriptTag -> Effect Unit
evalExternalScripts scriptTags = runEffectFn1 evalExternalScriptsImpl $ map (\(ScriptTag scriptTag) -> scriptTag) scriptTags

externalScriptsAllowed :: Aff Boolean
externalScriptsAllowed = toAff consentedToEmbeddedScripts

-- This is supposed to be a complete script tag
-- <script></script>
newtype ScriptTag = ScriptTag String
