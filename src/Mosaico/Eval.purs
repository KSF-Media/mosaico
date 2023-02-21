module Mosaico.Eval where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Mosaico.Ad (openConsentAndSetCookie)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit
foreign import consentedToEmbeddedScripts :: Promise Boolean

evalExternalScripts :: Array ScriptTag -> Effect Unit
evalExternalScripts scriptTags = runEffectFn1 evalExternalScriptsImpl $ map (\(ScriptTag scriptTag) -> scriptTag) scriptTags

externalScriptsAllowed :: Aff Boolean
externalScriptsAllowed = toAff consentedToEmbeddedScripts

-- This is supposed to be a complete script tag
-- <script></script>
newtype ScriptTag = ScriptTag String

type Props = { isArticle :: Boolean }

embedNagbar :: Component Props
embedNagbar =
  React.component "EmbedNagbar" $ \{ isArticle } -> React.do
    embedsAllowed /\ setEmbedsAllowed <- useState' Nothing
    useEffect unit do
      launchAff_ do
        permission <- externalScriptsAllowed
        liftEffect $ setEmbedsAllowed (Just permission)
      pure $ pure unit
    pure $ render embedsAllowed isArticle

render :: Maybe Boolean -> Boolean -> JSX
render (Just false) isArticle = DOM.div
  { className: "p-3 mx-auto mb-2 w-full max-w-[530px] font-bold text-white rounded-md border-2 bg-brand border-brand"
  , children:
      [ DOM.text $
          (if isArticle then "Den här artikeln" else "Den här sidan")
          <> " innehåller inbäddat innehåll som kanske inte visas korrekt om vi\
             \ inte har ditt samtycke att lagra information på din enhet. Du\
             \ kan ändra ditt val under rubriken Dataskydd längst ned på sidan,\
             \ eller genom att klicka "
      , DOM.a
          { href: "#"
          , onClick: handler preventDefault openConsentAndSetCookie
          , children: [ DOM.text "här för att hantera dataskydd." ]
          , className: "italic underline decoration-2"
          }
      ]
  }
render _ _ = mempty
