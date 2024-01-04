module Mosaico.Eval where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Mosaico.Component.Consent (openConsent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)

foreign import evalExternalScriptsImpl :: EffectFn1 (Array String) Unit

evalStaticPageScripts :: Array ScriptTag -> Effect Unit
evalStaticPageScripts scriptTags = runEffectFn1 evalExternalScriptsImpl $ map (\(ScriptTag scriptTag) -> scriptTag) scriptTags

evalArticleScripts :: Array ScriptTag -> Effect Unit
evalArticleScripts scriptTags = runEffectFn1 evalExternalScriptsImpl $ map (\(ScriptTag scriptTag) -> scriptTag) scriptTags

-- This is supposed to be a complete script tag
-- <script></script>
newtype ScriptTag = ScriptTag String

type Props =
  { isArticle :: Boolean
  , consent :: Maybe Boolean
  }

render :: Props -> JSX
render { consent: Just false, isArticle } = DOM.div
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
          , onClick: capture_ $ void openConsent
          , children: [ DOM.text "här för att hantera dataskydd." ]
          , className: "italic underline decoration-2"
          }
      ]
  }
render _ = mempty
