module Mosaico.Tag where

import Data.Newtype (un)
import Lettera.Models (Tag(..), tagToURIComponent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

import Prelude

renderTag :: (Tag -> EventHandler) -> Tag -> JSX
renderTag onClick tag =
    DOM.a
      { className: "inline-block relative z-20 p-0 mb-1 h-4 text-xs font-bold no-underline uppercase bg-transparent font-roboto text-brand mosaico-article__tag"
      , children: [ DOM.text $ (un Tag) tag ]
      , href: "/tagg/" <> tagToURIComponent tag
      , onClick: onClick tag
      }