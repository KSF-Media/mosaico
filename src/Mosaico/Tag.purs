module Mosaico.Tag where

import Prelude

import Data.Newtype (un)
import Lettera.Models (Tag(..), tagToURIComponent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

data TagType = Main | Listing

renderTag :: TagType -> (Tag -> EventHandler) -> Tag -> JSX
renderTag tagType onClick tag =
  DOM.a
  { className: case tagType of
       Main -> "inline-block relative z-20 p-0 mb-1 h-4 text-xs font-bold no-underline uppercase bg-transparent font-roboto text-brand mosaico-article__tag"
       Listing -> "font-roboto text-gray-400 border border-solid rounded-full border-gray-400 m-1 px-2 py-1 inline-block"
  , children: [ DOM.text $ (un Tag) tag ]
  , href: "/tagg/" <> tagToURIComponent tag
  , onClick: onClick tag
  }
