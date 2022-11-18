module Mosaico.MainContent where

import Prelude
import React.Basic (JSX)
import React.Basic.DOM as DOM

mainContent :: String -> Boolean -> Array JSX -> JSX
mainContent className fullWidth children =
    DOM.div
      { id: "mosaico-main-content"
      , className: (if fullWidth then "[grid-area:full-width] " else "[grid-area:thin] ") <> className
      , children
      }

jumpToMainContent :: JSX
jumpToMainContent =
    DOM.a
      { className: "flex overflow-hidden absolute top-0 justify-center justify-self-center items-center w-full h-0 text-xl font-bold text-white focus:p-1 focus:h-auto bg-brand z-[1000]"
      , children: [ DOM.text "Hoppa till huvudinnehåll" ]
      , href: "#mosaico-main-content"
      }
