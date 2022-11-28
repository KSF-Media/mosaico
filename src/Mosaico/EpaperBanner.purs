module Mosaico.EpaperBanner where

import React.Basic (JSX)
import React.Basic.DOM as DOM


render :: JSX
render =
  DOM.section
    { className: "mb-3 bg-gray-50"
    , children:
      [ DOM.header
          { className: "py-2 text-center"
          , children:
            [ DOM.h2
                { className: "flex justify-center items-center text-xl font-bold before:w-3 before:h-3 before:mr-1 before:inline-block before:bg-neutral"
                , children: [ DOM.text "E-tidningen" ]
                }
            ]
          }
      , DOM.a
        { href: "/epaper"
        , className: "block"
        , children:
          [ DOM.span
              { className: "block overflow-hidden w-72 max-w-full h-72 bg-top bg-no-repeat bg-contain mosaico-epaper--bg-cover"
              , children: [ DOM.span
                              { className: "sr-only"
                              , children: [DOM.text "E-tidningen"]
                              } ]
           } ]
        }
      ]
    }
