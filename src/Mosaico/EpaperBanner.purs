module Mosaico.EpaperBanner where

import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

type Props = { onMainClick :: String -> EventHandler}

render :: Props -> JSX
render props =
  DOM.section
    { className: "pb-3 mb-3 md:mx-2 bg-aptoma-epaper-background"
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
        , onClick : props.onMainClick "/epaper"
        , className: "block"
        , children:
          [ DOM.span
              { className: "block overflow-hidden mx-auto w-72 max-w-full h-72 bg-top bg-no-repeat bg-contain mosaico-epaper--bg-cover"
              , children: [ DOM.span
                              { className: "sr-only"
                              , children: [DOM.text "E-tidningen"]
                              } ]
           } ]
        }
      ]
    }
