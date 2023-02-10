module Mosaico.Timestamp where

import React.Basic (JSX)
import React.Basic.DOM as DOM

timestamp :: Array JSX -> JSX
timestamp children = DOM.span
    { className: "mr-1 text-sm font-light text-gray-500 dark:text-gray-300 font-roboto"
    , children: [DOM.span_  children]
    }
