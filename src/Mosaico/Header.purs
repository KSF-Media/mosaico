module Mosaico.Header where

import React.Basic (JSX)
import React.Basic.DOM as DOM

-- The characteristic line at the top of every KSF media's site
topLine :: JSX
topLine = DOM.hr { className: "sticky top-0 z-10 w-full h-3 border-t-0 bg-brand [grid-area:line]" }

