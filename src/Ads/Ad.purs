module Mosaico.Ad where

import Prelude

import Data.Nullable (Nullable)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Payload.Internal.Utils (toLowerCase)
import React.Basic (JSX)
import React.Basic.Classic (make)
import React.Basic.Classic as React
import React.Basic.DOM as DOM

foreign import fetchAdImpl :: EffectFn1 String Unit
foreign import getGamId :: EffectFn1 String (Nullable String)
foreign import showConsentRevocationMessage :: forall a. EffectFn1 a Unit

component :: React.Component Props
component = React.createComponent "ad"

type Self =
  { props :: Props }

type Props =
  { contentUnit :: String
  , inBody :: Boolean
  , hideAds :: Boolean
  }

-- TODO: convert to hooks
ad :: Props -> JSX
ad { hideAds: true } = mempty
ad props = make component
  { initialState: {}
  , didMount: \self -> do
      runEffectFn1 fetchAdImpl self.props.contentUnit
  , render: render
  } props
    where
      blockClass = "mosaico-ad"
      render self =
        let inBodyClass = if self.props.inBody then "in-body" else mempty
            contentUnitClass = toLowerCase self.props.contentUnit
        in DOM.aside
          { className: joinWith " " [blockClass, contentUnitClass]
          , children:
            [ DOM.div
                { id: self.props.contentUnit
                , className: joinWith " " [blockClass <> "__content-unit", inBodyClass]
                }
            ]
          }

openConsentRevocationMessage :: forall a. a -> Effect Unit
openConsentRevocationMessage = runEffectFn1 showConsentRevocationMessage
