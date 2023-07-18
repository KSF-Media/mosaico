module Mosaico.Article.Paywall where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Bottega.Models.PaymentMethod (PaymentMethod(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import Vetrina.Types (Product)

paperHeadline :: Paper -> JSX
paperHeadline HBL =
  DOM.div_
    [ DOM.text $ "HBL digitalt "
    , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "nu 0 €" ] }
    ]
paperHeadline p =
  DOM.div_
    [ DOM.text $ "Läs " <> Paper.paperName p <> " digitalt för "
    , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "endast 1 €" ] }
    ]

products :: Paper -> Array Product
products HBL = [ hblPremium ]
products VN = [ vnPremium ]
products ON = [ onPremium ]
products _ = []

-- For back end render with no user data available
staticPaywall :: Paper -> JSX
staticPaywall paper =
  Vetrina.staticRender (Just paper)
  (products paper)
  (Just $ paperHeadline paper)

type Props =
  { onPaywallEvent :: Effect Unit
  , onLogin :: EventHandler
  , setUser :: User -> Effect Unit
  , user :: Maybe User
  , paper :: Paper
  }

paywall :: (Vetrina.Props -> JSX) -> Props -> JSX
paywall vetrina props = vetrina
  { onClose: Just props.onPaywallEvent
  , onLogin: props.onLogin
  , user: props.user
  , setUser: props.setUser
  , products: products props.paper
  , unexpectedError: mempty
  , accessEntitlements: Set.fromFoldable case props.paper of
    HBL -> ["hbl-365", "hbl-web"]
    ON -> ["on-365", "on-web"]
    VN -> ["vn-365", "vn-web"]
    _ -> []
  , headline: Just $ paperHeadline props.paper
  , paper: Just props.paper
  , paymentMethods: [ CreditCard ]
  , customNewPurchase: Nothing
  , subscriptionExists: mempty
  , askAccountAlways: false
  , orderSource: PaywallSource
  }