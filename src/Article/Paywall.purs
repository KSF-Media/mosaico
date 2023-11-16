module Mosaico.Article.Paywall where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Bottega.Models.PaymentMethod (PaymentMethod(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import Mosaico.Client.Handlers (Handlers)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import Vetrina.Types (Product)

paperHeadline :: Paper -> JSX
paperHeadline HBL =
  DOM.div_
    [ DOM.text $ "Läs HBL Digital gratis ända in i 2024"]
paperHeadline p =
  DOM.div_
    [ DOM.text $ "Prova " <> Paper.paperName p <> " Digital för "
    , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "endast 1 €" ] }
    , DOM.text $ " i en månad"
    ]

products :: Paper -> Array Product
products HBL = [ hblPremium ]
products VN = [ vnPremium ]
products ON = [ onPremium ]
products _ = []

-- For back end render with no user data available
staticPaywall :: Props -> JSX
staticPaywall { paper } =
  Vetrina.staticRender (Just paper)
  (products paper)
  (Just $ paperHeadline paper)

type Props =
  { handlers :: Handlers
  , user :: Maybe User
  , paper :: Paper
  }

paywall :: (Vetrina.Props -> JSX) -> Props -> JSX
paywall vetrina props = vetrina
  { onClose: Just props.handlers.onPaywallEvent
  , onLogin: props.handlers.onLogin
  , user: props.user
  , setUser: props.handlers.setUser <<< Just
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
