module Mosaico.Component.Korsord where

import Prelude

import Data.Foldable (any)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import KSF.Api (UserAuth)
import KSF.Paper (Paper(..))
import KSF.Spinner (loadingSpinner)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffect)
import React.Basic.Hooks as React
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import loadKorsord :: Effect Unit

type Props =
  { entitlements :: Map String UserAuth
  , loadingEntitlements :: Boolean
  , paper :: Paper
  , paywall :: JSX
  , paywallCounter :: Int
  }

component :: Component Props
component = do
  React.component "Korsord" $ \props@{paywallCounter} -> React.do
    let entitlement =
          any (flip Map.member props.entitlements) ["hbl-web", "vn-web", "on-web"]
        isEntitled = case {entitlement, loading: props.loadingEntitlements} of
          {entitlement: true} -> Just true
          {loading: true} -> Nothing
          _ -> Just false
    useEffect isEntitled $ do
      when entitlement loadKorsord
      pure mempty
    pure $ render isEntitled props

render :: Maybe Boolean -> Props -> JSX
render entitlement { paper, paywall } =
  DOM.div
    { className: "flex flex-col justify-center"
    , children:
        [ korsordContent paper
        , case entitlement of
             Nothing ->
               DOM.div
                 { className: "flex justify-center"
                 , children: [ loadingSpinner ]
                 }
             Just false ->
               DOM.div
                 { className: "flex justify-center"
                 , children:
                     [ DOM.div
                         { className: "max-w-xl"
                         , children:
                             [ paywall ]
                         }
                     ]
                 }
             _ -> mempty
        ]
    }

korsordContent :: Paper -> JSX
korsordContent paper =
  DOM.div
    { className: "static-page__crosswords"
    , id: "keesing-crosswords"
    , children: [
      DOM.div
        { className: "flex flex-col mx-auto lg:w-240"
        , children:
          [ DOM.h1
              { className: "mt-5 mb-3 max-w-screen-md text-4xl font-duplexsans md:text-5xl"
              , children: [ DOM.text "Korsord och hjärngympa" ]
              }
          , DOM.p
              { className: "mb-3.5 pr-5"
              , children: [ DOM.text "Utmana dig själv med våra digitala pyssel i form av korsord, sudoku och andra logiska utmaningar." ]
              }
          , DOM.p
              { className: "mb-3.5 pr-5"
              , children:
                  [ DOM.text "Varje vecka publiceras fyra nya korsord att lösa, "
                  , DOM.strong_ [DOM.text "Tisdags-"]
                  , DOM.text " och "
                  , DOM.strong_ [DOM.text "Onsdagskrysset"]
                  , DOM.text ", "
                  , DOM.strong_ [DOM.text "Krypto"]
                  , DOM.text " samt "
                  , DOM.strong_ [DOM.text "Storkrysset"]
                  , DOM.text " på fredagar."
                  ]
              }
          , DOM.p
              { className: "mb-3.5 pr-5"
              , children:
                  [ DOM.text "Utöver korsorden hittar du varje dag nya underhållande pyssel och spel som bland annat "
                  , DOM.strong_ [DOM.text "Ordröj"]
                  , DOM.text " (Wordle) och "
                  , DOM.strong_ [DOM.text "Brobyggare"]
                  , DOM.text ". Den här sortens problemlösning är underhållande, skärper din tankeförmåga och fungerar som motion för hjärnan. Gör just din favorit till en del av din morgonrutin från och med nu!"
                  ]
              }
          , DOM.p
              { className: "mb-3.5 pr-5"
              , children: [ DOM.text "Lycka till!" ]
              }
          , DOM.br {}
          , DOM.p
              { className: "mb-3.5 pr-5"
              , children:
                  [ DOM.text "Upplever du problem kan du alltid ta kontakt med vår "
                  , DOM.a
                      { href: "/sida/kontakt"
                      , className: "static-page__link"
                      , children: [ DOM.text "kundservice" ]
                      }
                  , DOM.text ", så hjälper vi dig! Instruktioner för hur man löser vanliga problem hittar du under våra "
                  , DOM.a
                      { href: "/sida/fragor-och-svar#sajten"
                      , className: "static-page__link"
                      , children: [DOM.text "Frågor och svar"]
                      }
                  ]
              }
          , DOM.button
              { className: "hidden justify-center items-center self-end py-1 px-2 mb-2 font-bold text-white bg-brand md:flex"
              , onClick: handler_ $ Window.print =<< HTML.window
              , children:
                  [ DOM.i {className: "block mr-1 w-6 h-6 bg-white maskimage-print mask-size-contain mask-position-center"}
                  , DOM.text "Skriv ut"
                  ]
              }
          , DOM.link
              { rel: "stylesheet"
              , href: case paper of
                  VN -> "https://web.keesing.com/pub/config/ksf/css/vastranyland.css"
                  ON -> "https://web.keesing.com/pub/config/ksf/css/ostnyland.css"
                  _  -> "https://web.keesing.com/pub/config/ksf/css/hbl.css"
              }
          ]
        }
      ]
    }
