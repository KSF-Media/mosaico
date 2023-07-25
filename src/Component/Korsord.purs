module Mosaico.Component.Korsord where

import Prelude

import Data.Either (hush)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isJust, fromMaybe,  maybe)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Auth (loadToken)
import KSF.Paper (Paper(..))
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.User as User
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React
import Web.HTML as HTML
import Web.HTML.Window as Window

foreign import loadKorsord :: Effect Unit

type Props =
  { user :: Maybe (Maybe User)
  , paper :: Paper
  , paywall :: JSX
  }

component :: Component Props
component = do
  React.component "Korsord" $ \props@{user, paper} -> React.do
    entitlements /\ setEntitlements <- useState' Nothing
    useEffect {l1: isJust user, l2: isJust <$> user} do
      case user of
        Just Nothing -> Aff.launchAff_ do
          tokens <- User.loginIP paper
          liftEffect case (hush tokens) of
            Nothing -> setEntitlements $ Just mempty
            Just auth -> do
              Aff.launchAff_ $ do
                ipEntitlements <- User.getUserEntitlements auth
                liftEffect $ setEntitlements $ Just $ fromMaybe mempty $ hush ipEntitlements
        Just _ -> do
          tokens <- loadToken
          maybe
            (setEntitlements $ Just mempty)
            (Aff.launchAff_ <<< (liftEffect <<< setEntitlements <<< Just
                                 <<< fromMaybe Set.empty <<< hush <=< User.getUserEntitlements)) tokens
        _ -> pure unit
      pure $ pure unit
    let isEntitled = (\entitlement -> any (flip Set.member $ entitlement) ["hbl-web", "vn-web", "on-web"]) <$> entitlements
    useEffect isEntitled $ do
        when (isEntitled == Just true) loadKorsord
        pure $ pure unit
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
                  , DOM.text " på fredagar. Kom ihåg att du kan skicka in dina lösningar om du vill tävla om att vinna fina priser!"
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
