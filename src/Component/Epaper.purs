module Mosaico.Component.Epaper where

import Prelude

import Data.Either (hush)
import Data.Foldable (lookup)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe,  maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.UUID as UUID
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import KSF.Auth (loadToken)
import KSF.Api (UserAuth, Token(..))
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.User as User
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, useEffect, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { user :: Maybe (Maybe User)
  , paper :: Paper
  , onLogin :: EventHandler
  }

type State =
  { userAuth :: Maybe UserAuth
  , entitlements :: Maybe (Set String)
  }

component :: Component Props
component = do
  initialTokens <- loadToken
  React.component "Epaper" $ \props@{user, paper} -> React.do
    entitlements /\ setEntitlements <- useState' Nothing
    userAuth /\ setUserAuth <- useState' initialTokens
    useEffect (_.uuid <$> join user) do
      when (isNothing $ join user) $ Aff.launchAff_ do
        tokens <- User.loginIP paper
        liftEffect case (hush tokens) of
          Nothing -> setEntitlements $ Nothing
          Just auth -> do
            setUserAuth $ Just auth
            Aff.launchAff_ $ do
              ipEntitlements <- User.getUserEntitlements auth
              liftEffect $ setEntitlements $ hush ipEntitlements
      when (isNothing entitlements && isJust (join user)) do
        setEntitlements Nothing
        tokens <- loadToken
        setUserAuth tokens
        maybe
          (setEntitlements $ Just mempty)
          (Aff.launchAff_ <<< (liftEffect <<< setEntitlements <<< Just
                               <<< fromMaybe Set.empty <<< hush <=< User.getUserEntitlements)) tokens
      pure $ pure unit
    pure $ render (Just { userAuth, entitlements }) props

render :: Maybe State -> Props -> JSX
render state props =
  DOM.div
    { className: "flex justify-center"
    , children:
      [ DOM.div
          { className: "w-full mosaico-epaper lg:w-240"
          , children:
              [ renderPaper paper onLogin loading userAuth entitled
              , case paper of
                    HBL -> renderPaper JUNIOR onLogin loading userAuth (isEntitledTo JUNIOR)
                    _ -> mempty
              ]
          }]
    }
  where
    paper = props.paper
    onLogin = props.onLogin
    userAuth = _.userAuth =<< state
    entitled = isEntitledTo props.paper
    entitlements = _.entitlements =<< state
    loading = isNothing props.user || isJust userAuth && isNothing entitlements
    entitlementNeeded = [ HBL /\ "hbl-epaper"
                        , VN /\ "vn-epaper"
                        , ON /\ "on-epaper"
                        , JUNIOR /\ "junior-epaper"
                        ]
    isEntitledTo p = fromMaybe false $
                     Set.member <$> lookup p entitlementNeeded <*> (_.entitlements =<< state)

renderPaper :: Paper -> EventHandler -> Boolean -> Maybe UserAuth -> Boolean -> JSX
renderPaper paper onLogin loading userAuth entitled =
  section
    [ DOM.a
        { className: "w-full h-auto bg-center bg-no-repeat sm:max-w-xs md:w-1/2 mosaico-epaper--teaser mosaico-epaper--bg-cover" <> if paper == JUNIOR then " mosaico-epaper__junior" else ""
        , href: paperLink paper userAuth entitled
        , children: [ DOM.img { src: "https://cdn.ksfmedia.fi/mosaico/tablet-bg.png" } ]
        }
    , epaperBody paper onLogin loading userAuth entitled
    ]

section :: Array JSX -> JSX
section children =
  DOM.div
    { className: "flex flex-col items-center p-5 md:pt-14 md:flex-row md:justify-around odd:bg-aptoma-epaper-background odd:md:pb-14"
    , children
    }

epaperBody :: Paper -> EventHandler -> Boolean -> Maybe UserAuth -> Boolean -> JSX
epaperBody paper onLogin loading userAuth entitled =
  DOM.div
    { className: "flex flex-col px-8 pt-5 max-w-lg text-lg font-light leading-5 text-center md:max-w-md basis-1/2 mosaico-epaper--body font-roboto"
    , children: [ description paper, button ]
    }
    where
      description = if entitled then entitledDescription else unentitledDescription
      button = primaryOpenButton loading userAuth entitled

      primaryOpenButton :: Boolean -> Maybe UserAuth -> Boolean -> JSX
      primaryOpenButton true _             _     = loadingSpinner
      primaryOpenButton _    Nothing       _     = primaryButton onLogin "Logga in"
      primaryOpenButton _    _             false = primaryLink (prenumereraLink paper) "Prenumerera"
      primaryOpenButton _    (Just tokens) _     =
        fragment
          [ primaryLink (latestEpaper paper tokens) "Öppna det senaste numret"
          , secondaryLink (epaperSite paper <> authQuery tokens) "Bläddra i arkivet"
          ]

authQuery :: UserAuth -> String
authQuery { userId, authToken: (Token token) } = "?uuid=" <> UUID.toString userId <> "&token=" <> token

paperLink :: Paper -> Maybe UserAuth -> Boolean -> String
paperLink paper (Just tokens) true = latestEpaper paper tokens
paperLink paper _ _ = prenumereraLink paper

prenumereraLink :: Paper -> String
prenumereraLink paper = "https://prenumerera.ksfmedia.fi/#/" <> Paper.cssName paper

epaperSite :: Paper -> String
epaperSite HBL    = "https://etidningen.hbl.fi/"
epaperSite VN     = "https://etidningen.vastranyland.fi/"
epaperSite ON     = "https://etidningen.ostnyland.fi/"
epaperSite JUNIOR = "https://e-junior.hbl.fi/"
epaperSite _      = "INVALID"

latestEpaper :: Paper -> UserAuth -> String
latestEpaper paper tokens = epaperSite paper <> path <> authQuery tokens
  where
    path = case paper of
      HBL    -> "titles/hbl/3271/publications/latest"
      VN     -> "titles/vastranyland/3668/publications/latest"
      ON     -> "titles/ostnyland/3669/publications/latest"
      JUNIOR -> "titles/hbljunior/12509/publications/latest"
      _      -> "INVALID"

primaryButton :: EventHandler -> String -> JSX
primaryButton onClick text =
  DOM.button
    { onClick
    , className: "p-3 mr-2 ml-2 text-base font-normal tracking-wide text-white rounded border border-brand bg-brand"
    , children: [ DOM.text text ]
    }

primaryLink :: String -> String -> JSX
primaryLink href text =
  DOM.a
    { target: "_blank"
    , className: "p-3 mr-2 ml-2 text-base font-normal tracking-wide text-white rounded border border-brand bg-brand"
    , href
    , children: [ DOM.text text ]
    }

secondaryLink :: String -> String -> JSX
secondaryLink href text =
  DOM.a
    { target: "_blank"
    , className: "p-1 my-5 text-base font-normal text-gray-500 bg-transparent hover:opacity-75"
    , href
    , children: [ DOM.text text ]
    }

entitledDescription :: Paper -> JSX
entitledDescription JUNIOR = juniorDescription
entitledDescription _ =
  fragment
    [ DOM.h2_ [ DOM.text "Läs dagens tidning" ]
    , DOM.p_ [ DOM.text "Välkommen till den digitala e-tidningen! Här får du hela papperstidningen i en digital form. Klicka på en av länkarna nedan för att börja läsa." ]
    , epaperDescription
    ]

unentitledDescription :: Paper -> JSX
unentitledDescription JUNIOR = juniorDescription
unentitledDescription _ =
  fragment
    [ DOM.h2_ [ DOM.text "Läs dagens tidning" ]
    , DOM.p_ [ DOM.text "Välkommen till den digitala e-tidningen!" ]
    , epaperDescription
    ]

epaperDescription :: JSX
epaperDescription =
  fragment
  [ DOM.p_ [ DOM.text "I e-tidningen kan du bland annat:" ]
  , DOM.ul
      { className: "mt-4 mr-0 mb-4 ml-10 list-disc text-left"
      , children:
        [ DOM.li_ [ DOM.text "läsa artiklar i en lättläst textvy" ]
        , DOM.li_ [ DOM.text "söka efter artiklar i tidningen" ]
        , DOM.li_ [ DOM.text "fylla i korsordet digitalt" ]
        ]
      }
  ]

juniorDescription :: JSX
juniorDescription =
  fragment
    [ DOM.h2_ [ DOM.text "HBL Junior" ]
    , DOM.p_ [ DOM.text "HBL Junior är samhällsmagasinet för barn i åldern 7–13 år. Här kan du läsa tidningen i ett digitalt format. Tidningen utkommer varannan vecka." ]
    , DOM.p_ [ DOM.text "I HBL Junior hittar du det här och mycket mer:" ]
    , DOM.ul
        { className: "mt-4 mr-0 mb-4 ml-10 list-disc text-left"
        , children:
          [ DOM.li_ [ DOM.text "Nyheter och aktuella ämnen" ]
          , DOM.li_ [ DOM.text "Kultur och nöje" ]
          , DOM.li_ [ DOM.text "Sport och fritid" ]
          ]
        }
    ]
