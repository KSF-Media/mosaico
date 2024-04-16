module Mosaico.Component.Epaper where

import Prelude

import Data.Foldable (lookup)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Tuple.Nested ((/\))
import Data.UUID as UUID
import Foreign.Object as Object
import KSF.Api (UserAuth, Token(..))
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

type Props =
  { user :: Maybe (Maybe User)
  , entitlements :: Map String UserAuth
  , loadingEntitlements :: Boolean
  , paper :: Paper
  , onLogin :: EventHandler
  }

data AccessEntitlement = NoAccount | LoggedInNoAccess | Entitled UserAuth

render :: Props -> JSX
render props =
  DOM.div
    { className: "flex justify-center"
    , children:
      [ DOM.div
          { className: "w-full mosaico-epaper lg:w-240"
          , children:
              [ renderPaper props.paper props.onLogin loading $ getAuth paper
              , case paper of
                    HBL -> renderPaper JUNIOR props.onLogin loading $ getAuth JUNIOR
                    _ -> mempty
              ]
          }]
    }
  where
    paper = props.paper
    loading = isNothing props.user || props.loadingEntitlements
    entitlementNeeded = [ HBL /\ "hbl-epaper"
                        , VN /\ "vn-epaper"
                        , ON /\ "on-epaper"
                        , JUNIOR /\ "junior-epaper"
                        ]
    authForEntl p = flip Map.lookup props.entitlements =<< lookup p entitlementNeeded
    getAuth p = case authForEntl p of
      Just tokens -> Entitled tokens
      Nothing -> if isJust $ join props.user then LoggedInNoAccess else NoAccount

renderPaper :: Paper -> EventHandler -> Boolean -> AccessEntitlement -> JSX
renderPaper paper onLogin loading entitlement =
  section
    [ DOM.a
        { className: "w-full h-auto bg-center bg-no-repeat sm:max-w-xs md:w-1/2 mosaico-epaper--teaser mosaico-epaper--bg-cover" <> if paper == JUNIOR then " mosaico-epaper__junior" else ""
        , href: paperLink paper entitlement
        , children: [ DOM.img { src: "https://cdn.ksfmedia.fi/mosaico/tablet-bg.png" } ]
        }
    , epaperBody paper onLogin loading entitlement
    ]

section :: Array JSX -> JSX
section children =
  DOM.div
    { className: "flex flex-col items-center p-5 md:pt-14 md:flex-row md:justify-around odd:bg-aptoma-epaper-background odd:md:pb-14"
    , children
    }

epaperBody :: Paper -> EventHandler -> Boolean -> AccessEntitlement -> JSX
epaperBody paper onLogin loading entitlement =
  DOM.div
    { className: "flex flex-col px-8 pt-5 max-w-lg text-lg font-light leading-5 text-center md:max-w-md basis-1/2 mosaico-epaper--body font-roboto"
    , children: [ description paper, button ]
    , _data: Object.singleton "test-epaperbody" ""
    }
    where
      description = case entitlement of
        Entitled _ -> entitledDescription
        _          -> unentitledDescription
      button = primaryOpenButton loading entitlement

      primaryOpenButton :: Boolean -> AccessEntitlement -> JSX
      primaryOpenButton _    (Entitled tokens) =
        fragment
          [ primaryLink (latestEpaper paper tokens) "Öppna det senaste numret"
          , secondaryLink (epaperSite paper <> authQuery tokens) "Bläddra i arkivet"
          ]
      primaryOpenButton true _                 = loadingSpinner
      primaryOpenButton _    NoAccount         = primaryButton onLogin "Logga in"
      primaryOpenButton _    LoggedInNoAccess  = primaryLink (prenumereraLink paper) "Prenumerera"

authQuery :: UserAuth -> String
authQuery { userId, authToken: (Token token) } = "?uuid=" <> UUID.toString userId <> "&token=" <> token

paperLink :: Paper -> AccessEntitlement -> String
paperLink paper (Entitled tokens) = latestEpaper paper tokens
paperLink paper _ = prenumereraLink paper

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
    , _data: Object.singleton "test-primarybutton" ""
    }

primaryLink :: String -> String -> JSX
primaryLink href text =
  DOM.a
    { target: "_blank"
    , className: "p-3 mr-2 ml-2 text-base font-normal tracking-wide text-white rounded border border-brand bg-brand"
    , href
    , _data: Object.singleton "test-primarylink" ""
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
