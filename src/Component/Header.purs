module Mosaico.Component.Header
  ( Props
  , component
  , render
  )
  where

import Prelude

import Data.Array (drop)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object (singleton)
import Foreign.Object as Object
import KSF.Paper (toString, paperName)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera.Models (Category(..), tagToURIComponent)
import Mosaico.Client.Handlers (Handlers)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffectOnce)
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (scrollY, toEventTarget)

type Props
  = { categoryStructure :: Array Category
    , handlers :: Handlers
    -- Nothing for loading state, Just Nothing for no user
    , user :: Maybe (Maybe User)
    , showHeading :: Boolean
    }

component :: Component Props
component = do
  w <- window
  React.component "Header" $ \props -> React.do
    scrollPosition /\ setScrollPosition <- React.useState 0
    useEffectOnce do
      let target = toEventTarget w
      listener <-
        eventListener
        ( \_ -> do
             yPosition <- scrollY w
             setScrollPosition (const $ ceil yPosition)
             pure unit
        )
      addEventListener (EventType "scroll") listener false target
      addEventListener (EventType "pageshow") listener false target
      pure do
        _ <- removeEventListener (EventType "scroll") listener false target
        removeEventListener (EventType "pageshow") listener false target
    pure $ render scrollPosition props

render :: Int -> Props -> JSX
render scrollPosition props =
  -- We have an extra level of wrapping here to prevent 'wobbliness'
  -- when jumping to page elements (eg. /sida/fragor-och-svar).
  DOM.header
    { className: "header-container-container"
    , children:
        [ DOM.div
            { className: "text-aptoma-text-color bg-aptoma-site-background lg:pt-2 font-roboto header-container" <> (if scrollPosition == 0 then "" else " static-header")
            , children:
                [ DOM.div
                    { className: "px-6 text-base mosaico-header roboto lg:p-0"
                    , children: headerContent props
                    }
                , mainSeparator
                ]
              }
        ]
    }

headerContent :: Props -> Array JSX
headerContent props =
  [ srHeading
  , DOM.div
      { className: "hidden text-sm whitespace-nowrap mosaico-header__left-links w-fit md:flex"
      , children:
          [ DOM.a
              { className: "pr-1 no-underline"
              , href: "/sida/kontakt"
              , onClick: props.handlers.onStaticPageClick "kontakt"
              , children: [ DOM.text "KONTAKTA OSS" ]
              }
          , DOM.text "|"
          , DOM.a
              { className: "px-1 no-underline"
              , children: [ DOM.text "E-TIDNINGEN" ]
              , href: "/epaper"
              , onClick: props.handlers.onMainClick "/epaper"
              }
          , DOM.text "|"
          , DOM.a
              { className: "pl-1 no-underline"
              , children: [ DOM.text "KORSORD" ]
              , href: "/korsord"
              , onClick: props.handlers.onMainClick "/korsord"
              }
            ]
      }
  , DOM.div
      { className: "text-[13px] mosaico-header__right-links text-neutral flex flex-col items-end"
      , children:
          [ DOM.a
              { className: "hidden no-underline md:block"
              , children: [ DOM.text "KUNDSERVICE" ]
              , href: "/sida/kundservice"
              , onClick: props.handlers.onStaticPageClick "kundservice"
              }
          , DOM.a
              { className: "px-[3px] md:p-[3px] text-center text-white no-underline rounded bg-neutral md:block" <>  (guard (isLoggedIn props.user) $ " hidden")
              , children: [ DOM.text "PRENUMERERA" ]
              , href: "https://prenumerera.ksfmedia.fi/#/" <> String.toLower (toString mosaicoPaper)
              , target: "_blank"
              }
          ]
      }
  , DOM.a
      { className: "flex items-center h-12 mosaico-header__logo lg:h-16"
      , href: "/"
      , onClick: props.handlers.onLogoClick
      , children:
          [ icon "mosaico-header__icon--paper"
          , DOM.span
              { className: "sr-only"
              , children: [DOM.text (paperName mosaicoPaper)]
              }
          ]
      }
  , renderLoginLink props.user
  , DOM.nav
      { className: "hidden overflow-y-hidden flex-wrap justify-center items-center h-8 mosaico-header__center-links md:flex md:h-6"
      , children: map mkCategory headerCategories
      }
  , DOM.div
      { className: "flex justify-end items-center mosaico-header__right-buttons"
      , children:
          [ searchButton
          , DOM.button
              { className: "flex items-center"
              , children: [icon "w-5 h-5 maskimage-menu"
                          , DOM.span
                              { className: "hidden ml-2 lg:inline-block"
                              , children: [ DOM.text "MENY" ]
                              }
                          ]
              , onClick: handler_ props.handlers.onMenuClick
              }
          ]
      }
  ]

  where
    srHeading =
        if props.showHeading
        then DOM.h1
               { className: "sr-only"
               , children: [DOM.text $ paperName mosaicoPaper]
               }
        else mempty

    mkCategory category@(Category c@{ label }) =
        DOM.a
        { className: "px-2 h-6 no-underline"
        , href: case c of
          { tag: Just tag } -> "/tagg/" <> tagToURIComponent tag
          { url: Just url } -> url
          _                 -> "/" <> show label
        , onClick: props.handlers.onCategoryClick category
        , children: [ DOM.text $ String.toUpper $ unwrap label ]
        }

    -- First position is reserved for Startsidan
    headerCategories = drop 1 props.categoryStructure

    searchButton :: JSX
    searchButton = DOM.a
                    { className: "flex items-center mr-4"
                    , children: [ icon "w-5 h-5 maskimage-search"
                                , DOM.span
                                    { className: "hidden ml-2 lg:inline-block"
                                    , children: [ DOM.text "SÖK" ]
                                    }
                                ]
                    , href: "/sök"
                    , onClick: props.handlers.onMainClick "/sök"
                    }

    renderLoginLink Nothing =
      loadingSpinner
    renderLoginLink (Just Nothing) =
      DOM.button
         { children:
             [ icon "w-6 h-6 maskimage-login"
             , DOM.span
                 { className: "ml-1"
                 , children: [ DOM.text "LOGGA IN" ]
                 , onClick: props.handlers.onLogin
                 }
             ]
         , onClick: props.handlers.onLogin
         , className: "flex items-center self-center h-8 no-underline uppercase mosaico-header__account md:self-stretch"
         , _data: Object.fromFoldable [Tuple "login" "1"]
         }
    renderLoginLink (Just (Just user)) =
      let name = fromMaybe "INLOGGAD" $ toMaybe user.firstName
      in DOM.a
           { className: "flex items-center self-center h-8 no-underline uppercase mosaico-header__account md:self-stretch"
           , onClick: props.handlers.onMainClick "/konto"
           , href: "/konto"
           , children:
               -- here, vertically centering the icon 'perfectly' with the text looks off-center
               [ icon "w-6 h-6 mb-[2px] maskimage-profile"
               , DOM.span
                   { className: "hidden ml-1 lg:inline-block"
                   , children: [ DOM.text name ]
                   }
               ]
           , _data: Object.fromFoldable [Tuple "loggedin" "1"]
           }
    
    isLoggedIn (Just (Just _)) = true
    isLoggedIn _ = false
-- The separator between the header and the rest of the page
mainSeparator :: JSX
mainSeparator = DOM.hr { className: "mosaico-main-separator" }

icon :: String -> JSX
icon iconClass = DOM.span
                   { _aria: singleton "hidden" "true"
                   -- bg color here means 'color of icon'
                   , className: "block bg-aptoma-text-color mask-size-contain mask-position-center mask-repeat-none" <> " " <> iconClass
                   }
