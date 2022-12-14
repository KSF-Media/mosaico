module Mosaico.Header
  ( Props
  , component
  , render
  , topLine
  ) where

import Prelude

import Data.Array (head, splitAt)
import Data.Foldable (foldMap)
import Data.Int (ceil)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Foreign.Object (singleton)
import Effect (Effect)
import Foreign.Object as Object
import KSF.Paper (toString, paperName)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera.Models (Categories, Category(..))
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window (scrollY, toEventTarget)

type Props
  = { changeRoute :: String -> Effect Unit
    , categoryStructure :: Array Category
    , catMap :: Categories
    , onCategoryClick :: Category -> EventHandler
    , onLogin :: EventHandler
    , onProfile :: EventHandler
    , onStaticPageClick :: String -> EventHandler
    , onMenuClick :: Effect Unit
    -- Nothing for loading state, Just Nothing for no user
    , user :: Maybe (Maybe User)
    , showHeading :: Boolean
    }

component :: React.Component Props
component = do
  React.component "Header"
    $ \props -> React.do
        scrollPosition /\ setScrollPosition <- React.useState 0
        React.useEffect unit do
          w <- window
          listener <-
            eventListener
              ( \_ -> do
                  yPosition <- scrollY w
                  setScrollPosition (const $ ceil yPosition)
                  pure unit
              )
          addEventListener (EventType "scroll") listener false (toEventTarget w)
          addEventListener (EventType "pageshow") listener false (toEventTarget w)
          pure
            $ do
                _ <- removeEventListener (EventType "scroll") listener false (toEventTarget w)
                removeEventListener (EventType "pageshow") listener false (toEventTarget w)
        pure $ (render scrollPosition props)

render :: Int -> Props -> JSX
render scrollPosition props =
  -- We have an extra level of wrapping here to prevent 'wobbliness'
  -- when jumping to page elements (eg. /sida/fragor-och-svar).
  DOM.header
    { className: "header-container-container"
    , children:
        [ DOM.div
            { className: "header-container" <> (if scrollPosition == 0 then "" else " static-header")
            , children:
                [ DOM.div
                    { className: "mosaico-header"
                    , children:
                        [ srHeading
                        , DOM.div
                            { className: "mosaico-header__left-links"
                            , children:
                                [ DOM.a
                                    { href: "/sida/kontakt"
                                    , onClick: props.onStaticPageClick "kontakt"
                                    , children: [ DOM.text "KONTAKTA OSS" ]
                                    }
                                , DOM.text "|"
                                , DOM.a
                                    { children: [ DOM.text "E-TIDNINGEN" ]
                                    , href: "/epaper"
                                    , onClick: capture_ $ props.changeRoute "/epaper/"
                                    }
                                ]
                            }
                        , DOM.div
                            { className: "mosaico-header__right-links"
                            , children:
                                [ DOM.ul_
                                    [ DOM.li_
                                        [ DOM.a
                                            { className: "mosaico-header__kundservice-link"
                                            , children: [ DOM.text "KUNDSERVICE" ]
                                            , href: "/sida/kundservice"
                                            , onClick: props.onStaticPageClick "kundservice"
                                            }
                                        ]
                                    , DOM.li_
                                        [ DOM.a
                                            { className: "mosaico-header__prenumerera-link"
                                            , children: [ DOM.text "PRENUMERERA" ]
                                            , href: "https://prenumerera.ksfmedia.fi/#/" <> String.toLower (toString mosaicoPaper)
                                            , target: "_blank"
                                            }
                                        ]
                                    ]
                                ]
                            }
                        , DOM.a
                            { className: "mosaico-header__logo"
                            , href: "/"
                            , onClick: foldMap props.onCategoryClick frontpageCategory
                            , children: [ DOM.span
                                            { className: "sr-only"
                                            , children: [DOM.text (paperName mosaicoPaper)]
                                            }]
                            }
                        , renderLoginLink props.user
                        , DOM.nav
                            { className: "mosaico-header__center-links"
                            , children: map mkCategory headerCategories
                            }
                        , DOM.div
                            { className: "mosaico-header__right-buttons"
                            , children:
                                [ searchButton
                                , DOM.button
                                    { className: "mosaico-header__icon-button mosaico-header__icon-button--menu"
                                    , children: [ DOM.span { className: "mosaico-header__icon mosaico-header__icon--menu"}
                                                , DOM.span
                                                    { className: "menu-label"
                                                    , children: [ DOM.text "MENY" ]
                                                    }
                                                ]
                                    , onClick: handler_ props.onMenuClick
                                    }
                                ]
                            }
                        ]
                    }
                , mainSeparator
                ]
              }
        ]
    }
  where
    srHeading =
        if props.showHeading
        then DOM.h1
               { className: "sr-only"
               , children: [DOM.text $ paperName mosaicoPaper]
               }
        else mempty

    mkCategory category@(Category { label }) =
        DOM.a
        { href: "/" <> show label
        , onClick: props.onCategoryClick category
        , children: [ DOM.text $ String.toUpper $ unwrap label ]
        }

    { frontpageCategory, headerCategories } =
        let
        { after, before } = splitAt 1 props.categoryStructure
        in
        { frontpageCategory: head before, headerCategories: after }

    searchButton :: JSX
    searchButton = DOM.a
                    { className: "mosaico-header__icon-button mosaico-header__icon-button--search"
                    , children: [ DOM.span
                                    { _aria: singleton "hidden" "true"
                                    , className: "mosaico-header__icon mosaico-header__icon--search"
                                    }
                                , DOM.span
                                    { className: "menu-label"
                                    , children: [ DOM.text "S??K" ]
                                    }
                                ]
                    , href: "/s??k"
                    , onClick: capture_ $ props.changeRoute "/s??k"
                    }

    renderLoginLink Nothing =
      loadingSpinner
    renderLoginLink (Just Nothing) =
      DOM.button
         { children:
             [ DOM.span
                 { className: "mosaico-header__account-icon"
                 , children: [ DOM.span_ [] ]
                 }
             , DOM.span
                 { children: [ DOM.text "LOGGA IN" ]
                 , onClick: props.onLogin
                 }
             ]
         , onClick: props.onLogin
         , className: "mosaico-header__account mosaico-header__account--active"
         , _data: Object.fromFoldable [Tuple "login" "1"]
         }
    renderLoginLink (Just (Just user)) =
      let name = fromMaybe "INLOGGAD" $ toMaybe user.firstName
      in DOM.a
           { className: "mosaico-header__account"
           , onClick: props.onProfile
           , href: "/konto"
           , children:
               [ DOM.span
                   { className: "mosaico-header__account-icon"
                   , children: [ DOM.span_ [] ]
                   }
               , DOM.span
                   { className: "menu-label"
                   , children: [ DOM.text name ]
                   }
               ]
           , _data: Object.fromFoldable [Tuple "loggedin" "1"]
           }

-- The characteristic line at the top of every KSF media's site
topLine :: JSX
topLine = DOM.hr { className: "[grid-area:line] bg-brand w-full h-3 sticky top-0 z-10" }

-- The separator between the header and the rest of the page
mainSeparator :: JSX
mainSeparator = DOM.hr { className: "mosaico-main-separator" }
