module Mosaico.Header.Menu where

import Prelude

import Data.Array (intersperse, foldl, snoc)
import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String (toUpper)
import KSF.Paper (Paper(..), toString)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera.Models (Category(..), CategoryLabel)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)

type Props =
  { changeRoute :: String -> Effect Unit
  , categoryStructure :: Array Category
  , onCategoryClick :: Category -> EventHandler
    -- Nothing for loading state, Just Nothing for no user
  , user :: Maybe (Maybe User)
  , onLogin :: EventHandler
  , onLogout :: EventHandler
  }

type Section =
  { title :: String
  , subsections :: Array Subsection
  , url :: String
  , onClick :: EventHandler
  , className :: Maybe String
  }

type Subsection =
  { title :: CategoryLabel
  , onClick :: EventHandler
  }

render :: Props -> JSX
render props@{ onLogin, onLogout } = DOM.div
  { className: "flex mx-6 lg:mx-0 [grid-area:full-width] md:[grid-column:1/span_2] lg:[grid-column:2/span_3] mosaico-menu"
  , children: [ menuContent ]
  }
  where
    menuContent :: JSX
    menuContent = DOM.div
      { className: "flex flex-col justify-around mt-5 w-full"
      , children: [ prenumereraBlock, topButtons, renderSeparator, categories, renderDesktopSeparator, bottomBlock ]
      }

    categories :: JSX
    categories = DOM.div
      { className: "flex flex-col justify-around mb-4 lg:flex-row lg:flex-nowrap lg:justify-center"
      , children: (flip snoc) renderMobileSeparator $ intersperse renderMobileSeparator $ foldl mkCategory [] props.categoryStructure
      }

    bottomBlock :: JSX
    bottomBlock =
      DOM.div
      { className: "flex flex-col justify-around mb-4 lg:flex-row lg:flex-nowrap lg:justify-center"
      , children: bottomLinks
      }

    renderDesktopSeparator :: JSX
    renderDesktopSeparator = DOM.div { className: "hidden lg:block", children: [renderSeparator] }

    renderMobileSeparator :: JSX
    renderMobileSeparator = DOM.div { className: "block lg:hidden", children: [renderSeparator] }

    renderSeparator :: JSX
    renderSeparator = DOM.hr { className: "w-full h-0 border-t border-gray-300 border-solid"}

    onSearch :: EventHandler
    onSearch = capture_ $ props.changeRoute "/s??k"

    onEpaper :: EventHandler
    onEpaper = capture_ $ props.changeRoute "/epaper/"

    onKundservice :: EventHandler
    onKundservice = capture_ $ props.changeRoute "/sida/kundservice/"

    prenumereraBlock :: JSX
    prenumereraBlock = DOM.div
          { className: "lg:hidden"
          , children:
            [ renderCategoryHeader
              [ DOM.a
                { href: ("https://prenumerera.ksfmedia.fi/#/" <> String.toLower (toString mosaicoPaper))
                , children: [ DOM.text "PRENUMERERA" ]
                , className: "text-sm"
                }
              ]
              , DOM.div {className: "mt-4 mb-7", children: [ renderSeparator ]}
            ]
          }

    topButtons :: JSX
    topButtons = DOM.div
      { className: "flex flex-col justify-around mb-4 lg:flex-row lg:flex-nowrap lg:justify-center"
      , children:
        [ renderIcon "bg-search" "S??K" "/s??k" onSearch
        , renderIcon "bg-epaper" "E-TIDNINGEN" "/epaper" onEpaper
        , renderIcon "bg-kundservice" "KUNDSERVICE" "/sida/kundservice" onKundservice
        , case props.user of
            Nothing -> renderLoadingIcon
            Just (Just _) -> renderIcon "bg-logout" "LOGGA UT" "" onLogout
            Just Nothing -> renderIcon "bg-login" "LOGGA IN" "" onLogin
        ]
      }

    mkCategory acc category@(Category c) =
      let mkSubsection subCategory@(Category { label }) =
            { title: label
            , onClick: props.onCategoryClick subCategory
            }
          section =
            renderCategory
              { title: toUpper $ unwrap c.label
              , subsections: map mkSubsection c.subCategories
              , url: "/" <> show c.label
              , onClick: props.onCategoryClick category
              , className: Nothing
              }
      in acc `snoc` section

    bottomLinks :: Array JSX
    bottomLinks = [ renderCategory { title: "KONTAKTA OSS"
                    , subsections: []
                    , url: "/sida/kontakt"
                    , onClick: capture_ $ props.changeRoute "/sida/kontakt"
                    , className: Just "w-40"
                    }
                  , renderCategory { title: "F??RETAGSANNONSER"
                    , subsections: []
                    , url: "https://www.ksfmedia.fi/"
                    , onClick: mempty
                    , className: Just "w-40"
                    }
                  , maybe mempty renderPrivateAnnonserCategory $ privatAnnonserLink mosaicoPaper
                  , renderCategory { title: "JOBBA HOS OSS"
                    , subsections: []
                    , url: "https://www.ksfmedia.fi/jobba-hos-oss"
                    , onClick: mempty
                    , className: Just "w-40"
                    }
                  ] <> paperSpecificLinks mosaicoPaper
                  <> [ renderCategory { title: "NYHETSAPPAR"
                    , subsections: []
                    , url: "/sida/app"
                    , onClick: capture_ $ props.changeRoute "/sida/app"
                    , className: Just "w-40"
                    }
                  , renderCategory { title: "NYHETSBREV"
                    , subsections: []
                    , url: "/sida/nyhetsbrev"
                    , onClick: capture_ $ props.changeRoute "/sida/nyhetsbrev"
                    , className: Just "w-40"
                    }
                  ]

    privatAnnonserLink :: Paper -> Maybe String
    privatAnnonserLink HBL = Just "https://annonskiosken.ksfmedia.fi/ilmoita/hufvudstadsbladet"
    privatAnnonserLink VN = Just "https://annonskiosken.ksfmedia.fi/ilmoita/vastranyland"
    privatAnnonserLink ON = Just "https://annonskiosken.ksfmedia.fi/ilmoita/ostnyland"
    privatAnnonserLink _ = Nothing

    renderPrivateAnnonserCategory :: String -> JSX
    renderPrivateAnnonserCategory url =
      renderCategory
        { title: "PRIVATANNONSER"
        , subsections: []
        , url
        , onClick: mempty
        , className: Just "w-40"
        }

    paperSpecificLinks :: Paper -> Array JSX
    paperSpecificLinks VN = vastranylandMenuLinks
    paperSpecificLinks _ = mempty

    vastranylandMenuLinks :: Array JSX
    vastranylandMenuLinks =
      [ renderCategory { title: "ANSLAGSTAVLAN"
        , subsections: []
        , url: "/sida/anslagstavlan"
        , onClick: capture_ $ props.changeRoute "/sida/anslagstavlan"
        , className: Just "w-40"
        }
      ]

    renderLoadingIcon :: JSX
    renderLoadingIcon = DOM.div
      { className: "mx-4 w-40 text-sm lg:flex"
      , children: [ loadingSpinner ]
      }

    renderIcon :: String -> String -> String -> EventHandler -> JSX
    renderIcon icon title href onClick = DOM.a
      { children: [ DOM.span { className: "block mr-3 w-9 h-9 bg-no-repeat bg-contain" <> " " <> icon}
                  , DOM.text title
                  ]
      , className: "flex items-center mr-4 w-40 text-sm font-semibold lg:justify-center font-roboto"
      , href
      , onClick
      }

    renderCategoryHeader :: Array JSX -> JSX
    renderCategoryHeader children = DOM.div
      { className: "justify-center text-sm lg:flex lg:flex-row lg:justify-between"
      , children:
          [ DOM.div
              { className: "py-4 text-sm font-semibold font-roboto"
              , children
              }
          ]
      }

    renderCategory :: Section -> JSX
    renderCategory { subsections, title, url, onClick, className } = DOM.div
      { className: "lg:my-8 lg:flex lg:flex-col lg:min-w-[130px]"
      , children: [ renderCategoryHeader
                      [ DOM.h2_
                          [ DOM.a
                              { href: url
                              , children: [ DOM.text title ]
                              , onClick
                              , className: "block " <> fromMaybe mempty className
                              }
                          ]
                      ]
                  , DOM.div
                      { className: "lg:flex lg:flex-col"
                      , children: renderSubcategory <$> subsections
                      }
                  ]
      }

    renderSubcategory :: Subsection -> JSX
    renderSubcategory { title, onClick } = DOM.div
      { className: "text-sm font-roboto last:mb-2"
      , children:
          [ DOM.a
              { href: "/" <> show title
              , className: "block pb-2"
              , children: [ DOM.text $ unwrap title ]
              , onClick
              }
          ]
      }
