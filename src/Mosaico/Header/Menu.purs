module Mosaico.Header.Menu where

import Prelude

import Data.Array (intersperse, foldl, snoc)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Foreign.Object (singleton)
import Data.String as String
import Effect (Effect)
import KSF.Paper (Paper(..), toString)
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import Lettera.Models (Category(..), CategoryLabel, tagToURIComponent)
import Mosaico.Client.Handlers (Handlers)
import Mosaico.Paper (mosaicoPaper)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)

type Props =
  { categoryStructure :: Array Category
  , handlers :: Handlers
    -- Nothing for loading state, Just Nothing for no user
  , user :: Maybe (Maybe User)
  }

type Section =
  { title :: String
  , subsections :: Array Subsection
  , url :: String
  , onClick :: EventHandler
  }

type Subsection =
  { title :: CategoryLabel
  , onClick :: EventHandler
  }

foreign import toggleMode :: Effect Unit

render :: Props -> JSX
render props@{ handlers: { onLogin, onLogout, onMainClick, onStaticPageClick } } = DOM.div
  { className: "flex mx-6 lg:mx-0 [grid-area:full-width] md:[grid-column:1/span_2] lg:[grid-column:2/span_3] mosaico-menu"
  , children: [ menuContent ]
  }
  where
    menuContent :: JSX
    menuContent = DOM.div
      { className: "flex flex-col justify-around mt-5 mb-10 w-full"
      , children: [ prenumereraBlock, topButtons, renderSeparator, categories, renderDesktopSeparator, bottomBlock, themeToggler ]
      }

    categories :: JSX
    categories = DOM.div
      { className: "flex flex-col justify-around mb-4 lg:flex-row lg:flex-nowrap lg:justify-center"
      , children: (flip snoc) renderMobileSeparator $ intersperse renderMobileSeparator $ foldl mkCategory [] props.categoryStructure
      }

    bottomBlock :: JSX
    bottomBlock =
      DOM.div
      { className: "flex flex-col justify-around lg:flex-row lg:flex-nowrap lg:justify-center"
      , children: bottomLinks
      }

    renderDesktopSeparator :: JSX
    renderDesktopSeparator = DOM.div { className: "hidden lg:block", children: [renderSeparator] }

    renderMobileSeparator :: JSX
    renderMobileSeparator = DOM.div { className: "block lg:hidden", children: [renderSeparator] }

    renderSeparator :: JSX
    renderSeparator = DOM.hr { className: "w-full h-0 border-t border-gray-300 border-solid"}

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
        [ renderIcon "maskimage-search w-9 h-7" "SÖK" "/sök" $ onMainClick "/sök"
        , renderIcon "maskimage-epaper w-9 h-9" "E-TIDNINGEN" "/epaper" $ onMainClick $ "/epaper"
        , renderIcon "maskimage-crosswords w-9 h-7" "KORSORD" "/korsord" $ onMainClick "/korsord"
        , renderIcon "maskimage-kundservice w-9 h-9" "KUNDSERVICE" "/sida/kundservice" $ onStaticPageClick "kundservice"
        , case props.user of
            Nothing -> renderLoadingIcon
            Just (Just _) -> renderIcon "maskimage-logout w-9 h-7" "LOGGA UT" "" onLogout
            Just Nothing -> renderIcon "maskimage-login w-9 h-7" "LOGGA IN" "" onLogin
        ]
      }

    mkCategory acc category@(Category c) =
      let mkSubsection subCategory@(Category { label }) =
            { title: label
            , onClick: props.handlers.onCategoryClick subCategory
            }
          section =
            renderCategory
              { title: toUpper $ unwrap c.label
              , subsections: map mkSubsection c.subCategories
              , url: case c of
                  { tag: Just tag } -> "/tagg/" <> tagToURIComponent tag
                  { url: Just url } -> url
                  _                 -> "/" <> show c.label
              , onClick: props.handlers.onCategoryClick category
              }
      in acc `snoc` section

    bottomLinks :: Array JSX
    bottomLinks = [ renderBottomLink { title: "KONTAKTA OSS" , url: Right "/sida/kontakt" }
                  , renderBottomLink { title: "FÖRETAGSANNONSER" , url: Left "https://media.hbl.fi/" }
                  , renderBottomLink { title: "PRIVATANNONSER", url: Left "/annonskiosken" }
                  , renderBottomLink { title: "JOBBA HOS OSS" , url: Left "https://www.ksfmedia.fi/jobba-hos-oss" }
                  ]
                  <> paperSpecificLinks mosaicoPaper
                  <>
                  [ renderBottomLink { title: "NYHETSAPPAR" , url: Right "/sida/app" }
                  , renderBottomLink { title: "NYHETSBREV" , url: Right "/sida/nyhetsbrev" }
                  ]

    paperSpecificLinks :: Paper -> Array JSX
    paperSpecificLinks VN = vastranylandMenuLinks
    paperSpecificLinks _ = mempty

    vastranylandMenuLinks :: Array JSX
    vastranylandMenuLinks = [ renderBottomLink { title: "ANSLAGSTAVLAN" , url: Right "/sida/anslagstavlan" } ]

    renderLoadingIcon :: JSX
    renderLoadingIcon = DOM.div
      { className: "mx-4 w-40 text-sm lg:flex"
      , children: [ loadingSpinner ]
      }

    renderIcon :: String -> String -> String -> EventHandler -> JSX
    renderIcon icon title href onClick = DOM.a
      -- bg color here means 'color of icon'
      { children: [ DOM.span { className: "block mr-3 bg-aptoma-text-color mask-repeat-none mask-size-contain mask-position-center" <> " " <> icon}
                  , DOM.text title
                  ]
      , className: "flex items-center mr-4 mb-2 w-40 text-sm font-semibold lg:mb-0 lg:justify-center font-roboto"
      , href
      , onClick
      }

    renderCategoryHeader :: Array JSX -> JSX
    renderCategoryHeader children = DOM.div
      { className: "text-sm lg:flex lg:flex-row"
      , children:
          [ DOM.div
              { className: "py-4 text-sm font-semibold font-roboto"
              , children
              }
          ]
      }

    renderCategory :: Section -> JSX
    renderCategory { subsections, title, url, onClick } = DOM.div
      { className: "lg:my-8 lg:flex lg:flex-col lg:min-w-[130px] lg:px-1"
      , children:
          [ renderCategoryHeader
              [ DOM.h2_
                  [ DOM.a
                      { href: url
                      , children: [ DOM.text title ]
                      , onClick
                      , className: "block"
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

    renderBottomLink :: { title :: String , url :: Either String String } -> JSX
    renderBottomLink { title, url } = DOM.div
      { className: "justify-center py-4 text-sm font-semibold font-roboto lg:my-8 lg:px-2 lg:flex lg:flex-row lg:justify-around"
      , children:
          [ DOM.h2_
              [ DOM.a
                  { href: either identity identity url
                  , children: [ DOM.text title ]
                  , onClick: either (const mempty) props.handlers.onMainClick url
                  , className: "block"
                  }
              ]
          ]
      }

    themeToggler :: JSX
    themeToggler = DOM.div
      { className: "flex py-4 lg:justify-center"
      , children:
          [ DOM.button
              { className: "flex flex-row-reverse items-center text-sm font-semibold font-roboto lg:flex-row"
              , onClick: capture_ toggleMode
              , children:
                [ DOM.i
                    { className: "mr-3 w-7 h-7 glyphicon glyphicon-adjust text-[24px]"
                    , _aria: singleton "hidden" "true"
                    }
                , DOM.span
                    { className: "hidden mr-3 dark:block"
                    , children: [ DOM.text "MÖRKT TEMA PÅ" ]
                    }
                , DOM.span
                    { className: "block mr-3 dark:hidden"
                    , children: [ DOM.text "MÖRKT TEMA AV" ]
                    }
                ]
              }
          ]
      }
