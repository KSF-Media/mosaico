module Mosaico.Archive
       ( ArticleSelectionProps
       , DateSelectionProps
       , MonthSelectionProps
       , Props(..)
       , component
       , render
       ) where

import Prelude

import Data.Array (index, range)
import Data.Date (Date, Month, Year, day, lastDayOfMonth, month, year)
import Data.Enum (fromEnum)
import Data.Maybe (fromMaybe)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, JSX)
import React.Basic.Hooks as React

import KSF.Paper (Paper(..))
import Lettera.Models (ArticleStub)
import Mosaico.Client.Handlers (Handlers)
import Mosaico.Paper (mosaicoPaper)

monthNames :: Array String
monthNames =
  [ "januari", "februari", "mars", "april", "maj", "juni", "juli", "augusti"
  , "september", "oktober", "november", "december"
  ]

showMonthName :: Int -> String
showMonthName month = fromMaybe "error" (index monthNames (month - 1))

paperDecoration :: String
paperDecoration =
  -- The style breaks if we try to factor the common prefix:
  case mosaicoPaper of
    HBL -> "decoration-hbl"
    ON  -> "decoration-on"
    VN  -> "decoration-vn"
    _   -> ""

type MonthSelectionProps =
  { currentDate :: Date
  , handlers    :: Handlers
  }

type DateSelectionProps =
  { currentDate   :: Date
  , selectedMonth :: Month
  , selectedYear  :: Year
  , handlers      :: Handlers
  }

type ArticleSelectionProps =
  { selectedDate :: Date
  , articles     :: Array ArticleStub
  , handlers     :: Handlers
  }

data Props
  = MonthSelection MonthSelectionProps
  | DateSelection DateSelectionProps
  | ArticleSelection ArticleSelectionProps

component :: Component Props
component =
  React.component "Archive" $ pure <<< render

render :: Props -> JSX
render props =
  DOM.div
  { className: "px-4 lg:px-16 flex justify-center leading-6"
  , children:
    [ DOM.div
      { className: "w-full max-w-[980px]"
      , children:
        [ DOM.h1
          { className: "font-bold text-xl my-4"
          , children: [ DOM.text "Arkiv" ]
          }
        , case props of
            MonthSelection p ->
              renderMonthSelection p
            DateSelection p ->
              renderDateSelection p
            ArticleSelection p ->
              renderArticleSelection p
        ]
      }
    ]
  }

renderLink :: { href :: String, text :: String, onClick :: EventHandler } -> JSX
renderLink { href, text, onClick } = DOM.li_
  [ DOM.a
    { className: "underline decoration-2 " <> paperDecoration
    , href
    , onClick
    , children: [ DOM.text text ]
    }
  ]

renderMonthSelection :: MonthSelectionProps -> JSX
renderMonthSelection { currentDate, handlers } =
  let
    currentYear = fromEnum (year currentDate)
    years = range 2016 currentYear
    months y = case compare y currentYear of
      LT -> range 1 12
      EQ -> range 1 (fromEnum (month currentDate))
      GT -> [] -- Unreachable
    renderMonth y month =
      let url = "/arkiv/" <> show y <> "/" <> show month
      in renderLink
         { href: url
         , text: showMonthName month
         , onClick: handlers.onArchivePageClick url
         }
    renderYear y = DOM.li
      { children:
        [ DOM.h2
          { className: "text-lg font-bold mt-6 mb-2"
          , children: [ DOM.text (show y) ]
          }
        , DOM.ul
          { children: map (renderMonth y) (months y)
          }
        ]
      }
  in
    DOM.ul
    { className: "grid md:grid-cols-2 lg:grid-cols-3"
    , children: map renderYear years
    }

renderDateSelection :: DateSelectionProps -> JSX
renderDateSelection { currentDate, selectedMonth, selectedYear, handlers } =
  let
    y = fromEnum selectedYear
    m = fromEnum selectedMonth
    lastDay
      | selectedYear == year currentDate && selectedMonth == month currentDate =
          day currentDate
      | otherwise =
          lastDayOfMonth selectedYear selectedMonth
    days = range 1 (fromEnum lastDay)
    renderDay d =
      let url = "/arkiv/" <> show y <> "/" <> show m <> "/" <> show d
      in renderLink
         { href: url
         , text: show d <> "." <> show m <> "." <> show y
         , onClick: handlers.onArchivePageClick url
         }
  in
    DOM.div
    { children:
      [ DOM.h2
        { className: "text-lg font-bold mt-10 mb-2"
        , children: [ DOM.text (showMonthName m <> " " <> show y) ]
        }
      , DOM.ul
        { children: map renderDay days
        }
      ]
    }

renderArticleSelection :: ArticleSelectionProps -> JSX
renderArticleSelection { articles, selectedDate, handlers } =
  let
    y = fromEnum $ year selectedDate
    m = fromEnum $ month selectedDate
    d = fromEnum $ day selectedDate
    renderArticle article = renderLink
      { href: "/artikel/" <> article.uuid
      , text: fromMaybe article.title article.listTitle
      , onClick: handlers.onArticleClick article
      }
  in
    DOM.div
    { children:
      [ DOM.h2
        { className: "text-lg font-bold mt-10 mb-2"
        , children:
          [ DOM.text (show d <> "." <> show m <> "." <> show y) ]
        }
      , DOM.ul
        { children: map renderArticle articles
        }
      ]
    }
