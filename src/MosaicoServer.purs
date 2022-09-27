module MosaicoServer where

import Prelude

import Data.Maybe (Maybe(Nothing))
import Data.Monoid (guard)
import KSF.Paper as Paper
import Lettera.Models (ArticleStub, Category, Tag, categoriesMap)
import Mosaico.Footer (footer)
import Mosaico.Header as Header
import Mosaico.MainContent (mainContent, jumpToMainContent)
import Mosaico.Paper (mosaicoPaper)
import Mosaico.MostReadList as MostReadList
import Mosaico.LatestList as LatestList
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

type Props =
  { mainContent :: MainContent
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , categoryStructure :: Array Category
  , headless :: Boolean
  }

type MainContent =
  { content :: JSX
  , type :: MainContentType
  }

data MainContentType
  = ArticleContent
  | FrontpageContent
  | HtmlFrontpageContent
  | WebviewContent
  | TagListContent Tag
  | EpaperContent
  | StaticPageContent String
  | ProfileContent
  | MenuContent

app :: Props -> JSX
app = render

render :: Props -> JSX
render props =
  DOM.div
    { id: Paper.toString mosaicoPaper
    , children:
        [ jumpToMainContent
        , DOM.div
          { className: "grid mosaico" <> menuOpen
          , children:
              guard (not props.headless) header
              <>
              [ mainContent extraClasses [props.mainContent.content] ] <>
              guard (not props.headless)
                [ footer mosaicoPaper mempty mempty
                , case props.mainContent.type of
                    FrontpageContent -> aside
                    TagListContent _ -> aside
                    _ -> mempty
                ]
          }
        ]
    }
  where
    header =
      [ Header.topLine
      , Header.render 0
          { changeRoute: const mempty
          , categoryStructure: props.categoryStructure
          , catMap: categoriesMap props.categoryStructure
          , onCategoryClick: const mempty
          , user: Nothing
          , onLogin: mempty
          , onProfile: mempty
          , onStaticPageClick: mempty
          , onMenuClick: mempty
          , showHeading: case props.mainContent.type of
              ArticleContent -> false
              StaticPageContent _ -> false
              _ -> true
          }
      ]
    aside =
      DOM.aside
        { className: "mosaico--aside"
        , children:
          [ MostReadList.render { mostReadArticles: props.mostReadArticles, onClickHandler: const mempty }
          , LatestList.render { latestArticles: props.latestArticles, onClickHandler: const mempty }
          ]
        }
    menuOpen = case props.mainContent.type of
      MenuContent -> " menu-open"
      _           -> mempty
    extraClasses = case props.mainContent.type of
      MenuContent -> "md:[grid-column:1/span_2] lg:[grid-column:2/span_3]"
      _           -> mempty
