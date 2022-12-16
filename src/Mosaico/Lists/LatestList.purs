module Mosaico.LatestList where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import KSF.LocalDateTime (formatArticleTime)
import Lettera.Models (ArticleStub)
import Mosaico.Timestamp (timestamp)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)

type Props =
  { latestArticles :: Array ArticleStub
  , onClickHandler :: ArticleStub -> EventHandler
  }

render :: Props -> JSX
render props =
  DOM.div
    { className: "p-2 border border-gray-100 mosaico-asidelist mosaico-asidelist__latest"
    , children:
        [ DOM.h2
            { className: "my-3 mr-0 ml-2 text-lg font-bold uppercase lg:ml-3 font-roboto text-brand"
            , children: [ DOM.text "Senast publicerat" ]
            }
        , DOM.ul_ $ map renderLatestArticle props.latestArticles
        ]
    }
  where
    renderLatestArticle :: ArticleStub -> JSX
    renderLatestArticle a =
      DOM.li
        { className: "py-2 my-0 mx-2 border-b border-dotted lg:mx-3 border-b-gray-100"
        , children:
          [ DOM.a
              { onClick: props.onClickHandler a
              , href: "/artikel/" <> a.uuid
              , className: "no-underline cursor-pointer"
              , children:
                  [ DOM.div
                      { className: "list-article-liftup"
                      , children:
                          [ DOM.h3
                              { className: "text-xl leading-tight font-duplexserif"
                              , children: [ DOM.text $ fromMaybe a.title a.listTitle ]
                              }
                          , foldMap (\publishingTime ->
                              timestamp [ DOM.span_ [ DOM.text $ formatArticleTime publishingTime ] ]
                            ) $ a.publishingTime
                          , guard a.premium $ DOM.div
                              { className: "mosaico-article__meta"
                              , children:
                                  [ DOM.div
                                      { className: "premium-badge"
                                      , children: [ DOM.span_ [ DOM.text "premium" ]]
                                      }
                                  ]
                              }
                          ]
                      }
                  ]
              }
          ]
        }
