module Mosaico.Lists.MostReadList where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Monoid (guard)
import Lettera.Models (ArticleStub)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)


type Props =
  { mostReadArticles :: Array ArticleStub
  , onArticleClick :: ArticleStub -> EventHandler
  }

render :: Props -> JSX
render props =
  DOM.div
    { className: "mosaico-asidelist mosaico-asidelist__mostread"
    , children:
        [ DOM.h2
            { className: "text-lg font-bold uppercase font-roboto text-brand mosaico-asidelist--header"
            , children: [ DOM.text "Andra läser" ]
            }
        , DOM.ul_ $ map renderMostreadArticle props.mostReadArticles
        ]
    }
  where
    renderMostreadArticle :: ArticleStub -> JSX
    renderMostreadArticle a =
      DOM.li_
        [ DOM.a
            { onClick: props.onArticleClick a
            , href: "/artikel/" <> a.uuid
            , children:
                [ DOM.div
                    { className: "counter"
                    , children: [ DOM.div_ [] ]
                    }
                , DOM.div
                    { className: "list-article-liftup"
                    , children:
                        [ DOM.h3
                            { className: "text-xl leading-tight font-duplexserif"
                            , children: [ DOM.text $ fromMaybe a.title a.listTitle ]
                            }
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
