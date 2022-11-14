module Mosaico.Article.Advertorial where

import Prelude

import Data.Foldable (foldMap, elem)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Lettera.Models (Article, Image, ArticleTypeDetails)
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.Article as Article
import Mosaico.Share as Share
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks as React
import React.Basic.Hooks (Component)

type Props =
  { article :: Article
  }

component :: Component Props
component = do
  imageComponent <- Image.component
  boxComponent <- Box.component
  React.component "Advertorial" $ \props -> React.do
    pure $ render imageComponent boxComponent props

advertorialTopBanner :: Article ->  JSX
advertorialTopBanner article =
  let getCompanyName :: ArticleTypeDetails -> Maybe String
      getCompanyName details
        | "companyName" <- details.title = details.description
        | otherwise = Nothing
      annonsText = case article.articleTypeDetails >>= getCompanyName of
            Just company -> "ANNONS: " <> String.toUpper company
            Nothing -> "ANNONS"
  in DOM.div
       { className: "advertorial-top-banner"
       , _data: Object.fromFoldable [Tuple "articletype" "advertorial"]
       , children:
           [ DOM.span
               { className: "advertorial-top-banner__company"
               , children: [ DOM.text annonsText ]
               }
           , Share.articleShareButtons article.title article.shareUrl
           ]
       }

render :: (Image.Props -> JSX) -> (Box.Props -> JSX) -> Props -> JSX
render imageComponent boxComponent { article } =
  fragment
       [ DOM.article
           { id: "BRAND-NEUTRAL"
           , className: "flex flex-col justify-center items-center mx-3 mosaico-article"
           , children:
                [ DOM.header
                    { className: "mb-4 lg:w-192"
                    , children:
                      [ Article.headline article.title
                      , Article.preamble article.preamble
                      , DOM.div
                          { className: "advertorial-badge"
                          , children:
                              [ DOM.div
                                  { className: "premium-badge"
                                  , children: [ DOM.span_ [ DOM.text "Annons" ] ]
                                  }
                              ]
                          }
                      ]
                    }
                , foldMap (imageComponent <<< imageProps fullWidth) article.mainImage
                , DOM.div
                     { className: "flex flex-col p-1 lg:w-192 mosaico-article__main"
                     , children:
                         [ DOM.div
                             { className: "mosaico-article__body"
                             , children:
                                 [ DOM.section
                                     { className: "article-content"
                                     , children: map (Article.renderElement true imageComponent boxComponent Nothing) article.body
                                     }
                                 ]
                             }
                         ]
                     }
                ]
           }
       ]
  where
    fullWidth = elem "Standard" article.categories

imageProps :: Boolean -> Image -> Image.Props
imageProps fullWidth image =
   { clickable: true
   , main: true
   , params: Nothing
   , image
   , fullWidth
   }
