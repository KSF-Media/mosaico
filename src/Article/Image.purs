module Mosaico.Article.Image where

import Prelude

import Data.Array (head, last)
import Data.Foldable (fold)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Lettera.Models (Image)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks as React
import React.Basic.Hooks (Component, useState, (/\))
import Partial.Unsafe (unsafeCrashWith)


type Props =
  { clickable :: Boolean
  , main      :: Boolean
    -- Params are only applied on imengine images
  , params    :: Maybe String
  , image     :: Image
  , fullWidth :: Boolean
  }

component :: Component Props
component = do
  React.component "Image" $ \props -> React.do
    opened /\ setOpened <- useState false
    let toggleOpen = capture_ $ setOpened not
        openedImage = if opened then articleImageFullScreen toggleOpen props else mempty
    pure $ (render toggleOpen props) <> openedImage

render :: EventHandler -> Props -> JSX
render onClick props =
  if props.main then articleMainImage onClick props else articleImage onClick props

articleImage :: EventHandler -> Props -> JSX
articleImage onClick props@{ image: img } =
  DOM.div
    { className: "mosaico-article__image"
    , children:
        [ DOM.img
            { src: url props
            , title: fold img.caption
            }
        , renderCaption img.byline img.caption
        ]
    , onClick
    }

articleMainImage :: EventHandler -> Props -> JSX
articleMainImage onClick props@{ image: img } =
  DOM.div
    { className: "mosaico-article__main-image" <> guard props.fullWidth " full-width"
    , children:
        [ DOM.div
            { className: "wrapper"
            , children:
                [ DOM.img
                    { src: url props
                    , title: fold img.caption
                    }
                ]
            }
        , renderCaption img.byline img.caption
        ]
    , onClick
    }

url :: Props -> String
url props =
  props.image.url <>
  if String.take 16 props.image.url == "https://imengine" then fold props.params else ""

isSvg :: String -> Boolean
isSvg fileUrl =
  let splitRegex = case Regex.regex "[#?]" Flags.noFlags of
                      Right r -> r
                      Left msg -> unsafeCrashWith msg -- absurd
      urlPart = fromMaybe "" $ head $ Regex.split splitRegex fileUrl
      extension = String.trim $ fromMaybe "" $ last (String.split (String.Pattern ".") urlPart)
  in extension == "svg"

renderCaption :: Maybe String -> Maybe String -> JSX
renderCaption Nothing Nothing = mempty
renderCaption byline caption =
  DOM.div
    { className: "caption mosaico-article__main-image__caption"
    , children:
        [ DOM.span_ [DOM.text $ fold caption]
        , DOM.span
            { className: "byline"
            , children: [ DOM.text $ fold byline ]
            }
        ]
    }

articleImageFullScreen :: EventHandler -> Props -> JSX
articleImageFullScreen onClick props =
  DOM.div
    { className: "mosaico-article__focus-image"
    , children:
        [ DOM.div
            { className: "wrapper" <> guard (isSvg $ props.image.url) " wrapper-svg"
            , children:
                [ DOM.img
                    { src: props.image.url <> "&width=1600&height=1065&q=75"
                    }
                -- this close button does nothing, clicking anywhere on the image closes it
                , DOM.i { className: "mosaico-article__focus-image__close-button" }
                , renderCaption props.image.byline props.image.caption
                ]
            }
        ]
    , onClick
    }
