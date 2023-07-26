module Mosaico.Frontpage
  ( Frontpage(..)
  , ListFrontpageProps(..)
  , MosaicoHooks
  , PrerenderedFrontpageProps(..)
  , clientHooks
  , serverHooks
  , render
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (head, null)
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import KSF.LocalDateTime (formatArticleTime)
import KSF.HtmlRenderer (render) as HtmlRenderer
import KSF.Spinner (loadingSpinner)
import Lettera.Models (ArticleStub, Tag(..))
import Mosaico.BreakingNews as BreakingNews
import Mosaico.Client.Handlers (Handlers)
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage.Models (Hook(..), toHookRep)
import Mosaico.Paper (mosaicoPaper)
import Mosaico.Tag (TagType(..), renderTag)
import Mosaico.Timestamp (timestamp)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)

-- | Represents a frontpage
data Frontpage = List ListFrontpageProps | Prerendered PrerenderedFrontpageProps

type ListFrontpageProps =
  { label :: Maybe String
  , content :: Maybe (Array ArticleStub)
  , loading :: Boolean
  , handlers :: Handlers
  }

type PrerenderedFrontpageProps =
  { content :: Maybe String
  , breakingNews :: String
  , hooks   :: Array Hook
  -- The Handlers data has this handler as well but it needs the list
  -- of articles from the caller.
  , onClick :: EventHandler
  }

premiumBadge :: JSX
premiumBadge = DOM.div { className: "premium-badge" , children: [ DOM.text "premium" ] }

render :: Frontpage -> JSX
render (List props) =
    DOM.div
        { className: "mx-4 mosaico--article-list lg:mx-0"
        , children:
          [ maybeLabel props.label
          , genericRender (\list ->
              map renderListArticle list
              <> [if props.loading then loadingSpinner else mempty]
              <> [if not $ null list then moreButton props.handlers.getMore else mempty]
              )
            -- No need for top level click handler for this mode since
            -- we can easily put it on the individual article link.
            mempty
            props.content
          ]
        }
      where
        -- Only OC images use this
        addCrop url =
          if contains (Pattern "smooth-storage") url then url
          else url <> "&function=hardcrop&width=200&height=200&q=90"
        tagLink a = foldMap (renderTag Main props.handlers.onTagClick) $ head a.tags

        articleTitle a = [ DOM.h3
                             { className: "text-xl leading-tight text-aptoma-text-color font-duplexserif"
                             , children: [ DOM.text $ fromMaybe a.title a.listTitle ]
                             }
                         , DOM.div
                             { className: "mt-[2px]"
                             , children:
                                 [ foldMap (\publishingTime ->
                                       timestamp [ DOM.span_ [ DOM.text $ formatArticleTime publishingTime ] ]
                                   ) $ a.publishingTime
                                 , guard a.premium $
                                   DOM.div
                                     { className: "mosaico-article__meta"
                                     , children: [ premiumBadge ]
                                     }
                                 ]
                             }
                         ]

        listArticleImage a =
          let img = a.listImage <|> a.mainImage
              src = maybe (fallbackImage mosaicoPaper) (addCrop <<< _.tinyThumb) img
              alt = fold $ _.caption =<< img
          in  DOM.img
                { src
                , alt
                , className: "object-cover mt-6 w-20 h-20 md:w-28 md:h-28"
                }

        renderListArticle :: ArticleStub -> JSX
        renderListArticle a =
          DOM.article
            { className: "flex relative justify-between mb-5 cursor-pointer"
            , _data: Object.fromFoldable $
              -- Known bug, exclude from tests
              if null a.tags || Just true == (contains (Pattern ":") <<< un Tag <$> head a.tags) then []
              else [ Tuple "premium" $ if a.premium then "1" else "0"
                    , Tuple "uuid" $ a.uuid
                    ]
            , children:
                [ DOM.div
                    { className: "mr-4 break-words"
                    , children:
                        [ tagLink a
                        , DOM.a
                            { href: "/artikel/" <> a.uuid
                            , onClick: props.handlers.onArticleClick a
                            , children: articleTitle a
                            -- these ::after styles will be on top of everything except the tag links,
                            -- making everything clickable
                            , className: "no-underline after:w-full after:h-full after:absolute after:top-0 after:left-0 after:z-10"
                            }
                        ]
                    }
                , listArticleImage a
                ]
            }

render (Prerendered props) = genericRender
  (\content ->
    [ BreakingNews.render
        { content: props.breakingNews
        , currentArticleId: Nothing
        }
    , HtmlRenderer.render
        { content
        , hooks: Just $ toHookRep <$> props.hooks
        }
    ]
  )
  props.onClick
  props.content

moreButton :: Effect Unit -> JSX
moreButton getMore =
  DOM.button
    { children: [ DOM.text "Fler resultat" ]
    , className: "button-green"
    , onClick: capture_ getMore
    }

maybeLabel :: Maybe String -> JSX
maybeLabel categoryLabel =
  case categoryLabel of
    Just label -> DOM.h2
                    { className: "inline-block mb-8 text-3xl font-bold leading-none border-b-2 font-roboto border-brand"
                    , children: [ DOM.text label ]
                    }
    _          -> mempty

genericRender :: forall a. (a -> Array JSX) -> EventHandler -> Maybe a -> JSX
genericRender f onClick content = DOM.div
  { className: "mosaico-main"
  , children: maybe [loadingSpinner] f content
  , onClick
  }

type MosaicoHooks = (ArticleStub -> EventHandler) -> Array ArticleStub -> Array ArticleStub -> Array Hook

clientHooks :: MosaicoHooks
clientHooks onArticleClick mostRead latest =
  serverHooks onArticleClick mostRead latest <>
  [ Ad "Box Ad 1 DESKTOP"     "mosaico-ad__box1"
  , Ad "Box Ad 2 DESKTOP"     "mosaico-ad__box2"
  , Ad "Box Ad 3 DESKTOP"     "mosaico-ad__box3"
  , Ad "Box Ad 4 DESKTOP"     "mosaico-ad__box4"
  , Ad "Box Ad 5 DESKTOP"     "mosaico-ad__box5"
  , Ad "Ad 1"                 "mosaico-ad__bigbox1"
  , Ad "Ad 2"                 "mosaico-ad__bigbox2"
  , Ad "Ad MOBILE - mobparad" "mosaico-ad__mobparad"
  , Ad "Box Ad 1 MOBILE"      "mosaico-ad__mobbox1"
  , Ad "Box Ad 2 MOBILE"      "mosaico-ad__mobbox2"
  , Ad "Box Ad 3 MOBILE"      "mosaico-ad__mobbox3"
  ]

serverHooks :: MosaicoHooks
serverHooks onArticleClick mostRead latest =
  [ RemoveTooltips
  , MostRead mostRead onArticleClick
  , Latest latest onArticleClick
  , ArticleUrltoRelative
  , EpaperBanner
  ]
