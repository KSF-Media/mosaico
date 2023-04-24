module Mosaico.Article where

import Prelude

import Bottega.Models.Order (OrderSource(..))
import Control.Alt ((<|>))
import Data.Array (cons, head, insertAt, length, null, snoc, take, (!!))
import Data.Either (Either(..), either, hush)
import Data.Foldable (fold, foldMap)
import Data.List (fromFoldable, (:))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object as Object
import KSF.LocalDateTime (formatArticleTime)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import KSF.Spinner (loadingSpinner)
import KSF.User (User)
import KSF.Vetrina as Vetrina
import KSF.Vetrina.Products.Premium (hblPremium, vnPremium, onPremium)
import Lettera.Models (Article, ArticleStub, ArticleType(..), Author, BodyElement(..), ExternalScript, Image, MosaicoArticleType(..), Tag, FullArticle)
import Mosaico.Ad (ad) as Mosaico
import Mosaico.Article.Box as Box
import Mosaico.Article.Image as Image
import Mosaico.BreakingNews as BreakingNews
import Mosaico.Eval as Eval
import Mosaico.FallbackImage (fallbackImage)
import Mosaico.Frontpage (Frontpage(..), render) as Frontpage
import Mosaico.LatestList as LatestList
import Mosaico.Share as Share
import Mosaico.Tag (renderTag)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import Data.List.Types (List(..))

isPremium :: Either ArticleStub FullArticle -> Boolean
isPremium = either _.premium _.article.premium

getTags :: Either ArticleStub FullArticle -> Array Tag
getTags = either _.tags _.article.tags

getTitle :: Either ArticleStub FullArticle -> String
getTitle = either _.title _.article.title

getMainImage :: Either ArticleStub FullArticle -> Maybe Image
getMainImage = either _.mainImage _.article.mainImage

getPreamble :: Either ArticleStub FullArticle -> Array String
getPreamble = either _.preamble _.article.preamble

getBody :: Either ArticleStub FullArticle -> Array BodyElement
getBody = either (const mempty) _.article.body

getRemoveAds :: Either ArticleStub FullArticle -> Boolean
getRemoveAds = either _.removeAds _.article.removeAds

getShareUrl :: Either ArticleStub FullArticle -> Maybe String
getShareUrl = either _.shareUrl _.article.shareUrl

getExternalScripts :: Either ArticleStub FullArticle -> Array ExternalScript
getExternalScripts props = fromMaybe [] $ either (const mempty) _.article.externalScripts props

getArticleCategories :: Either ArticleStub FullArticle -> Array String
getArticleCategories = either (const mempty) _.article.categories

type Props =
  { paper :: Paper
  , article :: Either ArticleStub FullArticle
  , onLogin :: EventHandler
  , onPaywallEvent :: Effect Unit
  , onTagClick :: Tag -> EventHandler
  , onArticleClick :: ArticleStub -> EventHandler
  , onAuthorClick :: Author -> EventHandler
  , user :: Maybe (Maybe User)
  , mostReadArticles :: Array ArticleStub
  , latestArticles :: Array ArticleStub
  , advertorial :: Maybe ArticleStub
  , breakingNews :: String
  }

evalEmbeds :: Article -> Effect Unit
evalEmbeds = Eval.evalArticleScripts <<< map Eval.ScriptTag <<< map unwrap <<< fold <<< _.externalScripts

component :: Component Props
component = do
  imageComponent <- Image.component
  boxComponent <- Box.component
  nagbarComponent <- Eval.embedNagbar
  React.component "Article" $ \props -> React.do
    pure $ render nagbarComponent imageComponent boxComponent props

type BodyElement' = Tuple BodyElement Boolean

render :: (Eval.Props -> JSX) -> (Image.Props -> JSX) -> (Box.Props -> JSX) -> Props -> JSX
render embedNagbar imageComponent boxComponent props =
    let title = getTitle props.article
        tags = getTags props.article
        mainImage = getMainImage props.article
        renderElem = renderElement hideAds imageComponent boxComponent (Just props.onArticleClick)

        markPremiumElements :: Boolean -> Array BodyElement -> Array BodyElement'
        markPremiumElements premium elements =
            if premium
            then run [] elementCount (fromFoldable elements)
            else map (\el -> Tuple el false) elements
          where
            -- These rules should be kept in sync with Lettera's preview rendering.
            elementCount = if length elements < 5 then length elements / 2 else 3
            run :: Array BodyElement' -> Int -> List BodyElement -> Array BodyElement'
            run arr _ Nil = arr
            run arr 0 (x:xs) = run (snoc arr $ Tuple x true) 0 xs
            run arr n (x@(Html _):xs) = run (snoc arr $ Tuple x false) (n-1) xs
            --XXX: Paragraph is an enum variant in Lettera but not in Mosaico?
            --run arr n (x@(Paragraph _):xs) = run (snoc arr $ Tuple x false) (n-1) xs
            run arr n (x:xs) = run (snoc arr $ Tuple x true) n xs
        body = getBody props.article
        hideAds = getRemoveAds props.article
        bodyWithoutAd = map renderElem (markPremiumElements false body)
        bodyWithAd =
          [ Mosaico.ad { contentUnit: "mosaico-ad__mobparad", inBody: false, hideAds }
          , DOM.section
            { className: "article-content"
            , children: map renderElem $ insertAdsIntoBodyText "mosaico-ad__bigbox1" "mosaico-ad__bigbox2" $ markPremiumElements (isPremium props.article) body
            }
          ]
        advertorial = if hideAds then mempty else foldMap renderAdvertorialTeaser props.advertorial
        mostRead = foldMap renderMostReadArticles $
          if null props.mostReadArticles then Nothing else Just $ take 5 props.mostReadArticles
        shareUrl = getShareUrl props.article
        embedScripts = getExternalScripts props.article
        articleCategory = head $ getArticleCategories props.article

    in DOM.article
      { className: "md:flex md:justify-center mx-3 mosaico-article" <> (guard (isErrorArticle props) $ " mosaico-article-error")
      , _data: Object.fromFoldable $ Tuple "category" <$> (articleCategory <|> Just "")
      , children:
        [ DOM.div {className: "flex flex-col items-center lg:w-240"
        , children:
          [ DOM.div {className: "lg:w-192", children: [BreakingNews.render { content: props.breakingNews }]}
          , DOM.header
              { className: "mb-4 lg:w-192"
              , children:
                [ headline title
                , preamble $ getPreamble props.article
                -- We don't want to be able to share error articles
                , guard (maybe true ((_ /= ErrorArticle) <<< _.articleType) $ hush props.article)
                    tagAndShareButtons tags props.onTagClick (isPremium props.article) title shareUrl
                ]
              }
          , foldMap
              (\image -> imageComponent
                -- TODO: make error image unclickable
                { clickable: true
                , main: true
                , params: Just "&width=960&height=540&q=90"
                , image
                , fullWidth: false
                })
              mainImage
              , guard (not $ isErrorArticle props) $ DOM.div
                  { className: "flex flex-col self-end max-w-full md:flex-row lg:w-216"
                  , children:
                    [ DOM.div
                        { className: "flex flex-col p-1 max-w-full mosaico-article__main"
                        , children:
                            [ foldMap (renderMetabyline props.onAuthorClick <<< _.article) $ hush props.article
                            , DOM.div
                                { className: "mosaico-article__body"
                                , children:
                                  [ if not $ null embedScripts then embedNagbar { isArticle: true } else mempty ]
                                  <>
                                  case _.articleType <$> props.article of
                                    Right PreviewArticle ->
                                      renderElem (Tuple (Ad { contentUnit: "mosaico-ad__mobparad", inBody: false }) false)
                                      `cons` bodyWithoutAd
                                      `snoc` (if isNothing props.user then loadingSpinner else vetrina)
                                      `snoc` renderElem (Tuple (Ad { contentUnit: "mosaico-ad__bigbox1", inBody: false }) false)
                                      `snoc` advertorial
                                      `snoc` mostRead
                                    Right DraftArticle ->
                                      bodyWithoutAd
                                    Right FullArticle ->
                                      bodyWithAd
                                      `snoc` DOM.div { id: "tb-embed" }
                                      `snoc` advertorial
                                      `snoc` mostRead
                                    Left _ -> [ loadingSpinner ]
                                    _ -> mempty
                                }]}
                    , DOM.div
                        { className: "md:w-72 mosaico-article__aside shrink-0"
                        , children:
                        -- Race conditions a problem when detecting screen width here,
                        -- so both mobile and desktop ad slots are listed here.
                        -- These then get filtered down to mobile-only or desktop-only
                        -- slots in the Mosaico.ad code.
                          [ Mosaico.ad { contentUnit: "mosaico-ad__mobbox1", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box1", inBody: false, hideAds }
                          , LatestList.render
                                     { latestArticles: props.latestArticles
                                     , onClickHandler: props.onArticleClick
                                     }
                          , Mosaico.ad { contentUnit: "mosaico-ad__mobbox2", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box2", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__mobbox3", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box3", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box4", inBody: false, hideAds }
                          , Mosaico.ad { contentUnit: "mosaico-ad__box5", inBody: false, hideAds }
                          ]
                        }
                    ]
              }
          ]}]
    }
  where
    renderMetabyline :: (Author -> EventHandler) -> Article -> JSX
    renderMetabyline onAuthorClick article =
      DOM.div
        { className: "[grid-area:articlemetabyline] border-solid border-y mb-6 border-gray-100 text-gray-500 font-roboto md:mr-8"
        , children:
            [ DOM.div
                { className: "py-2 md:py-3"
                , children:
                    map
                        (\author -> DOM.div
                          { className: "mb-1 text-sm font-medium text-aptoma-text-color dark:text-aptoma-white"
                          , children: [ guard (article.articleType == Opinion) $
                                        renderOpinionType article.articleTypeDetails
                                      , DOM.a
                                          { className: "inline-block mr-2"
                                          , href: "/sök?q=" <> author.byline
                                          , onClick: onAuthorClick author
                                          , children: [ DOM.text author.byline ]
                                          }
                                      , foldMap
                                        (\authorEmail -> DOM.span
                                                        { className: "inline-block italic font-light text-gray-500"
                                                        , children: [ DOM.text authorEmail ]
                                                        }
                                        )
                                        author.email
                                      ]
                          })
                        article.authors
                    <> [foldMap
                        (\publishingTime -> DOM.div
                          { className: "mt-2 text-xs"
                          , children:
                              [ DOM.span_ [ DOM.text publishingTime ]
                              , foldMap
                                (\updateTime -> DOM.span_
                                  [ DOM.text $ " UPPDATERAD " <> updateTime]
                                )
                                ((\x -> guard (x /= publishingTime) $ Just x) =<<
                                 formatArticleTime <$> article.updateTime)
                              ]
                          })
                        (formatArticleTime <$> article.publishingTime)
                    ]
                }
            ]
        }

    isErrorArticle :: Props -> Boolean
    isErrorArticle props' = case props'.article of
                        Right { articleType: ErrorArticle } -> true
                        _                                   -> false

    renderOpinionType detail =
      foldMap (\opiniontype -> DOM.span
                              { className: "inline-block mr-2 text-base font-bold text-gray-900 uppercase font-duplexserif dark:text-aptoma-white"
                              , children: [ DOM.text opiniontype ]
                              }) $ _.title <$> detail

    vetrina =
      Vetrina.vetrina
        { onClose: Just props.onPaywallEvent
        , onLogin: props.onLogin
        , user: join props.user
        , products: Right case props.paper of
            HBL -> [ hblPremium ]
            ON -> [ onPremium ]
            VN -> [ vnPremium ]
            _ -> []
        , unexpectedError: mempty
        , headline: Just paperHeadline
        , paper: Just props.paper
        , paymentMethods: []
        , customNewPurchase: Nothing
        , subscriptionExists: mempty
        , loadingContainer: Nothing
        , accessEntitlements: Set.fromFoldable case props.paper of
            HBL -> ["hbl-365", "hbl-web"]
            ON -> ["on-365", "on-web"]
            VN -> ["vn-365", "vn-web"]
            _ -> []
        , orderSource: PaywallSource
        , askAccountAlways: false
        }

    paperName = case props.paper of
      HBL -> "HBL"
      p -> Paper.paperName p

    paperHeadline = case props.paper of
      HBL -> DOM.div_
              [ DOM.text $ "HBL digitalt "
              , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "nu 0 €" ] }
              ]
      _   -> DOM.div_
              [ DOM.text $ "Läs " <> paperName <> " digitalt för "
              , DOM.span { className: "vetrina--price-headline", children: [ DOM.text "endast 1 €" ] }
              ]

    renderMostReadArticles articles =
      DOM.div
        { className: "mt-20 mr-0 mb-8 ml-4 text-lg font-bold uppercase md:mx-0 bold font-roboto text-brand"
        , children: [ DOM.h2_ [DOM.text "ANDRA LÄSER" ]]
        } <>
      (Frontpage.render $ Frontpage.List
        { label: mempty
        , content: Just articles
        , loadMore: mempty
        , loading: false
        , onArticleClick: props.onArticleClick
        , onTagClick: props.onTagClick
        })

    renderAdvertorialTeaser :: ArticleStub -> JSX
    renderAdvertorialTeaser article =
      let img = article.listImage <|> article.mainImage
          imgSrc = maybe (fallbackImage props.paper) _.thumb img
          alt = fromMaybe "" $ _.caption =<< img
      in
        DOM.a
          { className: "block p-3 text-black no-underline bg-advertorial dark:text-aptoma-white"
          , href: "/artikel/" <> article.uuid
          , onClick: props.onArticleClick article
          , children:
              [ DOM.div
                  { className: "pt-1 pb-2 text-xs font-bold font-duplexserif"
                  , children: case article.articleTypeDetails of
                        Just { title: "companyName", description: Just company } ->
                          [ DOM.span
                              { className: "mr-1 text-gray-500 font-roboto dark:text-aptoma-white"
                              , children: [ DOM.text "ANNONS: " ]
                              }
                          , DOM.span
                            { className: "mr-1 text-black font-duplexserif dark:text-aptoma-white"
                            , children: [ DOM.text $ toUpper company ]
                            }
                          ]
                        _ ->
                          [ DOM.span
                              { className: "mr-1 text-gray-500 font-roboto dark:text-aptoma-white"
                              , children: [ DOM.text "ANNONS" ]
                              }
                          ]
                  }
              , DOM.div
                  { className: "flex overflow-y-hidden items-center w-full max-h-96"
                  , children:
                      [ DOM.img
                          { className: "overflow-y-hidden w-auto max-w-full"
                          , src: imgSrc
                          , alt
                          }
                      ]
                  }
              , DOM.h2
                  { className: "mt-3 text-3xl font-semibold break-words font-robotoslab"
                  , children: [ DOM.text $ fromMaybe article.title article.listTitle ]
                  }
              ]
          }


    insertAdsIntoBodyText :: String -> String -> Array BodyElement' -> Array BodyElement'
    insertAdsIntoBodyText cu1 cu2 body =
      if elements > 15
        then fromMaybe body $ do
          bodyWithAd <- insertAt (findAdSpace body $ elements/3) (Tuple (Ad { contentUnit: cu1, inBody: true }) false) body
          insertAt (findAdSpace bodyWithAd $ 2 * elements/3) (Tuple (Ad { contentUnit: cu2, inBody: true }) false) bodyWithAd
        else if elements > 6
          then fromMaybe body $ insertAt (findAdSpace body $ elements/2) (Tuple (Ad { contentUnit: cu1, inBody: true }) false) body
          else body `snoc` (Tuple (Ad { contentUnit: cu1, inBody: false }) false)
      where
        elements = length body
        findAdSpace :: Array BodyElement' -> Int -> Int
        findAdSpace body' i
          | i > elements = elements
          | Just (Tuple (Html _) _) <- body' !! (i-1)
          , Just (Tuple (Html _) _) <- body' !! i
          = i
          | otherwise = findAdSpace body' (i+1)

headline :: String -> JSX
headline title =
  DOM.h1
    { className: "mosaico-article__headline"
    , children: [ DOM.text title ]
    }

preamble :: Array String -> JSX
preamble texts =
  DOM.section
    { className: "mosaico-article__preamble"
    , children: map (DOM.p_ <<< pure <<< DOM.text) texts
    }

tagAndShareButtons :: Array Tag -> (Tag -> EventHandler) -> Boolean -> String -> Maybe String -> JSX
tagAndShareButtons tags onTagClick premium title shareUrl =
  DOM.section
    { className: "mosaico-article__tag-n-share"
    , children:
        [ DOM.div
            { className: "mosaico-article__tag-n-premium"
            , children:
                [ foldMap (renderTag onTagClick) $ head tags
                , guard premium $ DOM.div
                    { className: "premium-badge"
                    , children: [ DOM.span_ [ DOM.text "Premium" ]]
                    }
                ]
            }
        , Share.articleShareButtons title shareUrl
        ]
    }

-- TODO: maybe we don't want to deal at all with the error cases
-- and we want to throw them away?
renderElement :: Boolean -> (Image.Props -> JSX) -> (Box.Props -> JSX) -> Maybe (ArticleStub -> EventHandler) -> (Tuple BodyElement Boolean) -> JSX
renderElement hideAds imageComponent boxComponent onArticleClick (Tuple el premiumOnly) = case el of
  -- Can't place div's or blockquotes under p's, so place them under div.
  -- This is usually case with embeds
  Html content -> DOM.div
    { dangerouslySetInnerHTML: { __html: content }
    , className: "article-element article-element__html" <> premiumClass
    }
  Headline str -> DOM.h2
    { className: "article-element article-element__subheadline" <> premiumClass
    , children: [ DOM.text str ]
    }
  Image image -> imageComponent
      { clickable: true
      , main: false
      , params: Just "&width=640&q=90"
      , image
      , fullWidth: false
      }
  Box boxData ->
    DOM.div
      { className: "article-element" <> boxCSSType <> premiumClass
      , children:
          [ boxComponent
              { headline: boxData.headline
              , title: boxData.title
              , content: boxData.content
              , expanded: boxData.type == "review" || Box.autoExpand boxData.content
              }
          ]
      }
    where
      boxCSSType = case boxData.type of
        "review" -> " article-element__reviewbox"
        _        -> " article-element__factbox"
  Footnote footnote -> DOM.p
      { className: "article-element article-element__footnote" <> premiumClass
      , dangerouslySetInnerHTML: { __html: footnote }
      }
  Quote { body, author } -> DOM.figure
      { className: "article-element article-element__quote" <> premiumClass
      , children:
          [ DOM.blockquote_ [ DOM.text body ]
          , foldMap (DOM.figcaption_ <<< pure <<< DOM.text) author
          ]
      }
  Question question -> DOM.p
      { className: "article-element article-element__question" <> premiumClass
      , dangerouslySetInnerHTML: { __html: question }
      }
  Related related -> DOM.figure
      { className: "article-element article-element__related" <> premiumClass
      , children:
          [ DOM.ul_ $ map renderRelatedArticle related
          ]
      }
  Ad { contentUnit, inBody} -> Mosaico.ad { contentUnit, inBody, hideAds }
  where
    -- This class is used to tell Google that these elements can only
    -- be viewed with an active subscription. It doesn't apply any styling.
    premiumClass = if premiumOnly then " premium-only" else ""
    renderRelatedArticle article =
      DOM.li_
        [ DOM.a
            { href: "/artikel/" <> article.uuid
            , children: [ DOM.text article.title ]
            , onClick: foldMap (\f -> f article) onArticleClick
            }
        ]
