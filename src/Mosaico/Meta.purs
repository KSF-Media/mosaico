module Mosaico.Meta where

import Prelude

import Mosaico.Routes as Routes
import Data.Argonaut.Core as JSON
import Data.Tuple.Nested ((/\))
import Data.Array (null)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Either (Either(..))
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Data.String as String
import Effect (Effect)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Server (renderToStaticMarkup)
import Mosaico.FallbackImage (fallbackImageShare)
import Mosaico.Paper (mosaicoPaper)
import Data.Newtype (unwrap)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import Lettera.Models (Article, FullArticle, Category(..))
import Lettera.ArticleSchema (renderAsJsonLd)

foreign import deleteBySelector :: String -> Effect Unit
foreign import appendToHead :: String -> Effect Unit

staticPageTitle :: String -> Paper -> String
staticPageTitle page paper =
  case page, paper of
    "anslagstavlan", _   -> "Anslagstavlan"
    "bruksvillkor", _    -> "Bruksvillkor"
    "fiskecupen", _      -> "Fiskecupen"
    "fragor-och-svar", _ -> "Frågor och svar"
    "insandare", _       -> "Insändare"
    "kontakt", _         -> "Kontakta oss"
    "korsord", _         -> "Korsord och hjärngympa"
    "kundservice", _     -> "Kundservice"
    "nyhetsbrev", HBL    -> "Beställ HBL:s nyhetsbrev!"
    "nyhetsbrev", ON     -> "Beställ Östnylands nyhetsbrev!"
    "nyhetsbrev", VN     -> "Beställ Västra Nylands nyhetsbrev!"
    "tipsa-oss", _       -> "Tipsa oss"
    "app", HBL           -> "Hufvudstadsbladets appar för Iphone, Ipad och Android"
    "app", VN            -> "Västra Nylands appar för Iphone, Ipad och Android"
    "app", ON            -> "Östnylands appar för Iphone, Ipad och Android"
    _, _                 -> Paper.paperName paper

staticPageDescription :: String -> Paper -> Maybe String
staticPageDescription page paper =
    case page, paper of
      "korsord",    HBL -> Just "Utmana dig själv med Hufvudstadsbladets digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      "korsord",    VN  -> Just "Utmana dig själv med Västra Nylands digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      "korsord",    ON  -> Just "Utmana dig själv med Östnylands digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      "nyhetsbrev", HBL -> Just "Här kan du beställa HBL:s nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", VN  -> Just "Här kan du beställa Västra Nylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", ON  -> Just "Här kan du beställa Östnylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "app",        HBL -> Just "Med Hufvudstadsbladets appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        VN  -> Just "Med Västra Nylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        ON  -> Just "Med Östnylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      _, _ -> Nothing

pageTitle :: forall a. Routes.MosaicoPage -> Maybe (Either a FullArticle) -> String
pageTitle route maybeArticle =
    case route of
      Routes.Frontpage -> Paper.paperName mosaicoPaper
      Routes.TagPage tag _ -> unwrap tag
      Routes.SearchPage _ _ -> "Sök"
      Routes.ProfilePage -> "Min profil"
      Routes.MenuPage -> "Meny"
      Routes.NotFoundPage _ -> "Oj... 404"
      Routes.CategoryPage (Category c) _ -> unwrap c.label
      Routes.EpaperPage -> "E-Tidningen"
      Routes.StaticPage page -> staticPageTitle page mosaicoPaper
      Routes.ArticlePage _ -> case maybeArticle of
        Just (Right article) -> article.article.title
        Just (Left _) -> "Något gick fel"
        Nothing -> "Laddar..."

      -- TODO: are these good?
      Routes.DraftPage -> Paper.paperName mosaicoPaper
      Routes.DebugPage _ -> Paper.paperName mosaicoPaper
      Routes.DeployPreview -> Paper.paperName mosaicoPaper

getMeta :: forall a. Routes.MosaicoPage -> Maybe (Either a FullArticle) -> JSX
getMeta route maybeArticle =
    case route /\ maybeArticle of
      Routes.StaticPage page /\ _ -> staticPageMeta page
      Routes.ArticlePage _ /\ (Just (Right article)) -> articleMeta article.article
      _ -> defaultMeta Nothing

defaultMeta :: Maybe String -> JSX
defaultMeta overrideTitle =
  fragment
    [ DOM.meta { property: "og:type", content: "website" }
    , DOM.meta { property: "og:title", content: title }
    , DOM.meta { property: "og:image", content: fallbackImageShare mosaicoPaper }
    , foldMap (\content -> DOM.meta { property: "og:description", content }) description
    , foldMap (\content -> DOM.meta { name: "description", content }) description
    ]
  where
    title = fromMaybe (Paper.paperName mosaicoPaper) overrideTitle
    description = Paper.paperDescription mosaicoPaper

staticPageMeta :: String -> JSX
staticPageMeta pageName =
  fragment
    [ DOM.meta { property: "og:type", content: "website" }
    , DOM.meta { property: "og:title", content: title }
    , DOM.meta { property: "og:image", content: fallbackImageShare mosaicoPaper }
    , foldMap (\content -> DOM.meta { property: "og:description", content }) description
    , foldMap (\content -> DOM.meta { name: "description", content }) description
    ]
    where
        title = staticPageTitle pageName mosaicoPaper
        description = staticPageDescription pageName mosaicoPaper

articleMeta :: Article -> JSX
articleMeta article =
  fragment
    [ DOM.meta { property: "og:type", content: "article" }
    , DOM.meta { property: "og:title", content: article.title }
    , DOM.meta { property: "og:url", content: fromMaybe "" article.shareUrl }
    , DOM.meta { property: "og:description", content: description }
    , DOM.meta { property: "og:image", content: maybe (fallbackImageShare mosaicoPaper) _.url article.mainImage }
    , DOM.meta { name: "description", content: description }
    , foldMap (const $ DOM.meta { name: "robots", content: "max-image-preview:large"}) article.mainImage
    , DOM.script
        { type: "application/ld+json"
        , dangerouslySetInnerHTML:
            { __html:
                String.replaceAll (String.Pattern "<") (String.Replacement "\\u003c")
                  $ JSON.stringify
                  $ renderAsJsonLd article
            }
        }
    ]
  where
    description = if null article.preamble then fold (Paper.paperDescription mosaicoPaper) else fold article.preamble

updateMeta :: JSX -> Effect Unit
updateMeta tags = do
    let deleteMetaByProperty prop = deleteBySelector ("[property='" <> prop <> "']")
    traverse_ deleteMetaByProperty ["og:type", "og:title", "og:url", "og:description", "og:image"]
    deleteBySelector "[name='description']"
    deleteBySelector "[type='application/ld+json']"
    appendToHead $ renderToStaticMarkup tags
