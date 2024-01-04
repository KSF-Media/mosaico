module Mosaico.Meta where

import Prelude

import Mosaico.Routes as Routes
import Data.Argonaut.Core as JSON
import Data.Array (null)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Maybe (maybe, fromMaybe, Maybe(..))
import Data.String as String
import Data.UUID as UUID
import Effect (Effect)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Server (renderToStaticMarkup)
import Mosaico.FallbackImage (fallbackImageShare)
import Mosaico.Paper (mosaicoPaper)
import Mosaico.RSS as RSS
import Data.Newtype (unwrap)
import KSF.Paper (Paper(..))
import KSF.Paper as Paper
import Lettera.Models (ArticleStub, Category(..))
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
      "nyhetsbrev", HBL -> Just "Här kan du beställa HBL:s nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", VN  -> Just "Här kan du beställa Västra Nylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", ON  -> Just "Här kan du beställa Östnylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "app",        HBL -> Just "Med Hufvudstadsbladets appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        VN  -> Just "Med Västra Nylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        ON  -> Just "Med Östnylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      _, _ -> Nothing

pageTitle :: Routes.MosaicoPage -> Maybe ArticleStub -> String
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
      Routes.KorsordPage -> "Korsord och hjärngympa"
      Routes.StaticPage page -> staticPageTitle page mosaicoPaper
      Routes.ArchivePage view -> case view of
        Routes.MonthSelection -> "Arkiv"
        Routes.DateSelection _ _ -> "Arkiv"
        Routes.ArticleSelection _ -> "Arkiv"
      Routes.ArticlePage articleId _
        | Just article <- maybeArticle
        , UUID.parseUUID article.uuid == Just articleId -> article.title
      -- This should never happen TODO model better
      Routes.ArticlePage _ _ -> "Laddar..."

      -- TODO: are these good?
      Routes.DraftPage _ -> Paper.paperName mosaicoPaper
      Routes.DebugPage _ -> Paper.paperName mosaicoPaper
      Routes.DeployPreview -> Paper.paperName mosaicoPaper

getMeta :: Routes.MosaicoPage -> Maybe ArticleStub -> JSX
getMeta (Routes.StaticPage page) _ = staticPageMeta page
getMeta (Routes.ArticlePage articleId _) (Just article)
  | UUID.parseUUID article.uuid == Just articleId = articleMeta article
getMeta route _ =
  fragment
    [ DOM.meta { property: "og:type", content: "website" }
    , DOM.meta { property: "og:title", content: title }
    , DOM.meta { property: "og:image", content: fallbackImageShare mosaicoPaper }
    , foldMap (\content -> DOM.meta { property: "og:description", content }) description
    , foldMap (\content -> DOM.meta { name: "description", content }) description
    , fold $ RSS.getFeed mosaicoPaper route
    ]
  where
    title = pageTitle route Nothing
    description = case route of
      Routes.KorsordPage | mosaicoPaper == HBL -> Just "Utmana dig själv med Hufvudstadsbladets digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      Routes.KorsordPage | mosaicoPaper == VN -> Just "Utmana dig själv med Västra Nylands digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      Routes.KorsordPage | mosaicoPaper == ON -> Just "Utmana dig själv med Östnylands digitala pyssel i form av korsord, sudoku och andra logiska utmaningar."
      _ -> Paper.paperDescription mosaicoPaper

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

articleMeta :: ArticleStub -> JSX
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
    , foldMap canonical articleStem
    ]
  where
    description = if null article.preamble then fold (Paper.paperDescription mosaicoPaper) else fold article.preamble
    canonical stem =
      DOM.link
        { rel: "canonical"
        , href: stem <> article.uuid
        }
    articleStem = case mosaicoPaper of
      HBL -> Just "https://www.hbl.fi/artikel/"
      VN -> Just "https://www.vastranyland.fi/artikel/"
      ON -> Just "https://www.ostnyland.fi/artikel/"
      _ -> Nothing

updateMeta :: JSX -> Effect Unit
updateMeta tags = do
    let deleteMetaByProperty prop = deleteBySelector ("[property='" <> prop <> "']")
    traverse_ deleteMetaByProperty ["og:type", "og:title", "og:url", "og:description", "og:image"]
    traverse_ deleteBySelector ["[rel='canonical']", "[name='description']", "[type='application/ld+json']", "[type='application/rss+xml']"]
    appendToHead $ renderToStaticMarkup tags
