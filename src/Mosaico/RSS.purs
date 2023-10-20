module Mosaico.RSS where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import KSF.Paper as Paper
import Lettera as Lettera
import Lettera.Models (Category(..), CategoryLabel, Tag)
import Mosaico.Routes (MosaicoPage(..))
import React.Basic (JSX)
import React.Basic.DOM as DOM

categoryLink :: Paper.Paper -> Maybe CategoryLabel -> String
categoryLink paper maybeCat =
  Lettera.letteraFrontPageUrl
  <> "?paper=" <> Paper.toString paper
  <> foldMap (\cat -> "&category=" <> Lettera._encodeURIComponent (show cat)) maybeCat

tagLink :: Paper.Paper -> Tag -> String
tagLink paper t =
  Lettera.letteraTagUrl <> Lettera._encodeURIComponent (unwrap t)
  <> "?paper=" <> Paper.toString paper

rssDOM :: String -> String -> JSX
rssDOM href title =
  DOM.link
    { rel: "alternate"
    , type: "application/rss+xml"
    , title
    , href
    }

getFeed :: Paper.Paper -> MosaicoPage -> Maybe JSX
getFeed paper Frontpage =
  Just $ rssDOM (categoryLink paper Nothing) $
  Paper.paperName paper <> " RSS-flöde"
getFeed paper (CategoryPage (Category cat) _) =
  Just $ rssDOM (categoryLink paper $ Just cat.label) $
  Paper.paperName paper <> " RSS-flöde för " <> show cat.label
getFeed paper (TagPage tag _) =
  Just $ rssDOM (tagLink paper tag) $
  Paper.paperName paper <> " RSS-flöde för tagg " <> unwrap tag
getFeed _ _ = Nothing
