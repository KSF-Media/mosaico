module Mosaico.BreakingNews
  ( render
  )
  where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop, take)
import Foreign.Object (singleton)
import KSF.HtmlRenderer as HtmlRenderer
import KSF.HtmlRenderer.Models as HtmlRenderer
import React.Basic (JSX)
import React.Basic.DOM as DOM

type BreakingNewsProps =
  { currentArticleId :: Maybe String
  , content :: String
  }

linkToCurrentArticle :: String -> HtmlRenderer.Node -> Boolean
linkToCurrentArticle uuid node =
  let info = { tag:_, href:_ }
        <$> HtmlRenderer.getName node
        <*> HtmlRenderer.getStringAttrib "href" node
  in case info of
    Just { tag, href } -> tag == "a" && drop 9 href == uuid
    _ -> false

removeBreakingNewsItemHook :: String -> HtmlRenderer.HookRep
removeBreakingNewsItemHook currentArticleId = HtmlRenderer.replacingHook
  { shouldProcessNode: \n ->
     let info = { tag:_, className:_ }
           <$> HtmlRenderer.getName n
           <*> HtmlRenderer.getStringAttrib "class" n
         check { tag, className } = tag == "div"
           && take 10 className == "dre-group "
           && HtmlRenderer.dfs (linkToCurrentArticle currentArticleId) n
      in maybe false check info
  , processNode: \_ _ _ -> pure mempty
  }

render :: BreakingNewsProps -> JSX
render props =
  DOM.div
    { className: "mosaico--breaking-news"
    , _data: singleton "nosnippet" "true"
    , children:
        [ HtmlRenderer.render
            { content: props.content
            , hooks
            }
        ]
    }
  where
    hooks = pure <<< removeBreakingNewsItemHook <$> props.currentArticleId
