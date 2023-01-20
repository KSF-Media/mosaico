module Mosaico.Server.Template where

import Prelude

import Data.Argonaut.Core as JSON
import Data.Argonaut.Core (Json)
import Data.Array (cons)
import Data.Foldable (fold)
import Data.List (intercalate)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import KSF.Paper as Paper
import Mosaico.Paper (mosaicoPaper)
import React.Basic.DOM (text, title) as DOM
import React.Basic.DOM.Server (renderToStaticMarkup) as DOM

foreign import globalDisableAds :: Boolean

-- Some wrappers around Cheerio
foreign import data Template :: Type
foreign import data TemplateMaster :: Type

foreign import cloneTemplate :: TemplateMaster -> Template
foreign import renderTemplateHtml :: Template -> String

foreign import parseTemplate :: String -> TemplateMaster

foreign import appendMosaicoImpl :: EffectFn2 String Template Template
appendMosaico :: String -> Template -> Effect Template
appendMosaico content htmlTemplate = runEffectFn2 appendMosaicoImpl content htmlTemplate

foreign import appendHeadImpl :: EffectFn2 String Template Template
appendHead :: String -> Template -> Effect Template
appendHead content htmlTemplate =
  runEffectFn2 appendHeadImpl
  (fold
     [ "<link rel='icon' type='image/png' href='" <> base <> "/favicon-48.png'/>"
     , "<link rel='icon' type='image/svg+xml' href='" <> base <> "/favicon-svg.svg'/>"
     , "<link rel='apple-touch-icon' href='" <> base <> "/favicon-180.png'/>"
     , "<link rel='manifest' href='/assets/" <> file <> "-manifest.json'/>"
     , content
     ])
  htmlTemplate
    where
      file = Paper.cssName mosaicoPaper
      base = "https://cdn.ksfmedia.fi/mosaico/favicon/" <> file

foreign import appendVarsImpl :: EffectFn2 String Template Template
appendVars :: String -> Template -> Effect Template
appendVars = runEffectFn2 appendVarsImpl


mkWindowVariables :: Array (Tuple String Json) -> String
mkWindowVariables vars =
  let jsVars = map (\(name /\ value) -> "window." <> name <> "=" <> stringify value <> ";") $
               cons ("globalDisableAds" /\ JSON.fromBoolean globalDisableAds) vars
      stringify =
        String.replaceAll (String.Pattern "<") (String.Replacement "\\u003c") <<< JSON.stringify
  in "<script>" <> intercalate "" jsVars <> "</script>"

makeTitle :: String -> String
makeTitle title =
  DOM.renderToStaticMarkup $
    DOM.title { children: [ DOM.text title ] }
