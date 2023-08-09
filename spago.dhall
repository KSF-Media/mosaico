{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "mosaico"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-node"
  , "affjax-web"
  , "affresco-components"
  , "affresco-user"
  , "affresco-vetrina"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "formatters"
  , "http-methods"
  , "integers"
  , "js-date"
  , "js-promise-aff"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-http"
  , "node-path"
  , "node-process"
  , "nonempty"
  , "now"
  , "nullable"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "payload"
  , "prelude"
  , "profunctor"
  , "random"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "refs"
  , "routing"
  , "semirings"
  , "simple-json"
  , "strings"
  , "tailrec"
  , "tuples"
  , "uuid"
  , "validation"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
