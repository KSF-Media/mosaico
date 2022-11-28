module Mosaico.Search where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import KSF.InputField as InputField
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React

type Props =
  { query :: Maybe String
  , doSearch :: String -> Effect Unit
  , searching :: Boolean
  }

searchComponent :: Component Props
searchComponent = do
  component "SearchComponent" \props -> React.do
    query /\ setQuery <- useState' props.query
    pure $ render query setQuery props

render :: Maybe String -> (Maybe String -> Effect Unit) -> Props -> JSX
render query setQuery { doSearch, searching } =
  DOM.div
    { className: "pb-3 max-w-xl mb-3 mosaico-search md:pb-5 [grid-area:search]"
    , children:
        [ DOM.form
            { className: "flex border border-gray-300 border-solid mosaico-search__form"
            , children:
                [ DOM.span
                    { className: "flex-auto"
                    , children:
                        [ InputField.inputField
                            { type_: InputField.Text
                            , name: "q"
                            , placeholder: "Sök.."
                            , label: Nothing
                            , value: query
                            , onChange: setQuery
                            , validationError: Nothing
                            , disabled: searching
                            }
                        ]
                    }
                , DOM.span
                    { className: "flex justify-center items-center"
                    , children:
                        [ DOM.button
                            { type: "submit"
                            , className: "flex justify-center items-center mr-1 w-8 h-8 bg-green-500 rounded-md border-0 mosaico-search__button hover:bg-green-300"
                            , children: [ DOM.span { className: "w-6 h-6 bg-white maskimage-search mask-size-6" } ]
                            , disabled: isNothing query || query == Just "" || searching
                            }
                        ]
                    }
                ]
            , onSubmit: capture_
                case query of
                  Nothing -> pure unit
                  Just "" -> pure unit
                  Just q -> doSearch q
            }
        ]
    }

moreButton :: Effect Unit -> JSX
moreButton getMore =
  DOM.button
    { children: [ DOM.text "Fler resultat" ]
    , className: "button-green"
    , onClick: capture_ getMore
    }
