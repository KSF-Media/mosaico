module Mosaico.Component.Search where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), isNothing, maybe)
import Effect (Effect)
import KSF.InputField as InputField
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState', (/\))
import React.Basic.Hooks as React
import Mosaico.Client.Handlers (Handlers)

type Props =
  { query :: Maybe String
  , handlers :: Handlers
  , searching :: Boolean
  }

type State =
  { query :: Maybe String
  , setQuery :: Maybe String -> Effect Unit
  }

searchComponent :: Component Props
searchComponent = do
  component "SearchComponent" \props -> React.do
    query /\ setQuery <- useState' props.query
    pure $ render (Just {query, setQuery}) props

render :: Maybe State -> Props -> JSX
render state props =
  DOM.div
    { className: "pb-3 max-w-xl mb-3 mosaico-search md:pb-5 [grid-area:search]"
    , children:
        [ DOM.form
            { className: "flex border border-gray-300 border-solid mosaico-search__form dark:border-0 dark:bg-aptoma-group-content-bg"
            , children:
                [ DOM.span
                    { className: "flex-auto"
                    , children:
                        [ InputField.inputField
                            { type_: InputField.Text
                            , name: "q"
                            , placeholder: "Sök..."
                            , label: Nothing
                            , value: maybe props.query _.query state
                            , onChange: maybe (const mempty) _.setQuery state
                            , validationError: Nothing
                            , disabled: props.searching
                            , autoFocus: true
                            }
                        ]
                    }
                , DOM.span
                    { className: "flex justify-center items-center"
                    , children:
                        [ DOM.button
                            { type: "submit"
                            , className: "flex justify-center items-center mr-4 w-8 h-8 bg-green-500 rounded-md border-0 mosaico-search__button hover:bg-green-300"
                            , children: [ DOM.span { className: "w-6 h-6 bg-white maskimage-search mask-size-6" } ]
                            , disabled: let query = (state >>= _.query) <|> props.query
                                        in isNothing query || query == Just "" || props.searching
                            }
                        ]
                    }
                ]
            , onSubmit: capture_
                case _.query =<< state of
                  Just q | q /= "" -> props.handlers.doSearch q
                  _ -> pure unit
            }
        ]
    }
