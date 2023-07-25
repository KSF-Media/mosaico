module Mosaico.Frontpage.Events where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Effect (Effect)
import React.Basic.Events (EventHandler, handler)
import React.Basic.DOM.Events (nativeEvent)
import Web.DOM (Node)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.Event.Event as Event

onFrontpageClick :: (String -> Effect Boolean) -> EventHandler
onFrontpageClick setRoute = handler nativeEvent $ \event -> do
  let findAnchor :: Node -> Effect (Maybe String)
      findAnchor node = do
        let continue = maybe (pure Nothing) findAnchor =<< Node.parentNode node
        case Node.nodeName node of
          "A" ->
            maybe continue (pure <<< Just) =<<
            maybe (pure Nothing) (Element.getAttribute "href") (Element.fromNode node)
          _ ->
            continue
      tgt = Event.target event
  maybeHref <- maybe (pure Nothing) findAnchor $ Node.fromEventTarget =<< tgt
  case maybeHref of
    Just href | String.take 1 href == "/" -> do
      accepted <- setRoute href
      when accepted do
        Event.preventDefault event
        Event.stopPropagation event
    _ -> pure unit
