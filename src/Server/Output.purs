module Mosaico.Server.Output where

import Prelude

import Payload.ContentType as ContentType
import Payload.Headers as Headers
import Payload.ResponseTypes (Response(..))

htmlContent :: forall a. Response a -> Response a
htmlContent (Response response) =
  Response $ response { headers = Headers.set "content-type" ContentType.html response.headers }

jsContent :: forall a. Response a -> Response a
jsContent (Response response) =
  Response $ response { headers = Headers.set "content-type" ContentType.javaScript response.headers }
