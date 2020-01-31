module Bottega where

import Data.Function.Uncurried (Fn4, runFn4)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, unsafeToForeign)
import KSF.Api (UUID, UserAuth, oauthToken)

foreign import data Api :: Type
foreign import ordersApi :: Api

foreign import callApi_
  :: forall req res opts
   . Fn4
       Api
       String
       req
       { | opts }
       (EffectFnAff res)

callApi :: forall res opts. Api -> String -> Array Foreign -> { | opts } -> Aff res
callApi api methodName req opts =
  fromEffectFnAff (runFn4 callApi_ api methodName req opts)

getOrder :: UserAuth -> OrderNumber -> Aff Order
getOrder { userId, authToken } orderNumber =
  callApi ordersApi "orderOrderNumberGet" [ unsafeToForeign orderNumber ] { authorization, authUser }
  where
    authorization = oauthToken authToken
    authUser = unsafeToForeign userId

newtype OrderNumber = OrderNumber String

type Order =
  { orderNumber :: OrderNumber
  , orderUser   :: UUID
  , orderStatus :: OrderStatus
  }

type OrderStatus =
  { status :: String -- TODO: Use enum
  , time   :: String
  }