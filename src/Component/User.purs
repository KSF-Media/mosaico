module Mosaico.Component.User where

import Prelude

import Data.Array (any)
import Data.Either (Either(..), hush)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (foldMap)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Uncurried (runEffectFn1)
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.User (UserError, User, Subscription, magicLogin)
import KSF.User.Login as Login
import Lettera.Models (FullArticle)
import Mosaico.Analytics (sendArticleAnalytics, setUserVariable)
import Mosaico.Triggerbee (addToTriggerbeeObj, sendTriggerbeeEvent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffect, useEffectOnce)
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Props =
  { initialArticle :: Maybe FullArticle
  , setUser :: Maybe User -> Effect Unit
  , opened :: Boolean
  , onClose :: Effect Unit
  , paper :: Paper.Paper
  -- Main effect is loading the full article if we have a preview
  , onPaywallEvent :: Effect Unit
  }

component :: Sentry.Logger -> Component Props
component logger = do
  loginComponent <- Login.login
  React.component "LoginModal" \(props ::Props) -> React.do
    let sendAnalytics :: Boolean -> Maybe User -> Effect Unit
        sendAnalytics sendArticle u = do
          foldMap (\user -> runEffectFn1 sendTriggerbeeEvent user.email) u
          userStatus <- case u of
            Just user -> do
              now <- JSDate.now
              let isSubscriber = any (isActive) user.subs
                  isActive :: Subscription -> Boolean
                  isActive subscription = case toMaybe subscription.dates.end of
                    Just end -> now <= end
                    Nothing  -> true
              pure { isLoggedIn: true, isSubscriber }
            Nothing -> pure { isLoggedIn: false, isSubscriber: false }
          runEffectFn1 addToTriggerbeeObj userStatus
          setUserVariable u userStatus.isSubscriber
          case guard sendArticle (pure unit) *> props.initialArticle of
            Just a -> sendArticleAnalytics a.article u
            Nothing -> pure unit
        onUserFetch (Left err) = do
          props.onPaywallEvent
          props.setUser Nothing
          Console.error $ "Login error " <> show err
        onUserFetch (Right u) = do
          props.setUser $ Just u
          logger.setUser $ Just u
          props.onClose
          props.onPaywallEvent
          sendAnalytics false $ Just u

    useEffectOnce do
      withLoginLock <- (\l -> Aff.bracket (Aff.AVar.put unit l)
                              (const $ Aff.AVar.take l) <<< const) <$> AVar.empty
      alreadySentInitialAnalytics <- AVar.new false

      -- The initial loading state prevents opening the login dialog
      -- also so better have a timeout.
      stopDefaultLogin <- Aff.killFiber (Exception.error "give up login") <$> Aff.launchAff do
        Aff.delay $ Milliseconds 4999.0
        withLoginLock do
          liftEffect do
            props.setUser Nothing
            sendAnalytics true Nothing
          _ <- Aff.AVar.take alreadySentInitialAnalytics
          Aff.AVar.put true alreadySentInitialAnalytics

      Aff.launchAff_ do
        -- magicLogin doesn't actually call the callback if it fails
        magicLogin Nothing $ hush >>> \u -> Aff.launchAff_ $ withLoginLock do
          stopDefaultLogin
          liftEffect do
            logger.setUser u
            props.setUser u
            props.onPaywallEvent
          alreadySent <- Aff.AVar.take alreadySentInitialAnalytics
          when (not alreadySent) $ liftEffect $ sendAnalytics true u

      pure $ Aff.launchAff_ stopDefaultLogin

    -- Add eventListener for ESC key, which closes the modal
    -- Remove the listener when the LoginModal is unmounted
    useEffect props.opened $ if not props.opened then pure $ pure unit else do
      let captureEsc e = guard (e == "Escape") props.onClose
          fromKeyboardEvent = KeyboardEvent.fromEvent >=> (KeyboardEvent.key >>> Just)

      e <- eventListener $ fromKeyboardEvent >>> foldMap captureEsc

      document <- Web.document =<< Web.window
      addEventListener (EventType "keydown") e false (toEventTarget document)
      pure $ removeEventListener (EventType "keydown") e false (toEventTarget document)

    pure $ if props.opened then render props onUserFetch loginComponent else mempty

render :: Props -> (Either UserError User -> Effect Unit) -> (Login.Props -> JSX) -> JSX
render props onUserFetch loginComponent =
  DOM.div
    { className: "mosaico--login-modal-container"
    , children:
        [ DOM.div
            { className: "mosaico--login-modal"
            , children:
                [ DOM.div
                    { className: "mosaico--login-modal_title"
                    , children:
                        [ DOM.h2
                            { className: "mt-8 text-3xl font-bold font-roboto"
                            , children: [ DOM.text "Logga in" ]
                            }
                        , DOM.span
                            { className: "mosaico--login-modal_close"
                            , onClick: handler_ props.onClose
                            }
                        ]
                    }
                , loginComponent
                    { onMerge: pure unit
                    , onMergeCancelled: pure unit
                    , onRegister: pure unit
                    , onRegisterCancelled: pure unit
                    , onUserFetch
                    , onLogin: Aff.launchAff_
                    , disableSocialLogins: Set.empty
                    , paper: Just props.paper
                    }
                ]
            }
        ]
    }
