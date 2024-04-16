module Mosaico.Component.User where

import Prelude

import Control.Alt ((<|>))
import Data.Array (any)
import Data.Either (Either(..), hush)
import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (foldMap)
import Data.Set as Set
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.AVar as Aff.AVar
import Effect.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Uncurried (runEffectFn1)
import KSF.Api (UserAuth, Token(Token))
import KSF.Auth as Auth
import KSF.Paper as Paper
import KSF.Sentry as Sentry
import KSF.User (UserError(LoginInvalidCredentials), User, Subscription, magicLogin)
import KSF.User as User
import KSF.User.Login as Login
import Lettera.Models (FullArticle)
import Mosaico.Analytics (sendArticleAnalytics, setUserVariable)
import Mosaico.Triggerbee (addToTriggerbeeObj, sendTriggerbeeEvent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Props =
  { initialArticle :: Maybe FullArticle
  , user :: Maybe (Maybe User)
  , setUser :: Maybe User -> Effect Unit
  -- Loaded by need.  Combination of user and IP entitlements.
  , needEntitlements :: Boolean
  , setEntitlements :: Map.Map String UserAuth -> Effect Unit
  , setLoadingEntitlements :: Boolean -> Effect Unit
  , opened :: Boolean
  , onClose :: Effect Unit
  , paper :: Paper.Paper
  -- Main effect is loading the full article if we have a preview
  , onPaywallEvent :: Effect Unit
  , paywallCounter :: Int
  }

component :: Sentry.Logger -> Component Props
component logger = do
  loginComponent <- Login.login
  React.component "LoginModal" \(props ::Props) -> React.do
    ipEntitlements /\ setIPEntitlements <- useState Nothing
    userEntitlements /\ setUserEntitlements <- useState Nothing
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
        entitlementsWithToken token entl old =
          fromMaybe Map.empty $ (map (const token) <<< Set.toMap <$> entl) <|> old
        setUser user = do
          logger.setUser $ Just user
          props.setUser $ Just user
          props.onPaywallEvent
        onUserFetch (Left err) = do
          props.onPaywallEvent
          props.setUser Nothing
          Console.error $ "Login error " <> show err
        onUserFetch (Right user) = do
          setUser user
          props.onClose
          sendAnalytics false $ Just user

    useEffect props.paywallCounter do
      setUserEntitlements $ const Nothing
      pure mempty

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
        magicLogin Nothing $ hush >>> \u -> Aff.launchAff_ $ withLoginLock do
          stopDefaultLogin
          liftEffect $ maybe (props.setUser Nothing) setUser u
          alreadySent <- Aff.AVar.take alreadySentInitialAnalytics
          when (not alreadySent) $ liftEffect $ sendAnalytics true u

      pure $ Aff.launchAff_ stopDefaultLogin

    useEffect {c: props.paywallCounter, n: props.needEntitlements} $
      if props.needEntitlements then do
        props.setLoadingEntitlements true
        stopEntitlementsLoad <- map (Aff.killFiber (Exception.error "it's soggy")) $ Aff.launchAff do
          waitIPEntitlements <- case ipEntitlements of
            Just _ -> pure $ pure unit
            Nothing -> do
              map Aff.joinFiber $ Aff.suspendAff do
                token <- User.loginIP props.paper
                case token of
                  -- No need to try again
                  Left LoginInvalidCredentials -> do
                    let nil = { userId: UUID.emptyUUID, authToken: Token "" }
                    liftEffect $ setIPEntitlements $ Just <<< entitlementsWithToken nil (Just Set.empty)
                  Left _ -> pure unit
                  Right tok -> do
                    entitlements <- User.getUserEntitlements tok
                    liftEffect $ setIPEntitlements $ Just <<< entitlementsWithToken tok (hush entitlements)

          when (isNothing userEntitlements && isJust (join props.user)) do
            maybeToken <- Auth.loadToken
            case maybeToken of
              Just token -> do
                entitlements <- User.getUserEntitlements token
                liftEffect $ setUserEntitlements $ Just <<< (entitlementsWithToken token $ hush entitlements)
              _ -> pure unit

          waitIPEntitlements
          liftEffect $ props.setLoadingEntitlements false

        pure do
          props.setLoadingEntitlements false
          Aff.launchAff_ stopEntitlementsLoad
      else pure mempty

    useEffect {i: maybe Set.empty Map.keys ipEntitlements, u: maybe Set.empty Map.keys userEntitlements} do
      let u = fromMaybe Map.empty userEntitlements
          i = fromMaybe Map.empty ipEntitlements
      props.setEntitlements $ Map.union u i
      pure mempty

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
