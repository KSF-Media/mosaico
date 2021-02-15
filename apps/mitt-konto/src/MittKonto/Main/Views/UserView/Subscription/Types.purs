module MittKonto.Main.UserView.Subscription.Types where

import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import KSF.AsyncWrapper as AsyncWrapper
import KSF.Sentry as Sentry
import KSF.User as User
import KSF.User (User)
import React.Basic (JSX)
import React.Basic.Classic as React

type Self = React.Self Props State

type Props =
  { subscription :: User.Subscription
  , user :: User
  , logger :: Sentry.Logger
  }

type State =
  { wrapperProgress :: AsyncWrapper.Progress JSX
  , pausedSubscriptions :: Maybe (Array User.PausedSubscription)
  , pendingAddressChanges :: Maybe (Array User.PendingAddressChange)
  , now :: Maybe DateTime
  , updateAction :: Maybe SubscriptionUpdateAction
  }

data SubscriptionUpdateAction
  = PauseSubscription
  | TemporaryAddressChange
  | DeliveryReclamation

type Subscription =
  { package :: { name :: String
               , paper :: { name :: String }
               }
  , state :: String
  , dates :: User.SubscriptionDates
  }