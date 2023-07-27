module Mosaico.Analytics where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2) 
import Data.Maybe (Maybe(..), fromMaybe)
import Lettera.Models (Article)
import Data.Nullable (toMaybe)
import KSF.User.Cusno (Cusno, toString)
import KSF.User (User)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (format)
import KSF.Helpers (dateTimeFormatter)
import Data.Array (intercalate)

type StringArticleMetadata = 
  { title :: String
  , publishingTime :: String
  , authors :: String
  , premium :: String
  , listTitle :: String
  , category :: String
  , section :: String
  , articleUuid :: String
  , articleLength :: Int
  , tags :: String
  , userCusno :: String
  , userSubs :: String
  }

foreign import _sendPageView :: Effect Unit
sendPageView :: Effect Unit
sendPageView = _sendPageView

foreign import _pushToDataLayer :: EffectFn1 StringArticleMetadata Unit
pushToDataLayer :: StringArticleMetadata -> Effect Unit
pushToDataLayer = runEffectFn1 _pushToDataLayer

foreign import _setUserVariable :: EffectFn2 Cusno Boolean Unit
setUserVariable ∷ Maybe User -> Boolean -> Effect Unit
setUserVariable u isSubscriber = case u of
  Just user -> runEffectFn2 _setUserVariable user.cusno isSubscriber
  Nothing -> pure unit

sendArticleAnalytics:: Article -> Maybe User -> Effect Unit
sendArticleAnalytics article user = do
  let metadata = 
          { title: article.title
          , publishingTime: foldMap (\x -> format dateTimeFormatter x) article.publishingTimeUtc
          , authors: intercalate ", " $ _.byline <$> article.authors
          , premium: show article.premium
          , category: fromMaybe "" article.analyticsCategory
          , section: fromMaybe "" article.analyticsSection
          , listTitle: fromMaybe "" article.listTitle
          , articleLength: article.charLength
          , articleUuid: (article.uuid :: String)
          , tags: intercalate ", " $ show <$> article.tags
          -- remove these after GTM is set up to read relevant user data from window.ksfUser  >>>
          , userCusno: case user of
            Just u -> toString u.cusno
            Nothing   -> ""
          , userSubs: case user of
            Just u -> intercalate ", " $ _.package.id <$> u.subs
            Nothing -> ""
          -- <<<
          }
  pushToDataLayer metadata
