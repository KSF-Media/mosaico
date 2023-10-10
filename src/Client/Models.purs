module Mosaico.Client.Models where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple)
import Lettera.Models (ArticleStub, Category, Categories, FullArticle)
import KSF.Sentry as Sentry
import KSF.User (User)
import Mosaico.Cache (Cache)
import Mosaico.Feed (ArticleFeed, ArticleFeedType, Feeds)
import Mosaico.StaticPage (StaticPageResponse)
import Mosaico.Routes as Routes
import Routing.PushState (LocationState, PushStateInterface)

data ModalView = LoginModal

derive instance eqModalView :: Eq ModalView

-- Things that can be resolved before seeing main component's props.
type InitialValues =
  { cache :: Cache
  , nav :: PushStateInterface
  , locationState :: LocationState
  , staticPageContent :: Maybe String
  , staticPageScript :: Maybe String
  , logger :: Sentry.Logger
  , currentDate :: Date
  }

type State =
  { route :: Routes.MosaicoPage
  , prevRoute :: Maybe (Tuple Routes.MosaicoPage String)
  , clickedArticle :: Maybe ArticleStub
  , modalView :: Maybe ModalView
  , user :: Maybe (Maybe User)
  , staticPage :: Maybe StaticPageResponse
  , feeds :: Feeds
  , ssrPreview :: Boolean
  , scrollToYPosition :: Maybe Number
  , starting :: Boolean
  , articleAllowAds :: Boolean
    -- Bump whenever there's a chance that preview articles could be
    -- turned to full articles
  , paywallCounter :: Int
  , currentDate :: Date
  }

type Props =
  { article :: Maybe FullArticle
  , categoryStructure :: Array Category
  -- Derived from categoryStructure
  , catMap :: Categories
  , advertorials :: Maybe (NonEmpty Array ArticleStub)
  -- For tests, they are prone to break in uninteresting ways with ads
  , globalDisableAds :: Boolean
  , initialFeeds :: Array (Tuple ArticleFeedType ArticleFeed)
  , headless :: Boolean
  , initialCurrentDate :: Date
  }
