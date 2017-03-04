{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.Patent.Providers.EPO.Types where

import           Control.Lens.TH      (makeLenses)
import           Control.Monad.Catch  (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Logger (LoggingT, MonadLogger)
import           Data.Default
import           Data.Patent.Types
import           Network.Wreq.Session as Wreq
import           Protolude

type ServiceEndpoint = Text

newtype OAuth2Token = OAuth2Token
  { _rawToken :: ByteString
  } deriving (Show, Eq)

data Settings = Settings
  { _credentials     :: Credentials
  , _serviceEndpoint :: Text
  , _wreqSession     :: Wreq.Session
  } deriving (Show)

data Credentials = Credentials
  { _consumerKey :: Text
  , _secretKey   :: Text
  } deriving (Show, Eq)

data SessionState = SessionState
  { _oauth2Token :: Maybe OAuth2Token
  , _throttling  :: ThrottlingState
  } deriving (Show)

data ThrottlingState = ThrottlingState
  { _serviceState        :: ServiceState
  , _quotaPerTrafficType :: Map ServiceQuota ServiceStatus
  } deriving (Show)

data ServiceState
  = Idle
  | Busy
  | Overloaded
  deriving (Eq, Ord, Show)

data ServiceTraffic
  = Green
  | Yellow
  | Red
  | Black
  deriving (Eq, Ord, Show)

data ServiceQuota
  = RetrievalQuota
  | SearchQuota
  | INPADOCQuota
  | ImagesQuota
  | OtherQuota
  deriving (Eq, Ord, Show)

data ServiceStatus = ServiceStatus
  { _trafficStatus     :: ServiceTraffic
  , _requestsRemaining :: Int
  } deriving (Show)

instance Default ServiceStatus where
  def = ServiceStatus Black 0

newtype Session a = Session
  { runSession :: LoggingT (ReaderT Settings (StateT SessionState IO)) a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader Settings
             , MonadState SessionState
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadLogger
             )

data Instance = Instance
  { _numPages         :: Int
  , _fullCitation     :: Citation
  , _bookmarkSections :: [Bookmark]
  }

data Bookmark = Bookmark
  { _bookmarkTitle :: Text
  , _bookmarkPage  :: Int
  }

makeLenses ''Bookmark

makeLenses ''Instance

makeLenses ''Settings

makeLenses ''Credentials

makeLenses ''SessionState

makeLenses ''ServiceStatus

makeLenses ''ThrottlingState

makeLenses ''OAuth2Token
