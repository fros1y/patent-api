{-# LANGUAGE TemplateHaskell #-}

module Data.Patent.Types where

import           Protolude

import           Control.Lens.TH

type Date = Text

data Citation = Citation
  { _citationCountry     :: Text
  , _citationSerial      :: Text
  , _citationKind        :: Maybe Text
  , _citationPubDate     :: Maybe Date
  , _citationSpecialCase :: Maybe SpecialCase
  } deriving (Show, Eq, Ord)

data SpecialCase
  = EPO_US_Pub_App Text
  | JPX
  deriving (Show, Eq, Ord)

data CPCCode = CPCCode
  { _cpcSection   :: Text
  , _cpcClass     :: Text
  , _cpcSubclass  :: Text
  , _cpcMainGroup :: Text
  , _cpcSubgroup  :: Text
  } deriving (Show, Eq, Ord)

data Bibliography = Bibliography
  { _biblioPubDate           :: Date
  , _biblioIPCs              :: [Text]
  , _biblioCPCs              :: [CPCCode]
  , _biblioAppDate           :: Date
  , _biblioAppCitation       :: Citation
  , _biblioPriorityDates     :: [Date]
  , _biblioPriorityCitations :: [Citation]
  , _biblioApplicants        :: [Text]
  , _biblioInventors         :: [Text]
  , _biblioTitle             :: Text
  , _biblioPatentCitations   :: [Citation]
  , _biblioAbstract          :: Text
  , _familyID                :: Text
  } deriving (Show)

makeLenses ''CPCCode

makeLenses ''Citation

makeLenses ''Bibliography
