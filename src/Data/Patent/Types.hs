{-|
Module      : Data.Patent.Types
Description : Types common to the Data.Patent library, particularly Citation and Bibliography.
Copyright   : (c) Martin Galese, 2017
License     : AGPL-3
Maintainer  : martin@galese.net
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Patent.Types where

import           Protolude

import           Control.Lens.TH

type Date = Text

-- | A Citation stores sufficient information to either uniquely identify a specific patent document, or if Kind is not supplied, a number of patent documents associated with a given country code and serial
data Citation = Citation
  { _citationCountry     :: Text
  , _citationSerial      :: Text
  , _citationKind        :: Maybe Text
  , _citationPubDate     :: Maybe Date
  , _citationSpecialCase :: Maybe SpecialCase
  } deriving (Show, Eq, Ord)

-- | Some Citation formats need special treatment for the EPO API, and so may be tagged at parse, etc.
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
