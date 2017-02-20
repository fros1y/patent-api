module Data.Patent.Citation.Format
  ( asEPODOC
  , asDOCDB
  , equivalentCitation
  ) where

import           Control.Lens.Operators hiding ((&))
import           Data.Patent.Types
import qualified Data.Text              as T
import           Protolude

equivalentCitation :: Citation -> Citation -> Bool
equivalentCitation a b = a' == b'
  where
    a' = a {_citationKind = Nothing, _citationPubDate = Nothing}
    b' = b {_citationKind = Nothing, _citationPubDate = Nothing}

asEPODOC :: Citation -> Text
asEPODOC citation =
  (citation ^. citationCountry <> citation ^. citationSerial <>
   fromMaybe "" (citation ^. citationKind) <>
   date)
  where
    date =
      case citation ^. citationPubDate of
        Nothing -> ""
        Just d  -> " " <> d

asDOCDB :: Citation -> Text
asDOCDB citation =
  T.intercalate
    "."
    [ citation ^. citationCountry
    , citation ^. citationSerial
    , fromMaybe "%" (citation ^. citationKind)
    ]
