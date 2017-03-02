module Data.Patent.Citation.Format
   -- * Formatters
  ( asEPODOC
  , asDOCDB
  -- * Format-like comparison
  , equivalentCitation
  ) where

import           Control.Lens.Operators hiding ((&))
import           Data.Patent.Types
import qualified Data.Text              as T
import           Protolude

-- | Tests whether two 'Citation's are roughly equivalent given the vagueness with which patent citations are written in common usage (i.e., with the kind codes dropped)
equivalentCitation :: Citation -> Citation -> Bool
equivalentCitation a b = a' == b'
  where
    a' = a {_citationKind = Nothing, _citationPubDate = Nothing}
    b' = b {_citationKind = Nothing, _citationPubDate = Nothing}

-- | Formats a 'Citation' per the EPODOC standard promulgated by the EPO
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

-- | Formats a 'Citation' per the DOCDB standard promulgated by the EPO.
-- One exception is that a 'Citation' without a kind code gets the "%" character, which can serve as a wildcard in some EPO-related systems
asDOCDB :: Citation -> Text
asDOCDB citation =
  T.intercalate
    "."
    [ citation ^. citationCountry
    , citation ^. citationSerial
    , fromMaybe "%" (citation ^. citationKind)
    ]
