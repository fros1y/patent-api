module Data.Patent.Providers.EPO.Parsers.XMLDocDB where

import qualified Data.Patent.Types as Patent
import           Protolude
import           Text.XML.Cursor   (($/), (&//))
import qualified Text.XML.Cursor   as XML

parseAttributesToCitation :: XML.Cursor -> Patent.Citation
parseAttributesToCitation cursor =
  Patent.Citation
  { Patent._citationCountry = cc
  , Patent._citationSerial = serialNo
  , Patent._citationKind = Just kindCode
  , Patent._citationPubDate = Nothing
  , Patent._citationSpecialCase = Nothing
  }
  where
    cc = headDef "" $ XML.laxAttribute "country" cursor
    serialNo = headDef "" $ XML.laxAttribute "doc-number" cursor
    kindCode = headDef "" $ XML.laxAttribute "kind" cursor

parseXMLtoCitation :: XML.Cursor -> Patent.Citation
parseXMLtoCitation cursor =
  Patent.Citation
  { Patent._citationCountry = cc
  , Patent._citationSerial = serialNo
  , Patent._citationKind = Just kindCode
  , Patent._citationPubDate = Nothing
  , Patent._citationSpecialCase = Nothing
  }
  where
    cc = headDef "" $ cursor $/ XML.laxElement "country" &// XML.content
    serialNo =
      headDef "" $ cursor $/ XML.laxElement "doc-number" &// XML.content
    kindCode = headDef "" $ cursor $/ XML.laxElement "kind" &// XML.content
