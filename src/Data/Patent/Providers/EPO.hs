{-# LANGUAGE QuasiQuotes #-}

module Data.Patent.Providers.EPO
  ( getFamily
  , getBibliography
  , getCitingPatentDocs
  , Session
  , Credentials(..)
  , LogLevel(..)
  , withSession
  , v31
  ) where

import           Control.Arrow
import           Control.Lens                                   hiding ((&))
import           Control.Monad.Logger                           (LogLevel (..))
import           Data.Patent.Providers.EPO.Network
import           Data.Patent.Providers.EPO.Parsers.Bibliography
import qualified Data.Patent.Providers.EPO.Parsers.XMLDocDB     as XMLDocDB
import           Data.Patent.Providers.EPO.Types
import qualified Data.Patent.Types                              as Patent
import           Data.String.Here
import           Protolude
import qualified Text.XML                                       as XML
import           Text.XML.Cursor                                (($//))
import qualified Text.XML.Cursor                                as XML

getBibliography :: Text -> Patent.Citation -> Session Patent.Bibliography
getBibliography prefLang citation = do
  url <- buildURL $ publishedData "biblio" citation
  rawData <- throttledQuery url
  return $ (parseBibliography prefLang) <<< XML.parseLBS_ XML.def $ rawData

getFamily :: Patent.Citation -> Session [Patent.Citation]
getFamily citation = do
  url <- buildURL $ familyData citation
  rawData <- throttledQuery url
  return $ parseFamily <<< XML.parseLBS_ XML.def $ rawData

parseFamily :: XML.Document -> [Patent.Citation]
parseFamily xml = family
  where
    cursor = XML.fromDocument xml
    exchangeDocuments = cursor $// XML.laxElement "exchange-document"
    family = XMLDocDB.parseAttributesToCitation <$> exchangeDocuments

getCitingPatentDocs :: Patent.Citation -> Session [Patent.Citation]
getCitingPatentDocs citation = do
  let epoString =
        (citation ^. Patent.citationCountry) <>
        (citation ^. Patent.citationSerial)
  rawData <- search [i|ct=${epoString}|]
  let xml = XML.parseLBS_ XML.def rawData
      cursor = XML.fromDocument xml
      epodocs =
        XMLDocDB.parseXMLtoCitation <$>
        (cursor $// XML.laxElement "document-id" >=>
         XML.attributeIs "document-id-type" "docdb")
  return epodocs
