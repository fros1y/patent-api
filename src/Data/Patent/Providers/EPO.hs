{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : Data.Patent.Providers.EPO
Description : High-level API for requesting content from EPO OPS system.
Copyright   : (c) Martin Galese, 2017
License     : AGPL-3
Maintainer  : martin@galese.net
Stability   : experimental
Portability : POSIX
-}
module Data.Patent.Providers.EPO
  ( getFamilyBibliographies
  , getBibliography
  , getCitingPatentDocs
  -- * System types
  , Session
  , Credentials(..)
  , LogLevel(..)
  -- * Session manager
  , withSession
  -- * EPO OPS API Versions
  , v31
  , v32
  ) where

import           Control.Arrow
import           Control.Lens                                   hiding ((&))
import qualified Control.Monad.Catch                            as Catch
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

-- | Queries EPO OPS for a 'Bibliography' based on a 'Citation' and a (preferred) language code.
getBibliography :: Text -> Patent.Citation -> Session Patent.Bibliography
getBibliography prefLang citation = do
  url <- buildURL $ publishedData "biblio" citation
  rawData <- throttledQuery url
  return $ (parseBibliography prefLang) <<< XML.parseLBS_ XML.def $ rawData

-- | Retrieves 'Bibliography's related to a given 'Citation' by a simple family grouping in EPO OPS data.
getFamilyBibliographies :: Text -> Patent.Citation -> Session Family
getFamilyBibliographies prefLang citation = do
  url <- buildURL $ familyData citation
  rawData <- throttledQuery url
  return $ (parseFamily prefLang) <<< XML.parseLBS_ XML.def $ rawData

parseFamily :: Text -> XML.Document -> Family
parseFamily prefLang xml = family
  where
    cursor = XML.fromDocument xml
    exchangeDocuments = cursor $// XML.laxElement "exchange-document"
    fID = headDef "" $ concat $ XML.attribute "family-id" <$> exchangeDocuments
    fMembers = (extractBibliography prefLang) <$> exchangeDocuments

-- | Searches EPO OPS for other 'Citation's that contain a recorded citation to the given 'Citation'
getCitingPatentDocs :: Patent.Citation -> Session [Patent.Citation]
getCitingPatentDocs citation = do
  let epoString =
        (citation ^. Patent.citationCountry) <>
        (citation ^. Patent.citationSerial)
  rawData <- Catch.tryJust handleNoResults $ search [i|ct=${epoString}|]
  case rawData of
    Right rawData' -> do
      let xml = XML.parseLBS_ XML.def rawData'
          cursor = XML.fromDocument xml
          citations =
            XMLDocDB.parseXMLtoCitation <$>
            (cursor $// XML.laxElement "document-id" >=>
             XML.attributeIs "document-id-type" "docdb")
      return citations
    Left _ -> return []
