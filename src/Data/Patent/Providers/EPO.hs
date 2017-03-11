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
  ( lookup
  , findCitingDocuments
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

-- | Queries EPO OPS for one or more 'Bibliography' records based on a preferred language code, whether or not to pull the full family, and a 'Citation'.
lookup :: Text -> Bool -> Patent.Citation -> Session [Patent.Bibliography]
lookup prefLang familyScope citation = do
  url <-
    buildURL $
    if familyScope
      then familyData citation
      else publishedData "biblio" citation
  rawData' <- Catch.tryJust handleNoResults $ throttledQuery url
  case rawData' of
    Left _ -> return []
    Right rawData -> do
      let xml = XML.parseLBS_ XML.def rawData
          cursor = XML.fromDocument xml
          exchangeDocuments = cursor $// XML.laxElement "exchange-document"
      return $ (extractBibliography prefLang) <$> exchangeDocuments

-- | Searches EPO OPS for other 'Citation's that contain a recorded citation to the given 'Citation'
findCitingDocuments :: Patent.Citation -> Session [Patent.Citation]
findCitingDocuments citation = do
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
