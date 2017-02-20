{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Patent.Providers.EPO.PDF
  ( getCitationInstances
  , downloadCitationInstance
  , PageProgress
  , silentProgress
  ) where

import           Control.Lens.Operators            hiding ((&))
import           Control.Monad.Logger
import qualified Data.Patent.Citation.Format       as Format
import           Data.Patent.Providers.EPO.Network
import qualified Data.Patent.Providers.EPO.Types   as EPO
import qualified Data.Patent.Types                 as Patent
import           Data.String.Here
import qualified Data.Text                         as T
import           Protolude
import           System.IO.Streams                 (fromLazyByteString)
import qualified System.IO.Streams                 as S
import qualified System.IO.Temp                    as Temp
import qualified Text.Parsec                       as Parsec
import qualified Text.XML                          as XML
import           Text.XML.Cursor                   (($//), (>=>))
import qualified Text.XML.Cursor                   as XML
import qualified Turtle                            hiding ((<>))

type TotalPages = Int

type CurrPage = Int

type Instance = (Int, Patent.Citation)

type PageProgress = TotalPages -> CurrPage -> IO ()

getInstances :: Patent.Citation -> EPO.Session XML.Document
getInstances citation = do
  url <- buildURL $ publishedData "images" citation
  rawData <- throttledQuery url
  return $ XML.parseLBS_ XML.def rawData

-- | Gets a list of Document Instances that correspond with a particular Citation.
-- Strictness determines whether non-equivalent Citation are returned
-- Note that some likely unwanted kind codes (notably EP A3 and EP A4 search reports) are excluded unless you were
-- specifically requesting them.
getCitationInstances :: Bool -> Patent.Citation -> EPO.Session [Instance]
getCitationInstances strictly citation = do
  imagedata <- getInstances citation
  let rawInstances = getLinksAndCounts imagedata
      filteredInstances = filter allow rawInstances
      unwantedKind e (c, k) =
        e ^. Patent.citationCountry == c &&
        (citation ^. Patent.citationKind /= Just k &&
         e ^. Patent.citationKind == Just k)
      allow (l, e)
        | l <= 1 = False
        | strictly && not (e `Format.equivalentCitation` citation) = False
        | e `unwantedKind` ("EP", "A3") = False -- exclude search reports, unless we ask for them
        | e `unwantedKind` ("EP", "A4") = False
        | otherwise = True
  $(logDebug)
    [i|Found ${length rawInstances} total instances. After filtering, ${length filteredInstances} are left.|]
  return filteredInstances

imageLinkToCitation :: Text -> Maybe Patent.Citation
imageLinkToCitation imageLink =
  hush $ Parsec.parse opsImageFormat "opsImageFormat" imageLink
  where
    opsImageFormat :: Parsec.Parsec Text () Patent.Citation
    opsImageFormat = do
      void $ Parsec.string "published-data/images/"
      countryPart <- Parsec.count 2 Parsec.letter
      void $ Parsec.char '/'
      serialPart <- Parsec.many1 Parsec.digit
      void $ Parsec.char '/'
      kindPart <- Parsec.many1 (Parsec.letter <|> Parsec.digit)
      return $
        Patent.Citation
        { Patent._citationCountry = T.pack countryPart
        , Patent._citationSerial = T.pack serialPart
        , Patent._citationKind = Just $ T.pack kindPart
        , Patent._citationPubDate = Nothing
        }

getLinksAndCounts :: XML.Document -> [Instance]
getLinksAndCounts xml = catMaybes (getLinkAndCount <$> instances)
  where
    cursor = XML.fromDocument xml
    instances =
      cursor $// XML.laxElement "document-instance" >=>
      XML.attributeIs "desc" "FullDocument"
    getLinkAndCount instanceCursor =
      let instanceEPODOC =
            imageLinkToCitation $
            headDef "" (XML.attribute "link" instanceCursor)
          pageCount =
            join $
            (readMaybe . T.unpack) <$>
            headMay (XML.attribute "number-of-pages" instanceCursor)
      in case (pageCount, instanceEPODOC) of
           (Just pg, Just iEPODOC) -> Just (pg, iEPODOC)
           (_, _)                  -> Nothing

-- | Convenience predefinition for silent PageProgress updates.
silentProgress :: PageProgress
silentProgress _ _ = return ()

-- | For a given Instance, retrieves all individual pages from the EPO OPS system in PDF format.
-- These are stored into a temporary folder in the working directory of the calling process. After retrieval, the individual
-- PDF pages are joined together using a shell invocation of 'gs' (Ghostscript), which must be installed for this work.
-- The final PDF is stored in the working directory of the calling process, as ${EPODOC}.pdf format, and the temporary
-- directory is removed.
--
-- The first parameter, PageProgress, allows for easier progress reporting.
downloadCitationInstance :: PageProgress -> Instance -> EPO.Session ()
downloadCitationInstance progressFn (count, instanceCitation) = do
  let pages = [1 .. count]
      epokey = Format.asEPODOC instanceCitation
      output = epokey <> ".pdf"
  _ <-
    Temp.withTempDirectory "." "pat-download." $ \tmpDir -> do
      mapM_
        (downloadCitationPageAsPDF instanceCitation tmpDir (progressFn count))
        pages
      liftIO $
        Turtle.shell
          [i|gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${output} "${tmpDir}"/*.pdf|]
          Turtle.empty
  return ()

downloadCitationPageAsPDF :: Patent.Citation
                          -> [Char]
                          -> (CurrPage -> IO ())
                          -> Int
                          -> EPO.Session ()
downloadCitationPageAsPDF citation path progressFn page = do
  url <- buildURL $ imageData citation page
  let file =
        (T.pack path) <> "/" <> (Format.asEPODOC citation) <> "/" <>
        (T.justifyRight 5 '0' (show page)) <>
        ".pdf"
  downloadFile url (T.unpack file)
  liftIO $ progressFn page

downloadFile :: Text -> FilePath -> EPO.Session ()
downloadFile url name = do
  contents <- throttledQuery url
  output <- liftIO $ fromLazyByteString contents
  liftIO $ S.withFileAsOutput name (S.connect output)
