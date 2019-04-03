{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Data.Patent.Providers.EPO.PDF
Description : High-level API for retrieving PDFs of patent documents from EPO OPS system.
Copyright   : (c) Martin Galese, 2017
License     : AGPL-3
Maintainer  : martin@galese.net
Stability   : experimental
Portability : POSIX
-}
module Data.Patent.Providers.EPO.PDF
  ( getCitationInstances
  , downloadCitationInstance
  , streamCitationInstanceTIFF
  , streamCitationPageAsTIFF
  , streamCitationPageAsPDF
  -- * Types and defaults
  , PageProgress
  , EPO.Instance
  , EPO.numPages
  , EPO.fullCitation
  , EPO.bookmarkTitle
  , EPO.bookmarkPage
  , EPO.bookmarkSections
  , silentProgress
  ) where

import           Control.Lens.Operators            hiding ((&))
import qualified Control.Monad.Catch               as Catch
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
import           Text.XML.Cursor                   (($/), ($//), (>=>))
import qualified Text.XML.Cursor                   as XML
import qualified Turtle                            hiding ((<>))

type TotalPages = Int

type CurrPage = Int

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
getCitationInstances :: Bool -> Patent.Citation -> EPO.Session [EPO.Instance]
getCitationInstances strictly citation = do
  imagedata <- Catch.tryJust handleNoResults $ getInstances citation
  let rawInstances = either (const []) getLinksAndCounts imagedata
      filteredInstances = filter allow rawInstances
      unwantedKind e (c, k) =
        e ^. Patent.citationCountry == c &&
        (citation ^. Patent.citationKind /= Just k &&
         e ^. Patent.citationKind == Just k)
      allow EPO.Instance {EPO._numPages = l, EPO._fullCitation = e}
        | l <= 1 = False
        | strictly && not (e `Format.equivalentCitation` citation) = False
        | e `unwantedKind` ("EP", "A3") = False -- exclude search reports, unless we ask for them
        | e `unwantedKind` ("EP", "A4") = False
        | otherwise = True
  $(logDebug)
    [i|Found ${length rawInstances} total instances. After filtering, ${length filteredInstances} are left.|]
  return $
    if (null filteredInstances)
      then rawInstances
      else filteredInstances

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
        , Patent._citationSpecialCase = Nothing
        }

getBookmark :: XML.Cursor -> Maybe EPO.Bookmark
getBookmark cursor = liftM2 EPO.Bookmark mark page
  where
    page :: Maybe Int
    page =
      join $
      (readMaybe . T.unpack) <$> headMay (XML.attribute "start-page" cursor)
    mark :: Maybe Text
    mark = headMay $ XML.attribute "name" cursor

getLinksAndCounts :: XML.Document -> [EPO.Instance]
getLinksAndCounts xml = catMaybes (getLinkAndCount <$> instances)
  where
    cursor = XML.fromDocument xml
    instances =
      cursor $// XML.laxElement "document-instance" >=>
      XML.attributeIs "desc" "FullDocument"

getLinkAndCount :: XML.Cursor -> Maybe EPO.Instance
getLinkAndCount instanceCursor =
  (liftM3 EPO.Instance pageCount instanceEPODOC (Just bookmarks))
  where
    instanceEPODOC =
      imageLinkToCitation $ headDef "" (XML.attribute "link" instanceCursor)
    pageCount =
      join $
      (readMaybe . T.unpack) <$>
      headMay (XML.attribute "number-of-pages" instanceCursor)
    bookmarks =
      catMaybes $
      getBookmark <$> (instanceCursor $/ XML.laxElement "document-section")

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
downloadCitationInstance :: PageProgress
                         -> FilePath
                         -> EPO.Instance
                         -> EPO.Session ()
downloadCitationInstance progressFn basePath citeInstance = do
  let pages = [1 .. citeInstance ^. EPO.numPages]
      epokey = Format.asEPODOC $ citeInstance ^. EPO.fullCitation
      output = (T.pack basePath) <> "/" <> epokey <> ".pdf"
  _ <-
    Temp.withTempDirectory basePath "pat-download." $ \tmpDir -> do
      mapM_
        (downloadCitationPageAsPDF
           (citeInstance ^. EPO.fullCitation)
           tmpDir
           (progressFn (citeInstance ^. EPO.numPages)))
        pages
      liftIO $ writeBookmarks (citeInstance ^. EPO.bookmarkSections) tmpDir
      liftIO $
        Turtle.shell
          [i|gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=${output} "${tmpDir}"/*.pdf "${tmpDir}"/pdfmarks|]
          Turtle.empty
  return ()


streamCitationInstanceTIFF :: PageProgress
                           -> EPO.Instance
                           -> EPO.Session ([LByteString])
streamCitationInstanceTIFF progressFn citeInstance = do
  let pages = [1 .. citeInstance ^. EPO.numPages]
  mapM (streamCitationPageAsTIFF
        (citeInstance ^. EPO.fullCitation)
        (progressFn (citeInstance ^. EPO.numPages))) pages

writeBookmarks :: [EPO.Bookmark] -> FilePath -> IO ()
writeBookmarks marks path = writeFile (path <> "/" <> "pdfmarks") contents
  where
    contents = mconcat $ map writeBookmark marks

writeBookmark :: EPO.Bookmark -> Text
writeBookmark mark =
  [i|[/Title (${mark ^. EPO.bookmarkTitle}) /Page ${mark ^. EPO.bookmarkPage} /OUT pdfmark|] <>
  "\n"

downloadCitationPageAsPDF :: Patent.Citation
                          -> [Char]
                          -> (CurrPage -> IO ())
                          -> Int
                          -> EPO.Session ()
downloadCitationPageAsPDF citation path progressFn page = do
  url <- buildURL $ imageData citation page
  let file =
        (T.pack path) <> "/" <> (Format.asEPODOC citation) <> "-" <>
        (T.justifyRight 5 '0' (show page)) <>
        ".pdf"
  downloadFile url (T.unpack file)
  liftIO $ progressFn page

streamCitationPageAsPDF :: Patent.Citation
                          -> Int
                          -> EPO.Session (LByteString)
streamCitationPageAsPDF citation page = do
  url <- buildURL $ imageData citation page
  downloadStream url

downloadCitationPageAsTIFF :: Patent.Citation
                          -> [Char]
                          -> (CurrPage -> IO ())
                          -> Int
                          -> EPO.Session ()
downloadCitationPageAsTIFF citation path progressFn page = do
  url <- buildURL $ tiffData citation page
  let file =
        (T.pack path) <> "/" <> (Format.asEPODOC citation) <> "-" <>
        (T.justifyRight 5 '0' (show page)) <>
        ".tiff"
  downloadFile url (T.unpack file)
  liftIO $ progressFn page

streamCitationPageAsTIFF :: Patent.Citation
                          -> (CurrPage -> IO ())
                          -> Int
                          -> EPO.Session (LByteString)
streamCitationPageAsTIFF citation progressFn page = do
  url <- buildURL $ tiffData citation page
  liftIO $ progressFn page
  downloadStream url


downloadStream :: Text -> EPO.Session(LByteString)
downloadStream url = do
  contents <- throttledQuery url
  return contents

downloadFile :: Text -> FilePath -> EPO.Session ()
downloadFile url name = do
  contents <- throttledQuery url
  output <- liftIO $ fromLazyByteString contents
  liftIO $ S.withFileAsOutput name (S.connect output)
