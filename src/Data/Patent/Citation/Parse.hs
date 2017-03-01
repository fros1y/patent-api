module Data.Patent.Citation.Parse
  ( parseCitation
  ) where

import           Data.Char         (digitToInt)
import           Data.Patent.Types
import           Data.Text         (pack, unpack)
import           Protolude
import qualified Text.Parsec       as Parsec

usPubAppFormat :: Parsec.Parsec Text () Citation
usPubAppFormat = do
  void $ Parsec.string "US"
  year <- Parsec.count 4 Parsec.digit
  Parsec.optional $ Parsec.char '/'
  serialNo <- Parsec.count 7 Parsec.digit
  let trimLeadZero = dropWhile (== '0') serialNo -- For some reason, EPO data drops these zeros
      serialPart = year <> trimLeadZero
  kindPart <- Parsec.optionMaybe (Parsec.many1 Parsec.anyChar)
  return $
    Citation
    { _citationCountry = "US"
    , _citationSerial = pack serialPart
    , _citationKind = pack <$> kindPart
    , _citationPubDate = Nothing
    , _citationSpecialCase = Just $ EPO_US_Pub_App . pack $ (year <> serialNo)
    }

epodocFormat :: Parsec.Parsec Text () Citation
epodocFormat = do
  countryPart <- Parsec.count 2 Parsec.letter
  serialPart <- Parsec.many1 Parsec.digit
  kindPart <- Parsec.optionMaybe (Parsec.many1 Parsec.anyChar)
  return $
    Citation
    { _citationCountry = pack countryPart
    , _citationSerial = pack serialPart
    , _citationKind = pack <$> kindPart
    , _citationPubDate = Nothing
    , _citationSpecialCase = Nothing
    }

messyUSPatent :: Parsec.Parsec Text () Citation
messyUSPatent = do
  Parsec.optional $ do
    _ <- countryPhrase
    _ <- Parsec.spaces
    _ <- patentPhrase
    _ <- Parsec.spaces
    return ()
  serialNo <- commaSepPatNumber
  return $
    Citation
    { _citationCountry = "US"
    , _citationSerial = serialNo
    , _citationKind = Nothing
    , _citationPubDate = Nothing
    , _citationSpecialCase = Nothing
    }

lensLikeFormat :: Parsec.Parsec Text () Citation
lensLikeFormat = do
  countryPart <- Parsec.count 2 Parsec.letter
  _ <- Parsec.char '_'
  serialPart <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '_'
  kindPart <- Parsec.many1 Parsec.anyChar
  return $
    Citation
    { _citationCountry = pack countryPart
    , _citationSerial = pack serialPart
    , _citationKind = Just $ pack kindPart
    , _citationPubDate = Nothing
    , _citationSpecialCase = Nothing
    }

countryPhrase :: Parsec.Parsec Text () ()
countryPhrase =
  void $
  Parsec.choice
    [ Parsec.try $ Parsec.string "United States"
    , Parsec.try $ Parsec.string "U.S."
    , Parsec.string "US"
    ]

patentPhrase :: Parsec.Parsec Text () ()
patentPhrase = do
  _ <- typePhrase
  _ <- Parsec.optional $ Parsec.char '.'
  _ <- Parsec.spaces
  _ <- Parsec.optional numberSignalPhrase
  _ <- Parsec.optional $ Parsec.char '.'
  _ <- Parsec.spaces
  return ()

typePhrase :: Parsec.ParsecT Text u Identity ()
typePhrase =
  void $
  Parsec.choice
    [Parsec.try $ Parsec.string "Pat", Parsec.try $ Parsec.string "Patent"]

numberSignalPhrase :: Parsec.ParsecT Text u Identity ()
numberSignalPhrase =
  void $
  Parsec.choice
    [Parsec.try $ Parsec.string "No", Parsec.try $ Parsec.string "Number"]

jpxNumber :: Parsec.Parsec Text () Citation
jpxNumber = do
  _ <- Parsec.string "JP"
  emperor <-
    Parsec.choice
      [Parsec.try $ Parsec.string "S", Parsec.try $ Parsec.string "H"]
  serialPart <- Parsec.many1 Parsec.digit
  kindPart <- Parsec.optionMaybe (Parsec.many1 Parsec.anyChar)
  -- http://www.epo.org/searching-for-patents/helpful-resources/asian/japan/numbering.html
  return $
    Citation
    { _citationCountry = "JP"
    , _citationSerial = pack $ emperor <> serialPart
    , _citationKind = pack <$> kindPart
    , _citationPubDate = Nothing
    , _citationSpecialCase = Just JPX
    }

triplet :: Parsec.ParsecT Text u Identity [Char]
triplet = Parsec.optional (Parsec.char ',') >> Parsec.count 3 Parsec.digit

-- only matching "modern" 7 digit series patents
commaSepPatNumber :: Parsec.Parsec Text () Text
commaSepPatNumber = do
  firstPart <- Parsec.digit
  rest <- Parsec.count 2 triplet
  return $ pack (firstPart : concat rest)

patentFormats :: Parsec.Parsec Text () Citation
patentFormats =
  Parsec.choice
    [ Parsec.try usPubAppFormat
    , Parsec.try jpxNumber
    , Parsec.try epodocFormat
    , Parsec.try messyUSPatent
    , Parsec.try lensLikeFormat
    ]

-- | Parses a variety of textual formats into a normalized Citation structure.
--
-- Formats such as US1234567 or EP1234567 are understood, as are messier variations on "U.S. Pat. No. 1,234,567"
-- For some countries, notably JP, a kind code will basically be required to get any results in Citation. In other cases,
-- like U.S. patents, it is not required.
--
-- Other formats, like US_1234567_A, or US2016/1234567, are also supported.
-- For more information, check out
-- http://www.hawkip.com/advice/variations-of-publication-number-formatting-by-country
-- http://documents.epo.org/projects/babylon/eponet.nsf/0/94AA7EF4AAB18DDEC125806500367F15/$FILE/publ1_20161102_en.pdf
parseCitation :: Text -> Either Parsec.ParseError Citation
parseCitation input = Parsec.parse patentFormats (unpack input) input
