{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Patent.Providers.EPO.Network where

import           Control.Arrow
import qualified Control.Lens                    as Lens
import           Control.Lens.Operators          hiding ((&))
import           Control.Monad.Catch             (Handler (..))
import           Control.Monad.Logger
import qualified Control.Retry                   as Retry
import           Data.Aeson.Lens
import qualified Data.ByteString                 as B
import           Data.Default                    (def)
import qualified Data.List                       as List
import qualified Data.Map.Strict                 as Map
import qualified Data.Patent.Citation.Format     as Citation
import           Data.Patent.Providers.EPO.Types
import qualified Data.Patent.Types               as Patent
import           Data.String.Here
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Lib.Prelude
import qualified Network.Connection              as NetC
import qualified Network.HTTP.Client             as NetC
import qualified Network.HTTP.Client.TLS         as NetC
import qualified Network.HTTP.Types.Header       as NetH
import qualified Network.HTTP.Types.Status       as NetS
import qualified Network.Wreq                    as Wreq
import qualified Network.Wreq.Session            as WreqS
import           Protolude
import qualified Text.Parsec                     as Parsec

buildURL :: Text -> Session Text
buildURL path = do
  settings <- ask
  return $ settings ^. serviceEndpoint <> path

imageData :: Patent.Citation -> Int -> Text
imageData citation pageNo =
  "/rest-services/published-data/images/" <> imageLink <> ".pdf?Range=" <>
  (show pageNo)
  where
    imageLink =
      citation ^. Patent.citationCountry <> "/" <> citation ^.
      Patent.citationSerial <>
      "/" <>
      (fromMaybe "%" $ citation ^. Patent.citationKind) <>
      "/fullimage"

buildPublicationKey :: Patent.Citation -> Text
buildPublicationKey citation =
  if (isJust $ citation ^. Patent.citationKind)
    then "docdb/" <> Citation.asDOCDB citation
    else "epodoc/" <> Citation.asEPODOC citation

publishedData :: Text -> Patent.Citation -> Text
publishedData service citation =
  "/rest-services/published-data/publication/" <> searchKey <> "/" <> service
  where
    searchKey = buildPublicationKey citation

familyData :: Patent.Citation -> Text
familyData citation =
  "/rest-services/family/publication/epodoc/" <> searchKey <> "/biblio"
  where
    searchKey = Citation.asEPODOC citation

initialThrottlingState :: ThrottlingState
initialThrottlingState =
  ThrottlingState
  { _serviceState = Idle
  , _quotaPerTrafficType =
      Map.fromList
        [ (RetrievalQuota, ServiceStatus Green 200)
        , (SearchQuota, ServiceStatus Green 30)
        , (INPADOCQuota, ServiceStatus Green 60)
        , (ImagesQuota, ServiceStatus Green 200)
        , (OtherQuota, ServiceStatus Green 1000)
        ]
  }

initialState :: SessionState
initialState =
  SessionState
  { _oauth2Token = Nothing
  , _throttling = initialThrottlingState
  , _credentials = Credentials "" ""
  }

v31 :: Text
v31 = "https://ops.epo.org/3.1"

v32 :: Text
v32 = "https://ops.epo.org/3.2"

noVerifyTlsManagerSettings :: NetC.ManagerSettings
noVerifyTlsManagerSettings = NetC.mkManagerSettings noVerifyTlsSettings Nothing

noVerifyTlsSettings :: NetC.TLSSettings
noVerifyTlsSettings =
  NetC.TLSSettingsSimple
  { NetC.settingDisableCertificateValidation = True
  , NetC.settingDisableSession = True
  , NetC.settingUseServerName = False
  }

sessionSettings :: NetC.ManagerSettings
sessionSettings = NetC.tlsManagerSettings

withSession :: Credentials -> ServiceEndpoint -> LogLevel -> Session a -> IO a
withSession creds endpoint logLevel k =
  WreqS.withSessionControl Nothing sessionSettings $ \wreqSess ->
    fst <$>
    runStateT
      (runReaderT
         (runStderrLoggingT $ filterLogger logFilter $ runSession k)
         Settings {_serviceEndpoint = endpoint, _wreqSession = wreqSess})
      (initialState & credentials .~ creds)
  where
    logFilter _ level = level >= logLevel

requestOAuthToken :: Text -> Text -> Session OAuth2Token
requestOAuthToken client_id client_secret = do
  settings <- ask
  let opts =
        Wreq.defaults & Wreq.header "Accept" .~ ["application/json"] &
        Wreq.header "Content-Type" .~
        ["application/x-www-form-urlencoded"]
  let body =
        [ "grant_type" Wreq.:= ("client_credentials" :: [Char])
        , "client_id" Wreq.:= client_id
        , "client_secret" Wreq.:= client_secret
        ]
  resp <-
    liftIO $
    WreqS.postWith
      opts
      (settings ^. wreqSession)
      (T.unpack $ settings ^. serviceEndpoint <> "/auth/accesstoken")
      body
  let token = resp ^. Wreq.responseBody . key "access_token" . _String
  return $ (T.encodeUtf8 >>> OAuth2Token) token

authenticate :: Session OAuth2Token
authenticate = do
  creds <- Lens.use credentials
  token <- Lens.use oauth2Token
  let client_id = creds ^. consumerKey
      client_secret = creds ^. secretKey
  case token of
    Just t -> return t
    Nothing -> do
      $(logInfo) "Getting OAuth2 token"
      newtoken <- requestOAuthToken client_id client_secret
      oauth2Token .= Just newtoken
      return newtoken

search :: Text -> Session LByteString
search params = do
  url <-
    buildURL [i|/rest-services/published-data/search?q=${params}&Range=1-100|]
  rawData <- throttledQuery url
  return $ rawData

throttledQuery :: Text -> Session LByteString
throttledQuery url = do
  potentiallyThrottle (quotaTypeByURL url)
  query url

isAuthenticationProblem :: Wreq.Response body -> Bool
isAuthenticationProblem r =
  ((NetS.statusCode (NetC.responseStatus r) == 400) && "access_token_expired" `B.isPrefixOf`
   authHeader)
  where
    headers = NetC.responseHeaders r
    authHeader = fromMaybe "" $ List.lookup NetH.hWWWAuthenticate headers

isNoResults :: Wreq.Response body -> ByteString -> Bool
isNoResults r _ = (NetS.statusCode (NetC.responseStatus r) == 404)

handleNoResults :: NetC.HttpException -> Maybe Text
handleNoResults (NetC.HttpExceptionRequest _ (NetC.StatusCodeException r c))
  | isNoResults r c = Just "No Results"
  | otherwise = Nothing
handleNoResults _ = Nothing

handleError :: NetC.HttpException -> Session Bool
handleError (NetC.HttpExceptionRequest _ (NetC.StatusCodeException r _))
  | isAuthenticationProblem r = do
    $(logDebug) "Token expired. Trying reauthentication."
    oauth2Token .= Nothing
    void $ authenticate
    return True
  | otherwise = return True
handleError _ = return True

query :: Text -> Session LByteString
query url = do
  token <- authenticate
  settings <- ask
  let handlers = [const $ Handler $ handleError]
      opts =
        Wreq.defaults & Wreq.auth ?~ Wreq.oauth2Bearer (_rawToken token) &
        Wreq.header "Accept" .~
        ["application/xml"]
  $(logDebug) [i|GET: ${url}|]
  r <-
    Retry.recovering
      def
      handlers
      (\_ ->
         liftIO $ WreqS.getWith opts (settings ^. wreqSession) (T.unpack url))
  let body = r ^. Wreq.responseBody
      rawThrottle = r ^. Wreq.responseHeader "X-Throttling-Control"
  updateThrottling . T.decodeUtf8 $ rawThrottle
  return body

quotaTypeByURL :: Text -> ServiceQuota
quotaTypeByURL url
  | "/published-data/search" `T.isInfixOf` url = SearchQuota
  | "/published-data/images" `T.isInfixOf` url = ImagesQuota
  | "/published-data" `T.isInfixOf` url = RetrievalQuota
  | "/family/" `T.isInfixOf` url = INPADOCQuota
  | "/legal/" `T.isInfixOf` url = INPADOCQuota
  | otherwise = ImagesQuota

-- 'idle (images=green:200, inpadoc=green:60, other=green:1000, retrieval=green:200, search=green:30)'
maxQuota :: ServiceQuota -> Int
maxQuota RetrievalQuota = 200
maxQuota SearchQuota    = 30
maxQuota INPADOCQuota   = 60
maxQuota ImagesQuota    = 200
maxQuota OtherQuota     = 1000

potentiallyThrottle :: ServiceQuota -> Session ()
potentiallyThrottle quotaType = do
  quota <- Lens.use throttling
  let status =
        fromMaybe def $ Map.lookup quotaType (quota ^. quotaPerTrafficType)
  delayForServiceStatus $ quota ^. serviceState
  delayForTrafficLight $ status ^. trafficStatus
  delayForRate quotaType $ status ^. requestsRemaining

seconds :: Int
seconds = 1000000

delayForServiceStatus :: ServiceState -> Session ()
delayForServiceStatus Overloaded = do
  $(logWarn) "System: Overloaded."
  liftIO $ threadDelay $ 10 * seconds
delayForServiceStatus Busy = do
  $(logInfo) "System: Busy."
  liftIO $ threadDelay $ 1 * seconds
delayForServiceStatus Idle = do
  $(logDebug) "System: Idle."
  return ()

delayForTrafficLight :: ServiceTraffic -> Session ()
delayForTrafficLight Black = do
  $(logWarn) "Service Traffic: Black"
  liftIO $ threadDelay $ 60 * seconds
delayForTrafficLight Red = do
  $(logWarn) "Service Traffic: Red"
  liftIO $ threadDelay $ 20 * seconds
delayForTrafficLight Yellow = do
  $(logInfo) "Service Traffic: Yellow"
  liftIO $ threadDelay $ 5 * seconds
delayForTrafficLight Green = do
  $(logDebug) "Service Traffic: Green"
  return ()

delayForRate :: ServiceQuota -> Int -> Session ()
delayForRate service rate = do
  let maxRate = maxQuota service
      maxRate' :: Double
      maxRate' = fromIntegral maxRate
      rate' :: Double
      rate' = fromIntegral rate
      delay' :: Double
      delay' = ((maxRate' - rate') / maxRate') * 30
      delay :: Int
      delay = floor $ delay' * fromIntegral seconds
      percent :: Int
      percent =
        floor
          ((fromIntegral rate :: Double) / (fromIntegral maxRate :: Double) *
           100)
  $(logDebug) [i|Service quota at ${percent}%. Delaying ${delay'} seconds|]
  liftIO $ threadDelay delay

updateThrottling :: Text -> Session ()
updateThrottling rawThrottle = do
  throttle <- parseThrottleStatement rawThrottle
  $(logDebug) [i|rawThrottle: '${rawThrottle}'|]
  $(logDebug) [i|parsedThrottle: '${throttle}'|]
  throttling .= throttle

opsServiceStateFromString :: [Char] -> ServiceState
opsServiceStateFromString "idle" = Idle
opsServiceStateFromString "busy" = Busy
opsServiceStateFromString _      = Overloaded

opsServiceTrafficFromString :: [Char] -> ServiceTraffic
opsServiceTrafficFromString "green"  = Green
opsServiceTrafficFromString "yellow" = Yellow
opsServiceTrafficFromString "red"    = Red
opsServiceTrafficFromString _        = Black

opsServiceQuotaFromString :: [Char] -> ServiceQuota
opsServiceQuotaFromString "retrieval" = RetrievalQuota
opsServiceQuotaFromString "search"    = SearchQuota
opsServiceQuotaFromString "inpadoc"   = INPADOCQuota
opsServiceQuotaFromString "images"    = ImagesQuota
opsServiceQuotaFromString _           = OtherQuota

throttleStatement :: Parsec.Parsec Text () ThrottlingState
throttleStatement = do
  let service_state = do
        service_name <- Parsec.many1 Parsec.letter
        void $ Parsec.char '='
        traffic_light <- Parsec.many1 Parsec.letter
        void $ Parsec.char ':'
        request_limit <- Parsec.many1 Parsec.digit
        void $ Parsec.optional $ Parsec.char ','
        void $ Parsec.optional Parsec.spaces
        return
          ( opsServiceQuotaFromString service_name
          , ServiceStatus
            { _trafficStatus = opsServiceTrafficFromString traffic_light
            , _requestsRemaining = readDef 0 request_limit
            })
  system_state <-
    Parsec.choice
      [ Parsec.try $ Parsec.string "idle"
      , Parsec.try $ Parsec.string "busy"
      , Parsec.string "overloaded"
      ]
  void Parsec.spaces
  services <-
    Parsec.between
      (Parsec.char '(')
      (Parsec.char ')')
      (Parsec.many1 service_state)
  let service_states = Map.fromList services
  return $
    ThrottlingState
    { _serviceState = opsServiceStateFromString system_state
    , _quotaPerTrafficType = service_states
    }

parseThrottleStatement :: Text -> Session ThrottlingState
parseThrottleStatement input =
  case Parsec.parse throttleStatement (T.unpack input) input of
    (Right result) -> return result
    (Left err) -> do
      $(logError) [i|X-Throttle parsing failure: ${err}|]
      return $
        ThrottlingState
        {_serviceState = Overloaded, _quotaPerTrafficType = Map.empty}
