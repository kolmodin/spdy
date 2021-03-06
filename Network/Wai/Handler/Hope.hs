{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}
module Network.Wai.Handler.Hope where

import Network.Wai
import qualified Data.Vault as V
import Network.HTTP.Types ( Status(..), HttpVersion(..) )
import qualified Network.HTTP.Types as H

import Blaze.ByteString.Builder ( toByteString )

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Conduit
import Data.Conduit.Util
import Control.Monad.IO.Class (liftIO)

import Network.SPDY.Frame

import qualified Data.ByteString.Char8 as C8 ( pack )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Bits

import Data.Ord ( comparing )

import Network.Socket hiding ( recv, Closed )

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Control.Monad ( when )

import Control.Exception ( Exception, throwIO, Handler(..), catches )
import Data.Typeable
import Data.Either ( rights )

import Codec.Zlib
import Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import qualified Data.Text as T
import qualified Data.Text.Read as T

import qualified Data.List as List
import System.IO ( IOMode(ReadWriteMode), Handle )

import qualified Data.CaseInsensitive as CA ( foldedCase, mk )

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSE
import Crypto.Random.API ( getSystemRandomGen )

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A

import qualified Data.Certificate.X509 as X509
import qualified Data.PEM as PEM
import qualified Data.Certificate.KeyRSA as KeyRSA

import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as Map

import Prelude

readCertificate :: FilePath -> IO X509.X509
readCertificate filepath = do
    certs <- rights . parseCerts . PEM.pemParseBS <$> S.readFile filepath
    case certs of
        []    -> error "no valid certificate found"
        (x:_) -> return x
    where parseCerts (Right pems) = map (X509.decodeCertificate . L.fromChunks . (:[]) . PEM.pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . PEM.pemName) pems
          parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    pk <- rights . parseKey . PEM.pemParseBS <$> S.readFile filepath
    case pk of
        []    -> error "no valid RSA key found"
        (x:_) -> return x

    where parseKey (Right pems) = map (fmap (TLS.PrivRSA . snd) . KeyRSA.decodePrivate . L.fromChunks . (:[]) . PEM.pemContent)
                                $ filter ((== "RSA PRIVATE KEY") . PEM.pemName) pems
          parseKey (Left err) = error $ "Cannot parse PEM file: " ++ err

run :: String -> Application -> IO ()
run port app = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 10
  putStrLn ("SPDY Server listening to port " ++ port ++ ", " ++ show sock)

  cert    <- readCertificate "certificate.pem"
  pk      <- readPrivateKey  "key.pem"

  let loop = do
        (sock, sockaddr) <- accept sock
        _ <- forkIO $ do
          handle <- socketToHandle sock ReadWriteMode
          -- cryptoR <- newGenIO :: IO SystemRandom
          systemRandom <- getSystemRandomGen
          tlsctx <- TLS.contextNewOnHandle handle (myParams cert pk) systemRandom
          TLS.handshake tlsctx
          proto <- TLS.getNegotiatedProtocol tlsctx
          let conn = Connection { connSend = TLS.sendData tlsctx
                                , connClose = TLS.bye tlsctx >> sClose sock
                                , connReceive = TLS.recvData tlsctx
                                }
          case proto of
            Just "spdy/2" -> runWithConnection sockaddr conn app
            Just p -> liftIO $ putStrLn ("client suggested to not use spdy/2: " ++ show p)
            Nothing -> liftIO $ putStrLn "can't happen with chrome, client didn't use NPN"
        loop
  loop

  where
  myParams cert pk =
    TLS.defaultParamsServer
      { TLS.pAllowedVersions = TLS.SSL3 : TLS.pAllowedVersions TLS.defaultParamsServer
      , TLS.pCiphers = TLSE.ciphersuite_all
      , TLS.pCertificates    = [(cert, Just pk)]
      , TLS.onSuggestNextProtocols = return $ Just [ "spdy/2" ]
      , TLS.pLogging = TLS.defaultLogging
          -- { TLS.loggingPacketSent = \p -> putStrLn "sent:" >> putStrLn p
          -- , TLS.loggingPacketRecv = \p -> putStrLn "recv:" >> putStrLn p
          -- }
      }

runWithConnection :: SockAddr -> Connection -> Application -> IO ()
runWithConnection sockaddr conn app =
  sessionHandler (frameHandler app sockaddr) conn sockaddr

sessionHandler :: FrameHandler -> Connection -> SockAddr -> IO ()
sessionHandler handler conn sockaddr = do
  initS <- initSession
  _ <- forkIO $ sender conn (sessionStateSendQueue initS)
  go initS (runGetIncremental (runBitGet getFrame))
    `catches` [ Handler (\e ->
                   case e of
                     SPDYParseException str -> do putStrLn ("Caught this! " ++ show e)
                                                  sendGoAway initS 0
                     SPDYNVHException (Just sId) str -> do putStrLn ("Caught this! " ++ show e)
                                                           sendRstStream initS sId 1
                     SPDYNVHException Nothing    str -> do putStrLn ("Caught this! " ++ show e)
                                                           sendGoAway initS 0
                     SPDYSessionError str -> do putStrLn ("Ran into this: " ++ str)
                                                sendGoAway initS 0 -- protocol error
                     SPDYStreamError sId str -> do putStrLn ("Ran into this: " ++ str)
                                                   sendGoAway initS sId -- protocol error
                     )
              , Handler (\e ->
                   case e of
                     ZlibException n -> do putStrLn ("Caught this! " ++ show e)
                                           sendGoAway initS 0)
              ]
  where
  go s r =
    case r of
      Fail _ _ msg -> throwIO (SPDYParseException msg)
      Partial f -> do
        raw <- connReceive conn
        putStrLn ("Got " ++ show (S.length raw) ++ " bytes over the network")
        go s (f $ Just raw)
      Done rest _pos frame -> do
        putStrLn "Parsed frame."
        s' <- handler s frame
        go s' (runGetIncremental (runBitGet getFrame) `pushChunk` rest)

type FrameHandler = SessionState -> Frame -> IO SessionState

frameHandler :: Application -> SockAddr -> FrameHandler
frameHandler app sockaddr state frame = do
  print frame
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      -- checkAndSetLastValidReceiveId (sessionStateLastValidReceiveID state) sId
      state' <- createStream app sockaddr state flags sId pri nvh
      return state'
    RstStreamControlFrame flags sId status -> do
      putStrLn "RstStream... we're screwed."
      case getStreamState state sId of
        Nothing -> return ()
        Just st -> killThread (streamStateReplyThread st)
      resetFrame state sId
    DataFrame flags sId payload -> do
      let flag_fin = testBit flags 0
      case getStreamState state sId of
        Nothing -> do sendRstStream state sId 2 -- 2 == INVALID_STREAM
                      return state
        Just s -> do
          let bodyChan = streamStateBodyChan s
          case bodyChan of
            Nothing -> do sendRstStream state sId 2 -- which error code?
                          return state
            Just chan -> do writeChan chan (Just (L.toStrict payload))
                            when flag_fin $ do
                              writeChan chan Nothing
                            let s' | flag_fin = s { streamStateBodyChan = Nothing }
                                   | otherwise = s
                                state' = updateStreamState state s'
                            return state'
    PingControlFrame pingId -> do
      enqueueFrame state Nothing $ return (PingControlFrame pingId)
      return state
    SettingsFrame flags values -> do
      return state
    GoAwayFrame flags lgsID -> do
      return state
    NoopControlFrame -> do
      return state
    SynReplyControlFrame _ _ _ -> do
      return state

data Connection = Connection
  { connSend :: L.ByteString -> IO ()
  , connClose :: IO ()
  , connReceive :: IO S.ByteString
  }

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [(Word8, Maybe Word32, IO Frame)]  -- TODO(kolmodin): use a priority queue
  , sessionStateStreamStates :: HashMap Word32 StreamState
  , sessionStateNVHReceiveZContext :: Inflate
  , sessionStateNVHSendZContext :: Deflate
  , sessionStateLastValidSendID :: TVar Word32
  , sessionStateLastValidReceiveID :: TVar Word32
  }

data StreamState = StreamState
  { streamStateID :: Word32
  , streamStatePriority :: Word8
  , streamStateReplyThread :: ThreadId
  , streamStateBodyChan :: Maybe (Chan (Maybe S.ByteString))
  }

data SPDYException
  = SPDYParseException String
  | SPDYNVHException (Maybe Word32) String
  | SPDYStreamError Word32 String
  | SPDYSessionError String
  deriving (Show,Typeable)

instance Exception SPDYException

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  sendID <- newTVarIO 0
  receiveID <- newTVarIO 0
  zInflate <- liftIO $ initInflateWithDictionary defaultWindowBits nvhDictionary
  zDeflate <- liftIO $ initDeflateWithDictionary 6 nvhDictionary defaultWindowBits
  return $ SessionState queue Map.empty zInflate zDeflate sendID receiveID

resetFrame :: SessionState -> Word32 -> IO SessionState
resetFrame state rstStreamId = do
  let sendQVar = sessionStateSendQueue state
      streamStateMap = sessionStateStreamStates state
      streamStateMap' = Map.delete rstStreamId streamStateMap
  atomically $
    modifyTVar sendQVar (\lst -> [ (pri, sId,frame) | (pri, sId, frame) <- lst, sId /= Just rstStreamId ])
  return state { sessionStateStreamStates = streamStateMap' }

checkAndSetLastValidReceiveId :: Word32 -> Word32 -> Maybe SPDYException
checkAndSetLastValidReceiveId lastValidId newStreamId
  | not (odd newStreamId) =
      Just (SPDYStreamError newStreamId "New StreamID from client is not odd")
  | newStreamId <= lastValidId =
      Just (SPDYSessionError "New StreamID less or equal to last valid StreamID")
  | otherwise = Nothing

getStreamState :: SessionState -> Word32 -> Maybe StreamState
getStreamState state sId = Map.lookup sId (sessionStateStreamStates state)

updateStreamState :: SessionState -> StreamState -> SessionState
updateStreamState state stream =
  let streamStates = sessionStateStreamStates state
  in state { sessionStateStreamStates = Map.adjust (const stream) (streamStateID stream) streamStates }

insertStreamState :: SessionState -> StreamState -> SessionState
insertStreamState state stream =
  let streamStates = sessionStateStreamStates state
  in state { sessionStateStreamStates = Map.insert (streamStateID stream) stream streamStates }

enqueueFrame :: SessionState -> Maybe Word32 -> IO Frame -> IO ()
enqueueFrame state@(SessionState { sessionStateSendQueue = queue
                                 , sessionStateStreamStates = streamStates }) sId frame =
  atomically $ do
    q <- readTVar queue
    writeTVar queue (insertLastBy orderer (pri0, sId, frame) q)
  where
    pri0 = maybe 0 id $ do
      sId_ <- sId
      streamState <- Map.lookup sId_ streamStates
      return (streamStatePriority streamState)
    insertLastBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
    insertLastBy _   x [] = [x]
    insertLastBy cmp x ys@(y:ys')
     = case cmp x y of
         LT -> x : ys
         _  -> y : insertLastBy cmp x ys'
    orderer = comparing (\(pri, sId, _) -> (-pri, sId))

createStream :: Application -> SockAddr -> SessionState -> Word8 -> Word32 -> Word8 -> L.ByteString -> IO SessionState
createStream app sockaddr state@(SessionState { sessionStateNVHReceiveZContext = zInflate }) flags sId pri nvhBytes = do
  putStrLn $ "Creating stream context, id = " ++ show sId
  nvh <- decodeNVH =<< inflateWithFlush zInflate nvhBytes
  print (sId, pri, nvh)
  (tId, bodyChan) <- onSynStreamFrame app sockaddr state flags sId pri nvh
  let streamState = StreamState sId pri tId bodyChan
  return (insertStreamState state streamState)

inflateWithFlush :: Inflate -> L.ByteString -> IO [S.ByteString]
inflateWithFlush zInflate bytes = do
  let bytes' = L.toStrict bytes
  a <- unfoldM =<< feedInflate zInflate bytes'
  b <- flushInflate zInflate
  return (a++[b])

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM ma = ma >>= maybe (return []) (\x -> unfoldM ma >>= return . (:) x)

deflateWithFlush :: Deflate -> L.ByteString -> IO S.ByteString
deflateWithFlush deflate lbs = do
  str <- unfoldM =<< feedDeflate deflate (S.concat (L.toChunks lbs))
  fl <- unfoldM (flushDeflate deflate)
  return (S.concat (str ++ fl))

decodeNVH :: [S.ByteString] -> IO NameValueHeaderBlock
decodeNVH bytes = do
  case pushEndOfInput $ runGetIncremental (runBitGet getNVHBlock) `feedAll` bytes of
    Done _ _ nvh -> return nvh
    Fail _ _ msg -> throwIO (SPDYNVHException Nothing msg)
    Partial _    -> throwIO (SPDYNVHException Nothing "Could not parse NVH block, returned Partial.")
  where
  feedAll r [] = r
  feedAll r (x:xs) = r `pushChunk` x `feedAll` xs

sendGoAway :: SessionState -> Word32 -> IO ()
sendGoAway state sId = do
  enqueueFrame state Nothing $ return $ GoAwayFrame 0 sId

sendRstStream :: SessionState -> Word32 -> Word32 -> IO ()
sendRstStream state sId status = do
  enqueueFrame state (Just sId) $ return $ RstStreamControlFrame 0 sId status
  -- TODO: should we really pass sId and allow the rst frame to be removed
  -- from send queue?

onSynStreamFrame :: Application -> SockAddr -> SessionState -> Word8 -> Word32 -> Word8 -> NameValueHeaderBlock -> IO (ThreadId, Maybe (Chan (Maybe S.ByteString)))
onSynStreamFrame app sockaddr state flags sId pri nvh = do
  (bodySource,bodyChan) <- mkChanSource
  req <- case buildReq sockaddr bodySource nvh of -- catch errors, return protocol_error on stream
           Right req -> return req
           Left err -> throwIO (SPDYNVHException (Just sId) err)
  let flag_fin = testBit flags 0 -- other side said no more frames from their side
  when flag_fin $ do
    writeChan bodyChan Nothing
  tId <- forkIO $ runResourceT $ do
    resp <- app req
    let (status, responseHeaders, source) = responseSource resp
        headerStatus = ("status", showStatus status)
        headerVersion = ("version", "HTTP/1.1")
        headerServer = ("server", "Hope/0.0.0.0")
        appHeaders = [ (CA.foldedCase h, k) | (h,k) <- responseHeaders ]
    liftIO $ enqueueFrame state (Just sId) $ do
      let nvh' = List.sort $ map utf8 $ [headerStatus, headerVersion, headerServer] ++ appHeaders
      nvhReply <- deflateWithFlush (sessionStateNVHSendZContext state)
                                   (runPut (runBitPut (putNVHBlock nvh')))
      putStrLn "Constructed frame:"
      print ("syn_reply" :: String, sId, nvh')
      return (SynReplyControlFrame 0 sId nvhReply) :: IO Frame
    source $$ enqueueFrameSink
  return (tId, if flag_fin then Nothing else Just bodyChan)
  where
  utf8 (s,t) = (decodeUtf8 s, decodeUtf8 t)
  showStatus (Status statusCode statusMessage) = S.concat [C8.pack (show statusCode), " ", statusMessage]
  mkDataFrame = DataFrame 0 sId . L.fromStrict
  enqueueFrameSink =
    sinkState
      ()
      (\_ input -> do
        case input of
          (Chunk inpBuilder) -> liftIO $ enqueueFrame state (Just sId) $ return $ mkDataFrame (toByteString inpBuilder)
          Flush -> return ()
        return (StateProcessing ()))
      (\_ -> liftIO $ enqueueFrame state (Just sId) $ return $ DataFrame 1 sId "")

buildReq :: SockAddr -> Source (ResourceT IO) S.ByteString -> NameValueHeaderBlock -> Either String Request
buildReq sockaddr bodySource nvh = do
  method <- case lookup (decodeUtf8 "method") nvh of
              Just m -> return m
              Nothing -> Left "no method in NVH block"
  version <- case parseVersion <$> encodeUtf8 <$> lookup (decodeUtf8 "version") nvh of
               Just (Right v) -> return v
               Just (Left err) -> Left "could not parse http version in nvh block"
               Nothing -> Left "no http version in nvh block"

  (host,port') <-
    case T.split (==':') <$> lookup (decodeUtf8 "host") nvh of
      Just [host,port] -> return (encodeUtf8 host, T.decimal port)
      _                -> Left "no or invalid host in nvh block"

  port <- case port' of
            Right (p,"") -> return p
            _            -> Left "invalid port in nvh field 'host'"

  rawPath <- case lookup (decodeUtf8 "url") nvh of
               Just str -> return str
               Nothing -> Left "url parameter missing in nvh block"

  (path,query) <-
    case T.break (=='?') rawPath of
      ("", q) -> return ("/", q)
      (p,q) -> return (p,q)

  return Request
    { requestMethod = encodeUtf8 method
    , httpVersion = version
    , pathInfo = drop 1 (T.splitOn "/" path)
    , rawPathInfo = encodeUtf8 path
    , rawQueryString = encodeUtf8 query
    , serverName = host
    , serverPort = port
    , requestHeaders = [ (CA.mk (encodeUtf8 h), encodeUtf8 v) | (h,v) <- nvh ]
    , isSecure = True
    , remoteHost = sockaddr
    , queryString = H.parseQuery (encodeUtf8 query)
    , requestBody = bodySource
    , vault = V.empty
    }
 where
 parseVersion = A.parseOnly (do _ <- A.string "HTTP/"
                                x <- A.decimal
                                _ <- A.char '.'
                                y <- A.decimal
                                A.endOfInput
                                return $ HttpVersion x y)

mkChanSource :: MonadResource m => IO (Source m S.ByteString, Chan (Maybe S.ByteString))
mkChanSource = do
  chan <- newChan
  return (chan2source chan, chan)

chan2source :: MonadResource m => Chan (Maybe S.ByteString) -> Source m S.ByteString
chan2source chan =
  sourceIO
    (return ())
    (\_ -> return ())
    (\_ -> do v <- liftIO $ readChan chan
              case v of
                Nothing -> return IOClosed
                Just bs -> return (IOOpen bs))

sender :: Connection -> TVar [(Word8, Maybe Word32, IO Frame)] -> IO ()
sender conn queue = go
  where
  go = do
    (frameIO,len) <- getNextFrame
    putStrLn "Sending frame..."
    frame <- frameIO
    print frame
    putStrLn (show len ++ " more items in queue")
    connSend conn (runPut (runBitPut (putFrame frame)))
    go
  getNextFrame =
    atomically $ do
      q <- readTVar queue
      let len = length q
      case q of
        ((_, _,frame):rest) ->
          do writeTVar queue rest
             return (frame, len-1)
        [] -> retry
