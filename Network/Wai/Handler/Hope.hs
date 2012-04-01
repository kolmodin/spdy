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

import Data.Conduit hiding (Done)
import Control.Monad.IO.Class (liftIO)

import Network.SPDY.Frame

import qualified Data.ByteString.Char8 as C8 ( pack ) -- Also IsString instance
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.Bits

import Network.Socket hiding ( recv, Closed )

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Control.Monad ( when )

import Control.Exception ( Exception, throwIO, Handler(..), catches )
import Data.Typeable

import Codec.Zlib
import Data.Text.Encoding ( encodeUtf8, decodeUtf8 )
import qualified Data.Text as T
import qualified Data.Text.Read as T

import qualified Data.List as List
import System.IO ( IOMode(ReadWriteMode), Handle )

import qualified Data.CaseInsensitive as CA ( foldedCase, mk )

import Network.TLS ( TLSCtx )
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSE
import Crypto.Random ( newGenIO, SystemRandom )

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A

import qualified Data.Certificate.X509 as X509
import qualified Data.Certificate.PEM as PEM
import qualified Data.Certificate.KeyRSA as KeyRSA

import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as Map

import Prelude hiding ( catch )

readCertificate :: FilePath -> IO X509.X509
readCertificate filepath = do
    content <- S.readFile filepath
    certdata <-
        case PEM.parsePEMCert content of
            Nothing -> error "no valid certificate section"
            Just x  -> return x
    case X509.decodeCertificate $ L.fromChunks [certdata] of
        Left err -> error ("cannot decode certificate: " ++ err)
        Right x  -> return x

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    content <- S.readFile filepath
    pkdata <-
        case PEM.parsePEMKeyRSA content of
            Nothing -> error "no valid RSA key section"
            Just x  -> return (L.fromChunks [x])
    case KeyRSA.decodePrivate pkdata of
        Left err -> error ("cannot decode key: " ++ err)
        Right (_pub, x)  -> return $ TLS.PrivRSA x


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
          cryptoR <- newGenIO :: IO SystemRandom
          tlsctx <- TLS.server (myParams cert pk) cryptoR handle
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
    TLS.defaultParams
      { TLS.pAllowedVersions = TLS.SSL3 : TLS.pAllowedVersions TLS.defaultParams
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

type FrameHandler = SessionState -> Frame -> IO SessionState

data Connection = Connection
  { connSend :: L.ByteString -> IO ()
  , connClose :: IO ()
  , connReceive :: IO S.ByteString
  }

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [IO Frame]  -- TODO(kolmodin): use a priority queue
  , sessionStateStreamStates :: HashMap Word32 StreamState
  , sessionStateNVHReceiveZContext :: Inflate
  , sessionStateNVHSendZContext :: Deflate
  , sessionStateNextValidSendID :: Word32
  , sessionStateNextValidReceiveID :: Word32
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
  deriving (Show,Typeable)

instance Exception SPDYException

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  zInflate <- liftIO $ initInflateWithDictionary defaultWindowBits nvhDictionary
  zDeflate <- liftIO $ initDeflateWithDictionary 6 nvhDictionary defaultWindowBits
  return $ SessionState queue Map.empty zInflate zDeflate 1 2

frameHandler :: Application -> SockAddr -> FrameHandler
frameHandler app sockaddr state frame = do
  print frame
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      state' <- createStream app sockaddr state flags sId pri nvh
      return state' { sessionStateLastValidReceiveID = sId }
    RstStreamControlFrame flags sId status -> do
      putStrLn "RstStream... we're screwed."
      -- TODO: remove all knowledge of this stream. empty send buffer.
      return state
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
            Just chan -> do writeChan chan (Just payload)
                            when flag_fin $ do
                              writeChan chan Nothing
                            let s' | flag_fin = s { streamStateBodyChan = Nothing }
                                   | otherwise = s
                                state' = updateStreamState state s'
                            return state'
    PingControlFrame pingId -> do
      enqueueFrame state $ return (PingControlFrame pingId)
      return state
    SettingsFrame flags values -> do
      return state
    GoAwayFrame flags lgsID -> do
      return state
    NoopControlFrame -> do
      return state

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

enqueueFrame :: SessionState -> IO Frame -> IO ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame =
  atomically $ do
    q <- readTVar queue
    writeTVar queue (q ++ [frame])

createStream :: Application -> SockAddr -> SessionState -> Word8 -> Word32 -> Word8 -> S.ByteString -> IO SessionState
createStream app sockaddr state@(SessionState { sessionStateNVHReceiveZContext = zInflate }) flags sId pri nvhBytes = do
  putStrLn $ "Creating stream context, id = " ++ show sId
  nvh <- decodeNVH =<< inflateWithFlush zInflate nvhBytes
  print (sId, pri, nvh)
  (tId, bodyChan) <- onSynStreamFrame app sockaddr state flags sId pri nvh
  let streamState = StreamState sId pri tId bodyChan
  return (insertStreamState state streamState)

inflateWithFlush :: Inflate -> S.ByteString -> IO [S.ByteString]
inflateWithFlush zInflate bytes = do
  a <- withInflateInput zInflate bytes popper
  b <- flushInflate zInflate
  return (a++[b])

deflateWithFlush :: Deflate -> L.ByteString -> IO S.ByteString
deflateWithFlush deflate lbs = do
  str <- withDeflateInput deflate (S.concat $ L.toChunks lbs) popper
  fl <- flushDeflate deflate popper
  return (S.concat (str ++ fl))

decodeNVH :: [S.ByteString] -> IO NameValueHeaderBlock
decodeNVH bytes = do
  case eof $ runGetPartial (runBitGet getNVHBlock) `feedAll` bytes of
    Done _ _ nvh -> return nvh
    Fail _ _ msg -> throwIO (SPDYNVHException Nothing msg)
    Partial _    -> throwIO (SPDYNVHException Nothing "Could not parse NVH block, returned Partial.")
  where
  feedAll r [] = r
  feedAll r (x:xs) = r `feed` x `feedAll` xs

popper :: Monad m => m (Maybe a) -> m [a]
popper io = go id
  where
  go front = do
    mChunk <- io
    case mChunk of
      Nothing -> return (front [])
      Just x -> go (front . (:) x)

sendGoAway :: SessionState -> Word32 -> IO ()
sendGoAway state sId = do
  enqueueFrame state $ return $ GoAwayFrame 0 sId

sendRstStream :: SessionState -> Word32 -> Word32 -> IO ()
sendRstStream state sId status = do
  enqueueFrame state $ return $ RstStreamControlFrame 0 sId status

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
    liftIO $ enqueueFrame state $ do
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
  mkDataFrame = DataFrame 0 sId
  enqueueFrameSink =
    sinkState
      ()
      (\_ input -> do
        case input of
          (Chunk inpBuilder) -> liftIO $ enqueueFrame state $ return $ mkDataFrame (toByteString inpBuilder)
          Flush -> return ()
        return (StateProcessing ()))
      (\_ -> liftIO $ enqueueFrame state $ return $ DataFrame 1 sId "")

buildReq :: SockAddr -> Source IO S.ByteString -> NameValueHeaderBlock -> Either String Request
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

mkChanSource :: ResourceIO m => IO (Source m S.ByteString, Chan (Maybe S.ByteString))
mkChanSource = do
  chan <- newChan
  return (chan2source chan, chan)

chan2source :: ResourceIO m => Chan (Maybe S.ByteString) -> Source m S.ByteString
chan2source chan =
  sourceIO
    (return ())
    (\_ -> return ())
    (\_ -> do v <- liftIO $ readChan chan
              case v of
                Nothing -> return IOClosed
                Just bs -> return (IOOpen bs))

sender :: Connection -> TVar [IO Frame] -> IO ()
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
        (frame:rest) ->
          do writeTVar queue rest
             return (frame, len-1)
        [] -> retry

sessionHandler :: FrameHandler -> Connection -> SockAddr -> IO ()
sessionHandler handler conn sockaddr = do
  initS <- initSession
  _ <- forkIO $ sender conn (sessionStateSendQueue initS)
  go initS (runGetPartial (runBitGet getFrame))
    `catches` [ Handler (\e ->
                   case e of
                     SPDYParseException str -> do putStrLn ("Caught this! " ++ show e)
                                                  sendGoAway initS 0
                     SPDYNVHException (Just sId) str -> do putStrLn ("Caught this! " ++ show e)
                                                           sendRstStream initS sId 1
                     SPDYNVHException Nothing    str -> do putStrLn ("Caught this! " ++ show e)
                                                           sendGoAway initS 0)
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
        go s' (runGetPartial (runBitGet getFrame) `feed` rest)
