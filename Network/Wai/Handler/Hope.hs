{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
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

import Network.Socket hiding ( recv, Closed )

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM

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
        (conn, sockaddr) <- accept sock
        _ <- forkIO $ runResourceT $ do
          handle <- liftIO $ socketToHandle conn ReadWriteMode
          cryptoR <- liftIO (newGenIO :: IO SystemRandom)
          tlsctx <- TLS.server (myParams cert pk) cryptoR handle
          TLS.handshake tlsctx
          proto <- TLS.getNegotiatedProtocol tlsctx
          case proto of
            Just "spdy/2" -> (sessionHandler (frameHandler app sockaddr)) tlsctx sockaddr
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

type FrameHandler m = SessionState -> Frame -> ResourceT m SessionState

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [IO Frame]  -- TODO(kolmodin): use a priority queue
  , sessionStateStreamStates :: [StreamState]
  , sessionStateNVHReceiveZContext :: Inflate
  , sessionStateNVHSendZContext :: Deflate
  , sessionStateNextValidSendID :: Word32
  , sessionStateNextValidReceiveID :: Word32
  }

data StreamState = StreamState
  { streamStateID :: Word32
  , streamStatePriority :: Word8
  , streamStateReplyThread :: ThreadId
  }

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  zInflate <- liftIO $ initInflateWithDictionary defaultWindowBits nvhDictionary
  zDeflate <- liftIO $ initDeflateWithDictionary 6 nvhDictionary defaultWindowBits
  return $ SessionState queue [] zInflate zDeflate 1 2

frameHandler :: ResourceIO m => Application -> SockAddr -> FrameHandler m
frameHandler app sockaddr state frame = do
  liftIO $ print frame
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      state' <- createStream app sockaddr state sId pri nvh
      return state'
    RstStreamControlFrame flags sId status -> do
      liftIO $ putStrLn "RstStream... we're screwed."
      -- TODO: remove all knowledge of this stream. empty send buffer.
      return state
    PingControlFrame pingId -> do
      liftIO $ enqueueFrame state $ return (PingControlFrame pingId)
      return state
    SettingsFrame flags values -> do
      return state

enqueueFrame :: SessionState -> IO Frame -> IO ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame =
  atomically $ do
    q <- readTVar queue
    writeTVar queue (q ++ [frame])

createStream :: ResourceIO m => Application -> SockAddr -> SessionState -> Word32 -> Word8 -> S.ByteString -> ResourceT m SessionState
createStream app sockaddr state@(SessionState { sessionStateNVHReceiveZContext = zInflate }) sId pri nvhBytes = do
  liftIO $ putStrLn $ "Creating stream context, id = " ++ show sId
  nvhChunks <- liftIO $ do a <- withInflateInput zInflate nvhBytes popper
                           b <- flushInflate zInflate
                           return (a++[b])
  let Done _ _ nvh = eof $ runGetPartial (runBitGet getNVHBlock) `feedAll` nvhChunks
  liftIO $ print (sId, pri, nvh)
  tId <- liftIO $ forkIO $ onSynStreamFrame app sockaddr state sId pri nvh
  let streamState = StreamState sId pri tId
  return state { sessionStateStreamStates = streamState : sessionStateStreamStates state }
  where
  feedAll r [] = r
  feedAll r (x:xs) = r `feed` x `feedAll` xs

popper io = go id
  where
  go front = do
    mChunk <- io
    case mChunk of
      Nothing -> return (front [])
      Just x -> go (front . (:) x)

onSynStreamFrame :: Application -> SockAddr -> SessionState -> Word32 -> Word8 -> NameValueHeaderBlock -> IO ()
onSynStreamFrame app sockaddr state sId pri nvh = do
  req <- buildReq sockaddr nvh -- catch errors, return protocol_error on stream
  runResourceT $ do
    resp <- app req
    let (status, responseHeaders, source) = responseSource resp
        headerStatus = ("status", showStatus status)
        headerVersion = ("version", "HTTP/1.1")
        headerServer = ("server", "Hope/0.0.0.0")
        appHeaders = [ (CA.foldedCase h, k) | (h,k) <- responseHeaders ]
    liftIO $ enqueueFrame state $ do
      let nvh' = List.sort $ map utf8 $ [headerStatus, headerVersion, headerServer] ++ appHeaders
          nvhChunks = runPut (runBitPut (putNVHBlock nvh'))
      str <- withDeflateInput (sessionStateNVHSendZContext state) (S.concat $ L.toChunks nvhChunks) popper
      fl <- flushDeflate (sessionStateNVHSendZContext state) popper
      let nvhReply = S.concat (str ++ fl)
      putStrLn "Constructed frame:"
      print ("syn_reply" :: String, sId, nvh')
      return (SynReplyControlFrame 0 sId nvhReply) :: IO Frame
    source $$ enqueueFrameSink
  where
  utf8 (s,t) = (decodeUtf8 s, decodeUtf8 t)
  showStatus (Status statusCode statusMessage) = S.concat [C8.pack (show statusCode), " ", statusMessage]
  mkDataFrame = DataFrame sId 0
  enqueueFrameSink =
    sinkState
      ()
      (\_ input -> do
        case input of
          (Chunk inpBuilder) -> liftIO $ enqueueFrame state $ return $ mkDataFrame (toByteString inpBuilder)
          Flush -> return ()
        return (StateProcessing ()))
      (\_ -> liftIO $ enqueueFrame state $ return $ DataFrame sId 1 "")

-- Throws errors on failures.
buildReq :: SockAddr -> NameValueHeaderBlock -> IO Request
buildReq sockaddr nvh = do
  method <- case lookup (decodeUtf8 "method") nvh of
              Just m -> return m
              Nothing -> fail "no method in NVH block"
  version <- case parseVersion <$> encodeUtf8 <$> lookup (decodeUtf8 "version") nvh of
               Just (Right v) -> return v
               Just (Left err) -> fail "could not parse http version in nvh block"
               Nothing -> fail "no http version in nvh block"

  (host,port') <-
    case T.split (==':') <$> lookup (decodeUtf8 "host") nvh of
      Just [host,port] -> return (encodeUtf8 host, T.decimal port)
      _                -> fail "no or invalid host in nvh block"

  port <- case port' of
            Right (p,"") -> return p
            _            -> fail "invalid port in nvh field 'host'"
 
  rawPath <- case lookup (decodeUtf8 "url") nvh of
               Just str -> return str
               Nothing -> fail "url parameter missing in nvh block"

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
    , requestBody = sourceState () (\_ -> return StateClosed)
    , vault = V.empty
    }
 where
 parseVersion = A.parseOnly (do _ <- A.string "HTTP/"
                                x <- A.decimal
                                _ <- A.char '.'
                                y <- A.decimal
                                A.endOfInput
                                return $ HttpVersion x y)

sender :: TLSCtx a -> TVar [IO Frame] -> IO ()
sender tlsctx queue = go
  where
  go = do
    (frameIO,len) <- getNextFrame
    putStrLn "Sending frame..."
    frame <- frameIO
    print frame
    putStrLn (show len ++ " more items in queue")
    TLS.sendData tlsctx (runPut (runBitPut (putFrame frame)))
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

sessionHandler :: ResourceIO m => FrameHandler m -> TLSCtx Handle -> SockAddr -> ResourceT m ()
sessionHandler handler tlsctx sockaddr = do
  initS <- liftIO $ initSession
  liftIO $ forkIO $ sender tlsctx (sessionStateSendQueue initS)
  go initS (runGetPartial (runBitGet getFrame))
  where
  go s r =
    case r of
      Fail _ _ msg -> error msg
      Partial f -> do
        raw <- TLS.recvData tlsctx
        liftIO $ putStrLn ("Got " ++ show (S.length raw) ++ " bytes over the network, tls socket " ++ show (TLS.ctxConnection tlsctx))
        go s (f $ Just raw)
      Done rest _pos frame -> do
        liftIO $ putStrLn "Parsed frame."
        s' <- handler s frame
        go s' (runGetPartial (runBitGet getFrame) `feed` rest)
