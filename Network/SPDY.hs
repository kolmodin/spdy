{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Network.SPDY where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Conduit hiding (Done)
import Control.Monad.IO.Class (liftIO)

import Network.SPDY.Frame

import qualified Data.ByteString.Char8 as C8 ( pack ) -- Also IsString instance
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Network.Socket hiding ( recv )
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.ByteString.Lazy as NL

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Data.ByteString as S

import Codec.Zlib
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )

import Data.Monoid
import qualified Data.List as List
import System.IO ( IOMode(ReadWriteMode), hWaitForInput, Handle, hIsClosed, hFlush )

import Network.TLS ( TLSCtx )
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSE
import Crypto.Random ( newGenIO, SystemRandom )



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


server :: (TLSCtx Handle -> SockAddr -> ResourceT IO ()) -> String -> IO ()
server handler port = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 10
  putStrLn ("Listening to socket " ++ show sock)

  cert    <- readCertificate "certificate.pem"
  pk      <- readPrivateKey  "key.pem"

  let loop = do
        (conn, sockaddr) <- accept sock
        handle <- socketToHandle conn ReadWriteMode
        cryptoR <- newGenIO :: IO SystemRandom
        tlsctx <- TLS.server (myParams cert pk) cryptoR handle
        TLS.handshake tlsctx
        forkIO $ runResourceT $ handler tlsctx sockaddr
        loop
  loop

  where
  myParams cert pk =
    TLS.defaultParams
      { TLS.pAllowedVersions = TLS.SSL3 : TLS.pAllowedVersions TLS.defaultParams
      , TLS.pCiphers = TLSE.ciphersuite_all
      , TLS.pCertificates    = [(cert, Just pk)]
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
  -- , streamStateRecieveChan :: Chan S.ByteString
  }

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  zInflate <- liftIO $ initInflateWithDictionary defaultWindowBits nvhDictionary
  zDeflate <- liftIO $ initDeflateWithDictionary 6 nvhDictionary defaultWindowBits
  return $ SessionState queue [] zInflate zDeflate 1 2

frameHandler :: ResourceIO m => FrameHandler m
frameHandler state frame = do
  liftIO $ print frame
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      createStream state sId pri nvh
      return state
    RstStreamControlFrame flags sId status -> do
      liftIO $ putStrLn "RstStream... we're screwed."
      -- TODO: remove all knowledge of this stream. empty send buffer.
      return state
    PingControlFrame pingId -> do
      liftIO $ enqueueFrame state $ return (PingControlFrame pingId)
      return state

enqueueFrame :: SessionState -> IO Frame -> IO ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame =
  atomically $ do
    q <- readTVar queue
    writeTVar queue (q ++ [frame])

createStream :: ResourceIO m => SessionState -> Word32 -> Word8 -> S.ByteString -> ResourceT m SessionState
createStream state@(SessionState { sessionStateNVHReceiveZContext = zInflate }) sId pri nvhBytes = do
  liftIO $ putStrLn $ "Creating stream context, id = " ++ show sId
  nvhChunks <- liftIO $ do a <- withInflateInput zInflate nvhBytes popper
                           b <- flushInflate zInflate
                           return (a++[b])
  let streamState = StreamState sId pri
      Done _ _ nvh = eof $ runGetPartial (runBitGet getNVHBlock) `feedAll` nvhChunks
  liftIO $ print (sId, pri, nvh)
  liftIO $ do forkIO $ onSynStreamFrame state sId pri nvh
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

onSynStreamFrame :: SessionState -> Word32 -> Word8 -> NameValueHeaderBlock -> IO ()
onSynStreamFrame state sId pri nvh = do
  enqueueFrame state $ do
    let nvh' = List.sort $ map utf8 [ ("status","200 OK"),("version", "HTTP/1.1"), ("content-type", "text/html; charset=UTF-8"),("date", "Mon, 23 May 2005 22:38:34 GMT"),("server", "Apache/1.3.3.7 (Unix) (Red-Hat/Linux)")]
        nvhChunks = runPut (runBitPut (putNVHBlock nvh'))
    str <- withDeflateInput (sessionStateNVHSendZContext state) (S.concat $ L.toChunks nvhChunks) popper
    fl <- flushDeflate (sessionStateNVHSendZContext state) popper
    let nvhReply = S.concat (str ++ fl)
    putStrLn "Constructed frame:"
    print ("syn_reply", sId, nvh')
    return (SynReplyControlFrame 0 sId nvhReply) :: IO Frame
  enqueueFrame state $ return $ DataFrame 1 sId $ S.concat ("<html><h1>hello from spdy</h1><br/>" : S.concat ([ C8.pack (show b ++ "<br/>") | b <- nvh ]) : "</html>" : [])
  where
  utf8 (s,t) = (decodeUtf8 s, decodeUtf8 t)

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
  init <- liftIO $ initSession
  liftIO $ forkIO $ sender tlsctx (sessionStateSendQueue init)
  go init (runGetPartial (runBitGet getFrame))
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
