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

import Data.ByteString.Char8 () -- IsString instance
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

server :: (Socket -> SockAddr -> ResourceT IO ()) -> String -> IO ()
server handler port = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 10
  putStrLn ("Listening to socket " ++ show sock)
  let loop = do
        (conn, sockaddr) <- accept sock
        forkIO $ runResourceT $ handler conn sockaddr
        loop
  loop

type FrameHandler m = SessionState -> Frame -> ResourceT m SessionState

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [IO Frame]  -- TODO(kolmodin): use a priority queue
  , sessionStateStreamStates :: [StreamState]
  , sessionStateNVHReceiveZContext :: Inflate
  , sessionStateNVHSendZContext :: Deflate
  , sessionStateLastSentID :: Word32
  , sessionStateLastRecievedID :: Word32
  }

data StreamState = StreamState
  { streamStateID :: Word32
  , streamStatePriority :: Word8
  , streamStateRecieveChan :: Chan S.ByteString
  }

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  zInflate <- liftIO $ initInflateWithDictionary defaultWindowBits nvhDictionary
  zDeflate <- liftIO $ initDeflateWithDictionary 6 nvhDictionary defaultWindowBits
  return $ SessionState queue [] zInflate zDeflate (-1) 0

frameHandler :: ResourceIO m => FrameHandler m
frameHandler state frame = do
  liftIO $ print frame
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      createStream state sId pri nvh
      let fr = do let nvh' = List.sort $ map utf8 [ ("status","200 OK"),("version", "HTTP/1.1"), ("content-type", "text/html; charset=UTF-8"),("date", "Mon, 23 May 2005 22:38:34 GMT"),("server", "Apache/1.3.3.7 (Unix) (Red-Hat/Linux)")]
                      nvhChunks = runPut (runBitPut (putNVHBlock nvh'))
                  str <- withDeflateInput (sessionStateNVHSendZContext state) (S.concat $ L.toChunks nvhChunks) popper
                  fl <- flushDeflate (sessionStateNVHSendZContext state) popper
                  let nvhReply = S.concat (str ++ fl)
                  putStrLn "Constructed frame:"
                  print ("syn_reply", sId, nvh')
                  return $ SynReplyControlFrame 0 sId nvhReply
      enqueueFrame state fr
      enqueueFrame state $ return $ DataFrame sId 1 "hello from spdy"
      return state
    RstStreamControlFrame flags sId status -> do
      liftIO $ putStrLn "RstStream... we're screwed."
      -- TODO: remove all knowledge of this stream. empty send buffer.
      return state
    PingControlFrame pingId -> do
      enqueueFrame state $ return (PingControlFrame pingId)
      return state
  where
  utf8 (s,t) = (decodeUtf8 s, decodeUtf8 t)

enqueueFrame :: ResourceIO m => SessionState -> IO Frame -> ResourceT m ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame =
  liftIO $ atomically $ do
    q <- readTVar queue
    writeTVar queue (q ++ [frame])

createStream :: ResourceIO m => SessionState -> Word32 -> Word8 -> S.ByteString -> ResourceT m SessionState
createStream state@(SessionState { sessionStateNVHReceiveZContext = zInflate }) sId pri nvhBytes = do
  liftIO $ putStrLn $ "Creating stream context, id = " ++ show sId
  nvhChunks <- liftIO $ do a <- withInflateInput zInflate nvhBytes popper
                           b <- flushInflate zInflate
                           return (a++[b])
  receiveChan <- liftIO $ newChan
  sendChan <- liftIO $ newChan
  let streamState = StreamState sId pri receiveChan
      Done _ _ nvh = eof $ runGetPartial (runBitGet getNVHBlock) `feedAll` nvhChunks
  liftIO $ print (sId, pri, nvh)
  liftIO $ do forkIO $ clientHandler receiveChan nvh
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

clientHandler :: Chan S.ByteString -> NameValueHeaderBlock -> IO ()
clientHandler chan nvh = return ()

sender :: Handle -> TVar [IO Frame] -> IO ()
sender handle queue = go
  where
  go = do
    (frameIO,len) <- getNextFrame
    putStrLn "Sending frame..."
    frame <- frameIO
    print frame
    putStrLn (show len ++ " more items in queue")
    L.hPut handle (runPut (runBitPut (putFrame frame)))
    hFlush handle
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

sessionHandler :: ResourceIO m => FrameHandler m -> Socket -> SockAddr -> ResourceT m ()
sessionHandler handler conn sockaddr = do
  init <- liftIO $ initSession
  handle <- liftIO $ socketToHandle conn ReadWriteMode
  liftIO $ forkIO $ sender handle (sessionStateSendQueue init)
  go handle init (runGetPartial (runBitGet getFrame))
  where
  go handle s r =
    case r of
      Fail _ _ msg -> error msg
      Partial f -> do
        raw <- liftIO $ do hWaitForInput handle (-1)
                           S.hGetSome handle (4 * 1024)
        liftIO $ putStrLn ("Got " ++ show (S.length raw) ++ " bytes over the network, socket " ++ show conn)
        go handle s (f $ Just raw)
      Done rest _pos frame -> do
        liftIO $ putStrLn "Parsed frame."
        s' <- handler s frame
        go handle s' (runGetPartial (runBitGet getFrame) `feed` rest)
