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

import Network.Socket hiding ( recv )
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.ByteString.Lazy as NL

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Data.ByteString as S

import Data.Monoid

server :: (Socket -> SockAddr -> ResourceT IO ()) -> String -> IO ()
server handler port = withSocketsDo $ do
  addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  let loop = do
        (conn, sockaddr) <- accept sock
        forkIO $ runResourceT $ handler conn sockaddr
        loop
  loop

type FrameHandler m = SessionState -> Frame -> ResourceT m SessionState

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [Frame]  -- TODO(kolmodin): use a priority queue
  }

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  return $ SessionState queue

frameHandler :: ResourceIO m => FrameHandler m
frameHandler state frame = do
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      liftIO $ print frame
      createSession state sId pri []
      let syn_reply = SynReplyControlFrame 0 sId S.empty
      -- enqueueFrame state syn_reply
      return state
    RstStreamControlFrame flags sId status -> do
      liftIO $ putStrLn "RstStream... we're screwed."
      liftIO $ print frame
      -- TODO: remove all knowledge of this stream. empty send buffer.
      return state
    PingControlFrame pingId -> do
      liftIO $ print frame
      enqueueFrame state (PingControlFrame pingId)
      return state

enqueueFrame :: ResourceIO m => SessionState -> Frame -> ResourceT m ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame =
  liftIO $ atomically $ do
    q <- readTVar queue
    writeTVar queue (frame:q)

createSession :: ResourceIO m => SessionState -> Word32 -> Word8 -> NameValueHeaderBlock -> ResourceT m ()
createSession state sId pri nvh = do
  liftIO $ putStrLn $ "Creating stream context, id = " ++ show sId

sender :: Socket -> TVar [Frame] -> IO ()
sender socket queue = go
  where
  go = do
    frame <- getNextFrame
    putStrLn "Sending frame..."
    print frame
    NL.sendAll socket (runPut (runBitPut (putFrame frame)))
    go
  getNextFrame =
    atomically $ do
      q <- readTVar queue
      case q of
        (frame:rest) ->
          do writeTVar queue rest
             return frame
        [] -> retry

sessionHandler :: ResourceIO m => FrameHandler m -> Socket -> SockAddr -> ResourceT m ()
sessionHandler handler conn sockaddr = do
  init <- liftIO $ initSession
  liftIO $ forkIO $ sender conn (sessionStateSendQueue init)
  go init (runGetPartial (runBitGet getFrame))
  where
  go s r =
    case r of
      Fail _ _ msg -> error msg
      Partial f -> do
        raw <- liftIO $ NS.recv conn (16 * 1024)
        go s (f $ Just raw)
      Done rest pos frame -> do
        liftIO $ putStrLn "Parsed frame."
        s' <- handler s frame
        go s' (runGetPartial (runBitGet getFrame) `feed` rest)
