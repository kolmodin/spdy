module Network.SPDY where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Network.SPDY.Frame

import Network.Socket hiding ( recv )
import qualified Network.Socket.ByteString as NS
import qualified Network.Socket.ByteString.Lazy as NL

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Data.ByteString as S

import Data.Monoid

server :: (Socket -> SockAddr -> IO ()) -> String -> IO ()
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
        forkIO $ handler conn sockaddr
        loop
  loop

type SessionHandler = Socket -> SockAddr -> IO ()
type FrameHandler = SessionState -> Frame -> IO SessionState

data SessionState = SessionState
  { sessionStateSendQueue :: TVar [Frame]  -- TODO(kolmodin): use a priority queue
  }

initSession :: IO SessionState
initSession = do
  queue <- newTVarIO []
  return $ SessionState queue

frameHandler :: FrameHandler
frameHandler state frame = do
  case frame of
    SynStreamControlFrame flags sId assId pri nvh -> do
      print frame
      createSession state sId pri []
      let syn_reply = SynReplyControlFrame 0 sId S.empty
      -- enqueueFrame state syn_reply
      return state
    PingControlFrame pingId -> do
      print frame
      enqueueFrame state (PingControlFrame pingId)
      return state

enqueueFrame :: SessionState -> Frame -> IO ()
enqueueFrame SessionState { sessionStateSendQueue = queue } frame = do
  atomically $ do
    q <- readTVar queue
    writeTVar queue (frame:q)

createSession :: SessionState -> Word32 -> Word8 -> NameValueHeaderBlock -> IO ()
createSession state sId pri nvh = do
  putStrLn $ "Creating stream context, id = " ++ show sId

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

sessionHandler :: FrameHandler -> SessionHandler
sessionHandler handler conn sockaddr = do
  init <- initSession
  forkIO $ sender conn (sessionStateSendQueue init)
  go init (runGetPartial (runBitGet getFrame))
  where
  go s r =
    case r of
      Fail _ _ msg -> error msg
      Partial f -> do
        raw <- NS.recv conn (16 * 1024)
        go s (f $ Just raw)
      Done rest pos frame -> do
        putStrLn "Parsed frame."
        s' <- handler s frame
        go s' (runGetPartial (runBitGet getFrame) `feed` rest)
