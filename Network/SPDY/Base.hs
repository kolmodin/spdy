

module Network.SPDY.Base where

--std lib
import           Control.Concurrent          (ThreadId, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM      (atomically, retry)
import           Control.Concurrent.STM.TVar
import           Data.Binary.Put             (runPut)
import           Data.Bits                   (testBit)
import           Data.Bits                   ((.&.))
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import           Data.IORef                  (IORef, newIORef)
import           Data.Maybe                  (isJust)
import           Data.Ord                    (comparing)
import           Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime)
import           Data.Word                   (Word32, Word8)

-- 3rd party
import qualified Codec.Zlib                  as Z
import           Data.Binary.Bits.Put        (runBitPut)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           System.IO.Streams           (InputStream, OutputStream)
import qualified System.IO.Streams           as Streams

-- this library
import           Network.SPDY.Frame
import           Network.SPDY.NVH

type Queue = TVar [(Priority, Maybe StreamID, OutgoingFrame)]

data FailReason = ServerSentGoAway

data OutgoingFrame
  = OutgoingSynFrame (StreamID -> IO ()) (FailReason -> IO ()) (IO Frame)
  | OutgoingFrameCb (IO ()) Frame
  | OutgoingFrame Frame

data SpdyRole = SpdyServer | SpdyClient

type SpdySession = MVar Session

data Session = Session
  { sessionRole                 :: SpdyRole
  , sessionSendQueue            :: Queue
  , sessionLastUsedStreamID     :: StreamID
  , sessionLastReceivedStreamID :: StreamID
  , sessionIncomingStreams      :: HashMap StreamID Stream
  , sessionOutgoingStreams      :: HashMap StreamID Stream
  , sessionReceiverThread       :: ThreadId
  , sessionSenderThread         :: ThreadId
  , sessionRecvNVHZContext      :: IORef (Maybe Z.Inflate)
  , sessionSendNVHZContext      :: IORef (Maybe Z.Deflate)
  , sessionOutgoingPings        :: [(PingID, MVar POSIXTime)]
  , sessionNextUniquePingId     :: Word32
  }

data Stream = Stream
  { streamStreamID    :: StreamID
  , streamPriority    :: Priority
  , streamReceivedRst :: Bool
  , streamThisClosed  :: Bool
  , streamThatClosed  :: Bool
  } deriving Eq

data Callbacks = Callbacks
  { cb_end_of_input         :: IO ()
  , cb_recv_data_frame      :: Flags -> StreamID -> L.ByteString -> IO ()
  , cb_recv_syn_frame       :: Flags -> StreamID -> StreamID -> Priority -> NVH -> IO ()
  , cb_recv_syn_reply_frame :: Flags -> StreamID -> NVH -> IO ()
  , cb_recv_ping_frame      :: PingID -> POSIXTime -> POSIXTime -> IO ()
  , cb_go_away              :: Flags -> StreamID -> IO ()
  , cb_settings_frame       :: Flags -> [(Word32, Word8, Word32)] -> IO ()
  , cb_rst_frame            :: Flags -> StreamID -> RstStreamStatusCode -> IO ()
  }

defaultSessionState :: SpdyRole -> Queue -> ThreadId -> ThreadId -> IO Session
defaultSessionState role queue receiverId senderId = do
  zinf <- newIORef Nothing
  zdef <- newIORef Nothing
  return $ Session role queue (-1) 0 Map.empty Map.empty receiverId senderId zinf zdef [] (ping role)
  where
    ping SpdyServer = 2
    ping SpdyClient = 1

newClientFromCallbacks :: InputStream Frame -> OutputStream L.ByteString -> Callbacks -> IO SpdySession
newClientFromCallbacks inp out cb = do
  sessionMVar <- newEmptyMVar
  queue <- newTVarIO []
  receiverThreadId <- forkIO $ receiver sessionMVar inp cb
  senderThreadId <- forkIO $ sender out queue
  session <- defaultSessionState SpdyClient queue receiverThreadId senderThreadId
  putMVar sessionMVar session
  return sessionMVar

receiver :: SpdySession -> InputStream Frame -> Callbacks -> IO ()
receiver sessionMVar inp cb = go
  where
  go = do
    frameM <- Streams.read inp
    case frameM of
      Nothing -> cb_end_of_input cb
      Just frame ->
        case frame of
          DataFrame flags streamID payload -> do
            let flag_fin = testBit flags 0
            modifyMVar_ sessionMVar $ \ session -> do
              let streams = sessionOutgoingStreams session
              let streamM = do
                    stream <- Map.lookup streamID streams
                    if not (streamThatClosed stream)
                      then return stream
                      else Nothing
              case streamM of
                Nothing -> -- Stream does not exist, or is already closed.
                           -- Ignore this package.
                           return session
                Just _stream -> do
                  -- The stream does exist, and we do await more data frames.
                  cb_recv_data_frame cb flags streamID payload
                  if flag_fin
                    then return session { sessionOutgoingStreams =
                           Map.adjust (\stream -> stream { streamThatClosed = flag_fin }) streamID streams }
                    else return session
            go
          SynReplyControlFrame flags streamID nvhBytes -> do
            nvh <- withMVar sessionMVar $ \ session -> do
              let inflate = sessionRecvNVHZContext session
              decodeNVH (L.fromStrict nvhBytes) inflate
            cb_recv_syn_reply_frame cb flags streamID nvh
            go
          SynStreamControlFrame  flags streamId associatedStreamId priority nvhBytes -> do
            nvh <- withMVar sessionMVar $ \ session -> do
              let inflate = sessionRecvNVHZContext session
              decodeNVH nvhBytes inflate
            cb_recv_syn_frame cb flags streamId associatedStreamId priority nvh
            go
          GoAwayFrame flags lastGoodStreamId -> do
            cb_go_away cb flags lastGoodStreamId
            go
          SettingsFrame flags values -> do
            cb_settings_frame cb flags values
            go
          RstStreamControlFrame flags streamId statusCode -> do
            cb_rst_frame cb flags streamId statusCode
            go
          PingControlFrame pingID -> do
            role <- myRole sessionMVar
            let reply = do
                  queue <- getQueue sessionMVar
                  enqueueFrame queue maxBound Nothing
                    (OutgoingFrame (PingControlFrame pingID))
            case role of
              SpdyClient | even pingID -> reply -- remote is server
              SpdyServer | odd pingID -> reply -- remote is client
              _ -> modifyMVar sessionMVar $ \ session -> do
                let timeM = lookup pingID (sessionOutgoingPings session)
                    session' = session { sessionOutgoingPings =
                                 filter (\(pid, _) -> pid /= pingID)
                                        (sessionOutgoingPings session) }
                case timeM of
                  Nothing -> return (session, ())
                  Just timeMVar -> do
                    now <- getPOSIXTime
                    earlier <- readMVar timeMVar
                    cb_recv_ping_frame cb pingID earlier now
                    return (session', ())
            go
          NoopControlFrame -> go

myRole :: SpdySession -> IO SpdyRole
myRole = fmap sessionRole . readMVar

sender :: OutputStream L.ByteString -> Queue -> IO ()
sender out queue = go
  where
  go = do
    outFrame <- getNextFrame
    frame <- case outFrame of
      OutgoingSynFrame successCb _ frameIO -> do
        frame <- frameIO
        successCb (synStreamFrameStreamID frame)
        return frame
      OutgoingFrameCb cb frame -> do
        _ <- cb
        return frame
      OutgoingFrame frame -> return frame
    Streams.write (Just (runPut (runBitPut (putFrame frame)))) out
    go
  getNextFrame =
    atomically $ do
      q <- readTVar queue
      case q of
        ((_, _,frame):rest) ->
          do writeTVar queue rest
             return frame
        [] -> retry

submitPing :: SpdySession -> IO PingID
submitPing spdy = modifyMVar spdy $ \session -> do
  let pingID = sessionNextUniquePingId session
  timeRef <- newEmptyMVar
  enqueueFrame (sessionSendQueue session) maxBound Nothing $
    OutgoingFrameCb (updateTime timeRef) $ PingControlFrame pingID
  return (session { sessionNextUniquePingId = pingID + 2
                  , sessionOutgoingPings = addPing (pingID, timeRef) session}, pingID)
  where
    addPing tup session = tup : sessionOutgoingPings session
    updateTime ref = do
      newTime <- getPOSIXTime
      putMVar ref newTime

submitRequest :: SpdySession -> Priority -> NVH
              -> Maybe (InputStream B.ByteString)
              -> (StreamID -> IO ())
              -> (FailReason -> IO ())
              -> IO ()
submitRequest sessionMVar pri nvh postData successCb failCb = do
  sendSyn sessionMVar flags pri nvh successCb' failCb
  where
    hasData = isJust postData
    flags = if hasData then 0 else 1 -- flag_fin
    successCb' _streamId = successCb _streamId -- TODO: add postData to queue

sendSyn :: SpdySession -> Flags -> Priority -> NVH -> (StreamID -> IO ()) -> (FailReason -> IO ()) -> IO ()
sendSyn sessionMVar fl pri nvh successCb0 failCb = do
  queue <- getQueue sessionMVar
  enqueueFrame queue pri Nothing $ OutgoingSynFrame successCb failCb $ do
    (defl, nextSID) <- modifyMVar sessionMVar $ \session -> do
      let nextSID = sessionLastUsedStreamID session + 2
          nextSession = session { sessionLastUsedStreamID = nextSID }
      return (nextSession, (sessionSendNVHZContext session, nextSID))
    nvhBytes <- encodeNVH nvh defl
    return $ SynStreamControlFrame fl nextSID 0 pri nvhBytes
  where
    successCb streamId = do
      modifyMVar_ sessionMVar $ \session -> do
        let s = Stream
                  { streamStreamID = streamId
                  , streamPriority = pri
                  , streamReceivedRst = False
                  , streamThisClosed = fl .&. 1 /= 0
                  , streamThatClosed = False
                  }
            streams = Map.insert streamId s (sessionOutgoingStreams session)
        return session { sessionOutgoingStreams = streams }
      successCb0 streamId


getQueue :: SpdySession -> IO Queue
getQueue mvar = fmap sessionSendQueue (readMVar mvar)

sendGoAway :: Session -> StreamID -> IO ()
sendGoAway session lastGoodStreamID = do
  let queue = sessionSendQueue session
  enqueueFrame queue 10 Nothing $
    OutgoingFrame $ GoAwayFrame 0 lastGoodStreamID

enqueueFrame :: Queue -> Priority -> Maybe StreamID -> OutgoingFrame -> IO ()
enqueueFrame queue pri0 sId0 frame =
  atomically $ do
    q <- readTVar queue
    writeTVar queue (insertLastBy orderer (pri0, sId0, frame) q)
  where
    insertLastBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
    insertLastBy _   x [] = [x]
    insertLastBy cmp x ys@(y:ys')
     = case cmp x y of
         LT -> x : ys
         _  -> y : insertLastBy cmp x ys'
    orderer = comparing (\(pri, sId, _) -> (-(toInteger pri), sId))
