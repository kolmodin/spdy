

module Network.SPDY.Base where

--std lib
import           Control.Concurrent          (ThreadId, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM      (atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (liftM)
import           Control.Monad               (when)
import           Data.Binary.Get             (runGetOrFail)
import           Data.Binary.Put             (runPut)
import           Data.Bits                   (testBit)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.Maybe                  (isJust)
import           Data.Ord                    (comparing)
import           Data.Word                   (Word32, Word8)

-- 3rd party
import qualified Codec.Zlib                  as Z
import           Data.Binary.Bits.Get        (runBitGet)
import           Data.Binary.Bits.Put        (runBitPut)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           System.IO.Streams           (InputStream, OutputStream)
import qualified System.IO.Streams           as Streams

-- this library
import           Network.SPDY.Frame

type Queue = TVar [(Priority, Maybe StreamID, OutgoingFrame)]

data FailReason = ServerSentGoAway

data OutgoingFrame
  = OutgoingSynFrame (StreamID -> IO ()) (FailReason -> IO ()) (IO Frame)
  -- | OutgoingFrameIO (IO Frame)
  | OutgoingFrame Frame

data SpdyRole = SpdyServer | SpdyClient

type SpdySession = MVar Session

data Session = Session
  { sessionRole                 :: SpdyRole
  , sessionSendQueue            :: Queue
  , sessionLastUsedStreamID     :: StreamID
  , sessionLastReceivedStreamID :: StreamID
  , sessionActiveStreams        :: HashMap StreamID Stream
  , sessionReceiverThread       :: ThreadId
  , sessionSenderThread         :: ThreadId
  , sessionRecvNVHZContext      :: IORef (Maybe Z.Inflate)
  , sessionSendNVHZContext      :: IORef (Maybe Z.Deflate)
  , sessionNextUniquePingId     :: Word32
  }

data Stream = Stream
  { streamStreamID    :: StreamID
  , streamPriority    :: Priority
  , streamReceivedRst :: Bool
  , streamThisClosed  :: Bool
  , streamThatClosed  :: Bool
  }

data Callbacks = Callbacks
  { cb_end_of_input         :: IO ()
  , cb_recv_data_frame      :: Flags -> StreamID -> L.ByteString -> IO ()
  , cb_recv_syn_frame       :: Flags -> StreamID -> StreamID -> Priority -> NVH -> IO ()
  , cb_recv_syn_reply_frame :: Flags -> StreamID -> NVH -> IO ()
  , cb_go_away              :: Flags -> StreamID -> IO ()
  , cb_settings_frame       :: Flags -> [(Word32, Word8, Word32)] -> IO ()
  , cb_rst_frame            :: Flags -> StreamID -> RstStreamStatusCode -> IO ()
  }

defaultSessionState :: SpdyRole -> Queue -> ThreadId -> ThreadId -> IO Session
defaultSessionState role queue receiverId senderId = do
  zinf <- newIORef Nothing
  zdef <- newIORef Nothing
  return $ Session role queue (-1) 0 Map.empty receiverId senderId zinf zdef (ping role)
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
            -- TODO: verify that there indeed is such a stream
            cb_recv_data_frame cb flags streamID payload
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
            let match SpdyServer = odd pingID -- remote is client
                match SpdyClient = even pingID -- remote is server
            role <- myRole sessionMVar
            when (match role) $ do
                queue <- getQueue sessionMVar
                enqueueFrame queue maxBound Nothing
                  (OutgoingFrame (PingControlFrame pingID))
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
  enqueueFrame (sessionSendQueue session) maxBound Nothing $
    OutgoingFrame $ PingControlFrame pingID
  return (session { sessionNextUniquePingId = pingID + 2 }, pingID)

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
sendSyn sessionMVar fl pri nvh successCb failCb = do
  queue <- getQueue sessionMVar
  enqueueFrame queue pri Nothing $ OutgoingSynFrame successCb failCb $ do
    (defl, nextSID) <- modifyMVar sessionMVar $ \session -> do
      let nextSID = sessionLastUsedStreamID session + 2
          nextSession = session { sessionLastUsedStreamID = nextSID }
      return (nextSession, (sessionSendNVHZContext session, nextSID))
    nvhBytes <- encodeNVH nvh defl
    return $ SynStreamControlFrame fl nextSID 0 pri nvhBytes

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

encodeNVH :: NVH -> IORef (Maybe Z.Deflate) -> IO L.ByteString
encodeNVH nvh deflateRef = do
  deflate <- getDeflate deflateRef
  deflateWithFlush deflate $
    runPut (runBitPut (putNVHBlock nvh))

decodeNVH :: L.ByteString -> IORef (Maybe Z.Inflate) -> IO NameValueHeaderBlock
decodeNVH bytes inflateRef = do
  inflate <- getInflate inflateRef
  bytes' <- inflateWithFlush inflate (L.toStrict bytes)
  case runGetOrFail (runBitGet getNVHBlock) bytes' of
    Right (_, _, nvh) -> return nvh
    -- Fail _ _ msg -> throwIO (SPDYNVHException Nothing msg)
    -- Partial _    -> throwIO (SPDYNVHException Nothing "Could not parse NVH block, returned Partial.")

getOrMkNew :: IO a -> IORef (Maybe a) -> IO a
getOrMkNew new ref = do
  v <- readIORef ref
  case v of
    Just x -> return x
    Nothing -> do
      x <- new
      writeIORef ref (Just x)
      return x

mkInflate :: IO Z.Inflate
mkInflate = Z.initInflateWithDictionary Z.defaultWindowBits nvhDictionary

mkDeflate :: IO Z.Deflate
mkDeflate = Z.initDeflateWithDictionary 6 nvhDictionary Z.defaultWindowBits

getDeflate :: IORef (Maybe Z.Deflate) -> IO Z.Deflate
getDeflate = getOrMkNew mkDeflate

getInflate :: IORef (Maybe Z.Inflate) -> IO Z.Inflate
getInflate = getOrMkNew mkInflate

inflateWithFlush :: Z.Inflate -> B.ByteString -> IO L.ByteString
inflateWithFlush zInflate bytes = do
  a <- unfoldM =<< Z.feedInflate zInflate bytes
  b <- Z.flushInflate zInflate
  return $ L.fromChunks (a++[b])

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM ma = ma >>= maybe (return []) (\x -> liftM ((:) x) (unfoldM ma))

deflateWithFlush :: Z.Deflate -> L.ByteString -> IO L.ByteString
deflateWithFlush deflate lbs = do
  str <- unfoldM =<< Z.feedDeflate deflate (B.concat (L.toChunks lbs))
  fl <- unfoldM (Z.flushDeflate deflate)
  return (L.fromChunks (str ++ fl))
