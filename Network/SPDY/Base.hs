

module Network.SPDY.Base where

--std lib
import           Control.Concurrent          (ThreadId, forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM      (atomically, retry)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (liftM)
import           Data.Binary.Get             (runGetOrFail)
import           Data.Binary.Put             (runPut)
import           Data.Bits                   (testBit)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as L
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)
import           Data.Ord                    (comparing)
import           Data.Word                   (Word8,Word32)

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

type Queue = TVar [(Priority, Maybe StreamID, IO Frame)]

data SpdyRole = SpdyServer | SpdyClient

data Session = Session
  { sessionSendQueue            :: Queue
  , sessionLastUsedStreamID     :: StreamID
  , sessionLastReceivedStreamID :: StreamID
  , sessionActiveStreams        :: HashMap StreamID Stream
  , sessionReceiverThread       :: ThreadId
  , sessionSenderThread         :: ThreadId
  , sessionRecvNVHZContext      :: IORef (Maybe Z.Inflate)
  , sessionSendNVHZContext      :: IORef (Maybe Z.Deflate)
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

defaultSessionState :: Queue -> ThreadId -> ThreadId -> IO Session
defaultSessionState queue receiverId senderId = do
  zinf <- newIORef Nothing
  zdef <- newIORef Nothing
  return $ Session queue (-1) 0 Map.empty receiverId senderId zinf zdef

newClientFromCallbacks :: InputStream Frame -> OutputStream L.ByteString -> Callbacks -> IO (MVar Session)
newClientFromCallbacks inp out cb = do
  sessionMVar <- newEmptyMVar
  queue <- newTVarIO []
  receiverThreadId <- forkIO $ receiver sessionMVar inp cb
  senderThreadId <- forkIO $ sender out queue
  session <- defaultSessionState queue receiverThreadId senderThreadId
  putMVar sessionMVar session
  return sessionMVar

receiver :: MVar Session -> InputStream Frame -> Callbacks -> IO ()
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
            withMVar sessionMVar $ \ session -> do
              enqueueFrame session maxBound Nothing (return (PingControlFrame pingID))
            go
          NoopControlFrame -> go

sender :: OutputStream L.ByteString -> TVar [(Priority, Maybe StreamID, IO Frame)] -> IO ()
sender out queue = go
  where
  go = do
    (frameIO,len) <- getNextFrame
    --putStrLn "Sending frame..."
    frame <- frameIO
    --print $ C8.pack $ ">>> " ++ show frame
    --putStrLn (show len ++ " more items in queue")
    Streams.write (Just (runPut (runBitPut (putFrame frame)))) out
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

sendSyn :: Session -> Flags -> Priority -> NVH -> IO (Session, StreamID)
sendSyn session fl pri nvh = do
  let nextSID = sessionLastUsedStreamID session + 2
      nextSession = session { sessionLastUsedStreamID = nextSID }
  enqueueFrame session pri (Just nextSID) $ do
    nvhBytes <- encodeNVH nvh (sessionSendNVHZContext session)
    let frame = SynStreamControlFrame fl nextSID 0 pri nvhBytes
    return frame
  return (nextSession, nextSID)

sendGoAway :: Session -> StreamID -> IO ()
sendGoAway session lastGoodStreamID = do
  enqueueFrame session 10 Nothing $ do
    return $ GoAwayFrame 0 lastGoodStreamID

enqueueFrame :: Session -> Priority -> Maybe StreamID -> IO Frame -> IO ()
enqueueFrame session pri0 sId0 frame =
  atomically $ do
    let queue = sessionSendQueue session
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

mkStream :: StreamID -> Priority -> IO Stream
mkStream sid pri = do
  return $ Stream sid pri False False False

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