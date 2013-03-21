
module Network.SPDY.NVH
  ( encodeNVH
  , decodeNVH
  ) where

-- haskell platform
import           Control.Monad        (liftM)
import           Data.Binary.Get      (runGetOrFail)
import           Data.Binary.Put      (runPut)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.IORef           (IORef, readIORef, writeIORef)

-- 3rd party
import qualified Codec.Zlib           as Z
import           Data.Binary.Bits.Get (runBitGet)
import           Data.Binary.Bits.Put (runBitPut)

-- this library
import           Network.SPDY.Frame   (NVH, NameValueHeaderBlock, getNVHBlock,
                                       nvhDictionary, putNVHBlock)

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