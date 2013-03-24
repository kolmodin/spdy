

module Network.SPDY.Binary (streamInFrame, streamOutFrame) where

-- haskell platform
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary.Get
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L

-- 3rd party
import           Data.Binary.Bits.Get
import           Data.Binary.Bits.Put   (runBitPut)
import           Data.Binary.Put        (runPut)
import           System.IO.Streams      (InputStream, OutputStream, write)
import qualified System.IO.Streams      as Streams

-- this library
import           Network.SPDY.Frame

streamInFrame :: InputStream B.ByteString -> IO (InputStream Frame)
streamInFrame inp = Streams.fromGenerator (go decoder)
  where
    decoder = runGetIncremental (runBitGet getFrame)
    go dec = do
      chunkM <- liftIO (Streams.read inp)
      case chunkM of
        Nothing -> return ()
        Just bs ->
          case pushChunk dec bs of
            Done bs' _ frame -> do
              Streams.yield frame
              go (pushChunk decoder bs')
            Fail _bs' _pos errmsg ->
              liftIO $ print $ "Failed decoding of frames" ++ errmsg
            k@(Partial _) -> go k

streamOutFrame :: OutputStream L.ByteString -> IO (OutputStream Frame)
streamOutFrame outp = Streams.makeOutputStream go
  where
    go Nothing = write Nothing outp
    go (Just frame) = do
      let lbs = runPut (runBitPut (putFrame frame))
      write (Just lbs) outp
