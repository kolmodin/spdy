

module Network.SPDY.Binary (streamFrame) where

-- haskell platform
import           Control.Monad.IO.Class (liftIO)
import           Data.Binary.Get
import qualified Data.ByteString        as B

-- 3rd party
import           Data.Binary.Bits.Get
import           System.IO.Streams      (InputStream)
import qualified System.IO.Streams      as Streams

-- this library
import           Network.SPDY.Frame

streamFrame :: InputStream B.ByteString -> IO (InputStream Frame)
streamFrame inp = Streams.fromGenerator (go decoder)
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
            Fail bs' pos errmsg ->
              liftIO $ print "Failed decoding of frames"
            k@(Partial _) -> go k
