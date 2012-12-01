{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Test.Framework
import Test.Framework.Runners.Console
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import Network.HTTP.Types
import Blaze.ByteString.Builder.ByteString ( fromByteString )
import Network.Wai
import Network.Socket

import Data.Binary.Bits.Put
import Data.Binary.Put
import Data.Binary.Bits.Get
import Data.Binary.Get
import Network.SPDY.Frame
import Network.Wai.Handler.Hope hiding ( run )


main :: IO ()
main = defaultMain allTests
-- run this from tests/QC.hs once the server is not so verbose in it's
-- output.

allTests :: [Test]
allTests =
  [ testGroup "Roundtrip tests"
    [ testProperty "Start server" prop_start
    ]
  ]

milliseconds = (*) 1000

prop_start :: Property
prop_start = within (milliseconds 100) $ monadicIO $ do
  pipe <- run mkPipe
  let sockaddr = SockAddrUnix "this-is-so-wrong"
      connB = connectionB pipe
      pushB = pushbackB pipe
  run $ forkIO (runWithConnection sockaddr (connectionA pipe) miniApp)
  run $ connSend connB . runPut . runBitPut . putFrame $ PingControlFrame 0 
  frame <- run (get connB pushB)
  case frame of
    PingControlFrame 0 -> return ()
    _ -> error "dafaq"

get conn pushback = go (runGetIncremental (runBitGet getFrame))
  where
  go r = do
   case r of
      Fail _ _ msg -> error "could not parse frame"
      Partial f -> do
        raw <- connReceive conn
        go (f $ Just raw)
      Done rest _pos frame -> do
        pushback rest
        return frame

-- application

miniApp :: Application
miniApp req = return (ResponseBuilder status200 [] (fromByteString "cowboy"))

-- pipe

data Pipe = Pipe (TChan B.ByteString) (TChan B.ByteString)

mkPipe :: IO Pipe
mkPipe = liftM2 Pipe newTChanIO newTChanIO

connectionA :: Pipe -> Connection
connectionA (Pipe a b) =
  Connection { connSend = \bs -> atomically $ mapM_ (writeTChan b) (L.toChunks bs)
             , connClose = return ()
             , connReceive = atomically $ readTChan a
             }

connectionB :: Pipe -> Connection
connectionB (Pipe a b) = connectionA (Pipe b a)

pushbackA :: Pipe -> B.ByteString -> IO ()
pushbackA (Pipe a b) = atomically . unGetTChan a

pushbackB :: Pipe -> B.ByteString -> IO ()
pushbackB (Pipe a b) = pushbackA (Pipe b a)
