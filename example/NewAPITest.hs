{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.MVar
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import System.IO (Handle, IOMode(WriteMode), openFile, hClose)


import           Network.SPDY.Base
import           Network.SPDY.Connection
import           Network.SPDY.Frame


main = do
  handle <- openFile "google-chrome.rpm" WriteMode
  (tlsctx, spdy) <- connect "dl.google.com" "443" (callbacks handle)
  sId <- sendRequest spdy
  getLine
  withMVar spdy $ \ session -> sendGoAway session sId
  getLine

callbacks :: Handle -> Callbacks
callbacks handle = Callbacks
  { cb_end_of_input = do
      putStrLn "eof in cb"
  , cb_recv_data_frame = \flags streamId payload -> do
      print ("data_frame", flags, streamId, "payload: " ++ show (L.length payload) ++ " bytes")
      L.hPut handle payload
      if (flags==1)
        then hClose handle >> putStrLn "done"
        else return ()
  , cb_recv_syn_frame = \flags streamId associatedStreamId priority nvh -> do
     print ("syn_frame", flags, streamId, associatedStreamId, priority, nvh)
  , cb_recv_syn_reply_frame = \flags streamId nvh -> do
      print ("syn_reply_frame", flags, streamId, nvh)
  }

sendRequest :: MVar Session -> IO StreamID
sendRequest spdy = modifyMVar spdy $ \session -> do
  sendSyn session 1 0 nvh

nvh :: NVH
nvh =
  [ ("host", "dl.google.com:443")
  , ("method", "GET")
  , ("url", "/linux/direct/google-chrome-stable_current_x86_64.rpm")
  , ("scheme", "https")
  , ("version", "HTTP/1.1")
  , ("accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
  , ("accept-charset", "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
  , ("accept-encoding", "gzip,deflate,sdch")
  , ("accept-language", "en-US,en;q=0.8")
  , ("user-agent", "hope/0.0.0.0")
  ]
