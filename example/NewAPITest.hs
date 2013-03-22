{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.MVar
import           Control.Monad           (when)
import qualified Data.ByteString.Lazy    as L
import           Data.IORef              (IORef, modifyIORef, newIORef,
                                          readIORef, writeIORef)
import           System.IO               (Handle, IOMode (WriteMode), hClose,
                                          openFile)


import           Network.SPDY.Base
import           Network.SPDY.Connection
import           Network.SPDY.Frame

main :: IO ()
main = do
  state <- newIORef []
  (_tlsctx, spdy) <- connect "dl.google.com" "443" (callbacks state)
  {-sId <- sendRequest spdy $ \streamId -> do
    handle <- openFile "google-chrome.rpm" WriteMode
    modifyIORef state ((streamId,handle):)
  -}
  submitPing spdy
  getLine
  return ()

callbacks :: IORef [(StreamID,Handle)] -> Callbacks
callbacks stateRef = Callbacks
  { cb_end_of_input =
      putStrLn "eof in cb"
  , cb_recv_data_frame = \flags streamId payload -> do
      print ("data_frame"::String, flags, streamId, "payload: " ++ show (L.length payload) ++ " bytes")
      state <- readIORef stateRef
      let Just handle = lookup streamId state
      L.hPut handle payload
      when (flags==1) $ do
        hClose handle
        writeIORef stateRef [(sid, h) | (sid,h) <- state, sid /= streamId]
        putStrLn "done"
  , cb_recv_syn_frame = \flags streamId associatedStreamId priority nvh ->
     print ("syn_frame"::String, flags, streamId, associatedStreamId, priority, nvh)
  , cb_recv_syn_reply_frame = \flags streamId nvh ->
      print ("syn_reply_frame"::String, flags, streamId, nvh)
  , cb_recv_ping_frame = \pingId sentTime repliedTime -> do
      let delta = realToFrac repliedTime - realToFrac sentTime
      print ("ping reply"::String, pingId, sentTime, repliedTime, delta*1000)
  , cb_settings_frame = \flags settings -> print ("settiongs"::String, flags, settings)
  , cb_rst_frame = \flags streamId code -> print ("rst"::String, flags, streamId, code)
  , cb_go_away = \flags streamId -> print ("go away"::String, flags, streamId)
  }

sendRequest :: SpdySession -> (StreamID -> IO ()) -> IO StreamID
sendRequest spdy successCb = do
  streamIDMVar <- newEmptyMVar
  submitRequest spdy 0 nvh Nothing
    (\streamId -> do
        putMVar streamIDMVar streamId
        successCb streamId)
    (\reason -> putMVar streamIDMVar (error "ups..."))
  streamId <- takeMVar streamIDMVar
  return $! streamId

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
