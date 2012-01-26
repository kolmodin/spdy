module Main where

import Network.SPDY

import System.Environment

main = do
  args <- getArgs
  port <- case args of
    [portS] -> return portS
    [] -> return "2000"
  server (sessionHandler frameHandler) port
