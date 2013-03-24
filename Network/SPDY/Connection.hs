{-# LANGUAGE OverloadedStrings #-}

module Network.SPDY.Connection where

-- haskell platform
import           Control.Concurrent.MVar
import qualified Network.Socket          as Socket
import           System.IO               (IOMode (ReadWriteMode))

-- 3rd party
import           Crypto.Random.AESCtr    (makeSystem)
import           Network.TLS             (TLSCtx)
import qualified Network.TLS             as TLS
import qualified Network.TLS.Extra       as TLSE

-- this library
import           Network.SPDY.Base
import           Network.SPDY.Binary
import           Network.SPDY.TLS

connect :: String -> String -> Callbacks -> IO (TLSCtx, MVar Session)
connect host port cb = do
  (addrinfo:_) <- Socket.getAddrInfo Nothing (Just host) (Just port)
  sock <- Socket.socket (Socket.addrFamily addrinfo) Socket.Stream Socket.defaultProtocol
  Socket.connect sock (Socket.addrAddress addrinfo)
  handle <- Socket.socketToHandle sock ReadWriteMode
  systemRandom <- makeSystem

  cert    <- readCertificate "certificate.pem"
  pk      <- readPrivateKey  "key.pem"

  tlsctx <- TLS.contextNewOnHandle handle (myParams cert pk) systemRandom
  TLS.handshake tlsctx
  prot <- TLS.getNegotiatedProtocol tlsctx
  print prot
  (is, os) <- makeTLSStreams tlsctx

  ifs <- streamInFrame is
  ofs <- streamOutFrame os

  session <- newClientFromCallbacks ifs ofs cb

  return (tlsctx, session)

  where
    myParams cert pk =
      TLS.defaultParamsClient
        { TLS.pAllowedVersions = TLS.SSL3 : TLS.pAllowedVersions TLS.defaultParamsServer
        , TLS.pCiphers = TLSE.ciphersuite_all
        , TLS.pCertificates    = [(cert, Just pk)]
        , TLS.onNPNServerSuggest = Just $ \ protos -> do
             putStrLn $ "Server suggests: " ++ show protos
             return "spdy/2"
        , TLS.pLogging = TLS.defaultLogging
        }
