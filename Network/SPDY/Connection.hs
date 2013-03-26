{-# LANGUAGE OverloadedStrings #-}

module Network.SPDY.Connection where

-- haskell platform
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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

connect :: String -> String -> Callbacks -> IO (TLSCtx, SpdySession)
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

server :: String -> (Socket.SockAddr -> Callbacks) -> IO ()
server port cb = do
  (addrinfo:_) <- Socket.getAddrInfo
                    (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
                   Nothing (Just port)
  sock <- Socket.socket (Socket.addrFamily addrinfo) Socket.Stream Socket.defaultProtocol
  Socket.bindSocket sock (Socket.addrAddress addrinfo)
  Socket.listen sock 10
  putStrLn ("SPDY Server listening to port " ++ port ++ ", " ++ show sock)

  cert    <- readCertificate "certificate.pem"
  pk      <- readPrivateKey  "key.pem"

  let loop = do
        (csock, sockAddr) <- Socket.accept sock
        _ <- forkIO $ do
          handle <- Socket.socketToHandle csock ReadWriteMode
          systemRandom <- makeSystem
          tlsctx <- TLS.contextNewOnHandle handle (myParams cert pk) systemRandom
          TLS.handshake tlsctx
          proto <- TLS.getNegotiatedProtocol tlsctx
          case proto of
            Just "spdy/2" -> do
              (is, os) <- makeTLSStreams tlsctx
              ifs <- streamInFrame is
              ofs <- streamOutFrame os
              lock <- newEmptyMVar
              let cb' sa = let cb0 = cb sa
                           in cb0 { cb_end_of_input = do
                                      cb_end_of_input cb0
                                      -- TODO: exception handling
                                      putMVar lock () }
              _ <- newClientFromCallbacks ifs ofs (cb' sockAddr)
              -- return (tlsctx, session)
              takeMVar lock
              return ()
            Just p -> putStrLn ("client suggested to not use spdy/2: " ++ show p)
            Nothing -> putStrLn "can't happen with chrome, client didn't use NPN"
        loop
  loop

  where
  myParams cert pk =
    TLS.defaultParamsServer
      { TLS.pAllowedVersions = TLS.SSL3 : TLS.pAllowedVersions TLS.defaultParamsServer
      , TLS.pCiphers = TLSE.ciphersuite_all
      , TLS.pCertificates    = [(cert, Just pk)]
      , TLS.onSuggestNextProtocols = return $ Just [ "spdy/2" ]
      , TLS.pLogging = TLS.defaultLogging
          -- { TLS.loggingPacketSent = \p -> putStrLn "sent:" >> putStrLn p
          -- , TLS.loggingPacketRecv = \p -> putStrLn "recv:" >> putStrLn p
          -- }
      }
