
module Network.SPDY.TLS where

-- haskell platform
import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import           Data.Either

-- 3rd party
import           Network.TLS             (TLSCtx)
import qualified Network.TLS             as TLS
import           System.IO.Streams       (InputStream, OutputStream)
import qualified System.IO.Streams       as Streams

import qualified Data.Certificate.KeyRSA as KeyRSA
import qualified Data.Certificate.X509   as X509
import qualified Data.PEM                as PEM


makeTLSStreams :: TLSCtx -> IO (InputStream B.ByteString, OutputStream L.ByteString)
makeTLSStreams tlsctx = do
  is <- Streams.makeInputStream input
  os <- Streams.makeOutputStream output
  return $! (is, os)
  where
    input = do
      recv <- try $ TLS.recvData tlsctx
      case recv of
        Right bs -> return $! if B.null bs then Nothing else Just bs
        Left e ->
          case e of
            TLS.Error_EOF -> return Nothing

    output Nothing = return $! ()
    output (Just lbs) =
      TLS.sendData tlsctx lbs

readCertificate :: FilePath -> IO X509.X509
readCertificate filepath = do
    certs <- rights . parseCerts . PEM.pemParseBS <$> B.readFile filepath
    case certs of
        []    -> error "no valid certificate found"
        (x:_) -> return x
    where parseCerts (Right pems) = map (X509.decodeCertificate . L.fromChunks . (:[]) . PEM.pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . PEM.pemName) pems
          parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    pk <- rights . parseKey . PEM.pemParseBS <$> B.readFile filepath
    case pk of
        []    -> error "no valid RSA key found"
        (x:_) -> return x

    where parseKey (Right pems) = map (fmap (TLS.PrivRSA . snd) . KeyRSA.decodePrivate . L.fromChunks . (:[]) . PEM.pemContent)
                                $ filter ((== "RSA PRIVATE KEY") . PEM.pemName) pems
          parseKey (Left err) = error $ "Cannot parse PEM file: " ++ err
