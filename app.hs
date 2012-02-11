{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Hope as Hope (run)
import Network.Wai.Handler.Warp as Warp (run)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import Control.Monad.IO.Class (liftIO)

import Network.Wai.Application.Static
import System.Environment
import Control.Concurrent ( forkIO )

import Data.CaseInsensitive ( mk )

app :: Application
app _ = do
  liftIO $ putStrLn "I've done some IO here"
  return $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, Web!"

app2 :: Application
app2 = staticApp defaultFileServerSettings

main :: IO ()
main = do
  args <- getArgs
  port <- case args of
    [portS] -> return portS
    [] -> return "2000"
  putStrLn $ "http://localhost:" ++ port
  mergedRun port app2


mergedRun port app = do
  let spdyPort = show (read port + 1)
  forkIO $ Hope.run spdyPort app
  Warp.run (read port) (advertiseSPDY spdyPort app)
  return ()

advertiseSPDY :: String -> Application -> Application
advertiseSPDY spdyPort app request = do
  resp <- app request
  return $ case resp of
    ResponseFile    s h fpath fpart -> ResponseFile    s (alternateProtocol:h) fpath fpart
    ResponseBuilder s h builder     -> ResponseBuilder s (alternateProtocol:h) builder
    ResponseSource  s h source      -> ResponseSource  s (alternateProtocol:h) source
  where
  alternateProtocol = (mk "Alternate-Protocol", B.concat [C8.pack spdyPort, ":npn-spdy/2"])
  
