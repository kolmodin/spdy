module Network.SPDY.Utils where

import Control.Monad hiding (join)
import Data.Char
import Data.List
import Numeric

import Data.Word

import qualified Data.ByteString.Lazy as L

w2c :: Word8 -> Char
w2c = chr . fromIntegral

printHexBS :: L.ByteString -> IO ()
printHexBS = printHex . map w2c . L.unpack

printHex :: String -> IO ()
printHex str0 = do
  putStr (join "" $ map (toHex' "") [0..lineLength-1])
  putStr "  "
  putStrLn $ join " " $ map (toHex' " ") [0..lineLength-1]
  loop str0
  where
  loop str = do
    let (line, rest) = splitAt lineLength str
    if null line
      then return ()
      else do
        printHexLine line
        loop rest
  lineLength = 4
  nicify :: String -> String
  nicify = map (\x -> if isAlphaNum x || x `elem` "/+-:!#$%^&*(){\"'<>, " then x else '.')
  join glue lst = concat $ intersperse glue lst
  printHexLine :: String -> IO ()
  printHexLine line = do
    putStr (nicify line)
    replicateM (lineLength - length line) (putStr " ")
    putStr "  "
    putStrLn $ join " " (map (toHex . ord) line)

toHex :: Int -> String
toHex i = toHex' "0" i

toHex' :: String -> Int -> String
toHex' filling w =
  case showHex w "" of
            w1:w2:[] -> w1:w2:[]
            w2:[]    -> filling ++ w2:[]
            _        -> error "showHex returned weird stuff"

-- vim: set ts=2 sw=2 tw=72 et ft=haskell :
