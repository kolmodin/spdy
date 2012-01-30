module Main where

import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )

import Data.Text ( Text )

import Network.SPDY.Frame

import Test.QuickCheck hiding ( Result )

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 ()
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Word

-- Properties.

prop_roundtrip_nvh :: [(Text,Text)] -> Property
prop_roundtrip_nvh = roundtrip_bit putNVHBlock getNVHBlock

prop_roundtrip_frame :: Frame -> Property
prop_roundtrip_frame = roundtrip_bit putFrame getFrame

-- Instances.

instance Arbitrary Frame where
  arbitrary = do
    oneof [ arbitraryDataFrame ]

instance Arbitrary Text where
  arbitrary = do str <- genName
                 return (decodeUtf8 str)

-- Utilities.

roundtrip :: Eq a => (a -> b) -> (b -> Result a) -> a -> Property
roundtrip writer reader x = property $
  let result = reader (writer x)
  in case result of
       Done left _ x' | x /= x' -> False
                      | S.length left /= 0 -> False
                      | otherwise -> True

roundtrip_binary :: Eq a => (a -> Put) -> Get a -> a -> Property
roundtrip_binary writer reader =
  roundtrip
    (L.toChunks . runPut . writer)
    (\bss -> eof $ runGetPartial reader `feed` S.concat bss)

roundtrip_bit :: Eq a => (a -> BitPut ()) -> BitGet a -> a -> Property
roundtrip_bit writer reader =
  roundtrip_binary
    (runBitPut . writer)
    (runBitGet   reader)

arbitraryDataFrame :: Gen Frame
arbitraryDataFrame = do
 sId <- arbitraryWord31be 
 flags <- arbitrary
 payload <- genPayload
 return (DataFrame sId flags payload)

arbitraryWord31be :: Gen Word32
arbitraryWord31be = do
  v <- choose (0, 2^31 - 1) :: Gen Integer
  return (fromIntegral v)

genPayload = do
  len <- choose (0,100)
  str <- sequence (replicate len $ choose (0,255))
  return (S.pack str)

genName = do
  len <- choose (1,10)
  str <- sequence (replicate len $ choose ('a','z'))
  return (S8.pack str)
