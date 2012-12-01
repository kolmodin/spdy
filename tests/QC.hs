module Main ( main ) where

import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )

import Data.Text ( Text )

import Network.SPDY.Frame

import Test.Framework
import Test.Framework.Runners.Console
import Test.Framework.Providers.QuickCheck2
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

main :: IO ()
main = defaultMain allTests

allTests :: [Test]
allTests =
  [ testGroup "Wire"
      [ testProperty "Name Value Header Block" prop_roundtrip_nvh
      , testGroup "Frame"
          [ testProperty "Data Frame"       (arbitraryDataFrame           >>= prop_roundtrip_frame)
          , testProperty "Syn Stream Frame" (arbitrarySynStreamFrame      >>= prop_roundtrip_frame)
          , testProperty "Syn Reply Frame"  (arbitrarySynReplyStreamFrame >>= prop_roundtrip_frame)
          , testProperty "Rst Stream Frame" (arbitraryRstStreamFrame      >>= prop_roundtrip_frame)
          , testProperty "Go Away Frame"    (arbitraryGoAwayFrame         >>= prop_roundtrip_frame)
          , testProperty "Settings Frame"   (arbitrarySettingsFrame       >>= prop_roundtrip_frame)
          ]
      ]
  ]


-- Properties.

prop_roundtrip_nvh :: [(Text,Text)] -> Property
prop_roundtrip_nvh = roundtrip_bit putNVHBlock getNVHBlock

prop_roundtrip_frame :: Frame -> Property
prop_roundtrip_frame = roundtrip_bit putFrame getFrame

-- Instances.

instance Arbitrary Frame where
  arbitrary = do
    oneof [ arbitraryDataFrame
          , arbitrarySynStreamFrame
          , arbitrarySynReplyStreamFrame
          , arbitraryRstStreamFrame
          ]

instance Arbitrary Text where
  arbitrary = do str <- genBS 1 10
                 return (decodeUtf8 str)

-- Utilities.

roundtrip :: Eq a => (a -> b) -> (b -> Decoder a) -> a -> Property
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
    (\bss -> pushEndOfInput $ runGetIncremental reader `pushChunk` S.concat bss)

roundtrip_bit :: Eq a => (a -> BitPut ()) -> BitGet a -> a -> Property
roundtrip_bit writer reader =
  roundtrip_binary
    (runBitPut . writer)
    (runBitGet   reader)

-- Frames.

arbitraryDataFrame :: Gen Frame
arbitraryDataFrame = do
 sId <- arbitraryWord31be 
 flags <- arbitrary
 payload <- genPayload
 return (DataFrame flags sId payload)

arbitrarySynStreamFrame :: Gen Frame
arbitrarySynStreamFrame = do
  flags <- arbitrary
  sId <- arbitraryWord31be
  aId <- arbitraryWord31be
  pri <- arbitraryPriority
  nvh <- genBS 2 200
  return (SynStreamControlFrame flags sId aId pri nvh)

arbitrarySynReplyStreamFrame :: Gen Frame
arbitrarySynReplyStreamFrame = do
  flags <- arbitrary
  sId <- arbitraryWord31be
  nvh <- genBS 2 200
  return (SynReplyControlFrame flags sId nvh)

arbitraryRstStreamFrame :: Gen Frame
arbitraryRstStreamFrame = do
  sId <- arbitraryWord31be
  status <- arbitrary
  return (RstStreamControlFrame 0 sId status)

arbitraryGoAwayFrame :: Gen Frame
arbitraryGoAwayFrame = do
  flags <- arbitrary
  sId <- arbitraryWord31be
  return (GoAwayFrame flags sId)

arbitrarySettingsFrame :: Gen Frame
arbitrarySettingsFrame = do
  flags <- arbitrary
  values <- listOf arbitrarySettingValue
  return (SettingsFrame flags values)

--

arbitrarySettingValue :: Gen (Word32, Word8, Word32)
arbitrarySettingValue = do
  id_ <- choose (0,2^24-1)
  flag <- arbitrary
  value <- arbitrary
  return (id_, flag, value)

arbitraryPriority :: Gen Word8
arbitraryPriority = do
  v <- choose (0, 2^2 - 1) :: Gen Integer
  return (fromIntegral v)

arbitraryWord31be :: Gen Word32
arbitraryWord31be = do
  v <- choose (0, 2^31 - 1) :: Gen Integer
  return (fromIntegral v)

genPayload = do
  len <- choose (0,100)
  str <- sequence (replicate len $ choose (0,255))
  return (S.pack str)

genBS from to = do
  len <- choose (from,to)
  str <- sequence (replicate len $ choose ('a','z'))
  return (S8.pack str)
