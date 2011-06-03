{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module Network.SPDY.Frame where

import Data.Binary.Get ( runGet )
import Data.Binary.Put ( runPut )

import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put

import Data.Text ( Text )
import qualified Data.Text.Encoding as Text

import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Bits
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative ( (<$>) )
import Control.Exception (assert)
import Control.Monad ( when, liftM2 )

-- import Network.SPDY.Utils

import Debug.Trace

data Frame 
  = DF DataFrame
  | CF ControlFrame
    deriving Show

data DataFrame = DataFrame {
    dataFrameStreamID :: Word32,
    dataFrameFlags :: Word8,
    dataFramePayload :: B.ByteString
  } deriving (Show, Eq)

data ControlFrame =
  ControlFrame {
    controlFrameVersion :: Word16,
    controlFrameType :: Word16,
    controlFrameFlags :: Word8,
    controlFrameLength :: Word32,
    controlFrameBody :: B.ByteString
  } deriving (Show, Eq)

data ControlFrameBody
  = SynStreamControlFrame {
      synStreamFrameStreamID :: Word32,
      synStreamFrameAssociatedStreamID :: Word32,
      synStreamFramePriority :: Word8,
      synStreamFrameNameValueHeaderBlock :: NameValueHeaderBlock }
  | SynReplyControlFrame {
      synReplyFrameStreamID :: Word32,
      synReplyFrameLength :: Word16 }
  | FinStreamControlFrame {
      finStreamFrameStreamID :: Word32,
      finStreamFrameStatus :: Word32 }
  | NoopControlFrame
  deriving (Show, Eq)

type NameValueHeaderBlock = [(Text,Text)]

getFrame :: BitGet Frame
getFrame = trace "getFrame" $ do
  c <- getWord8 1
  case c of
    0 -> DF <$> trace "entering dataFrame" dataFrame
    1 -> CF <$> trace "entering controlFrame" controlFrame

controlFrame :: BitGet ControlFrame
controlFrame = do
  trace "entered controlFrame" $ do
  controlFrameVersion <- getWord16be 15
  trace ("version = " ++ show controlFrameVersion) $ do
  controlFrameType <- getWord16be 16
  trace ("frame type = " ++ show controlFrameType) $ do
  controlFrameFlags <- getWord8 8
  trace ("frame flags = " ++ show controlFrameFlags) $ do
  controlFrameLength <- getWord32be 24
  trace ("frame length = " ++ show controlFrameLength) $ do
  controlFrameBody <- getByteString (fromIntegral controlFrameLength)
  trace ("frame body = " ++ show controlFrameBody ) $ do
  trace ("frame done.") $ do
  return ControlFrame { .. }


dataFrame :: BitGet DataFrame
dataFrame = do
  trace "entered dataFrame" $ do
  dataFrameStreamID <- getWord32be 31
  trace ("streamID = " ++ show dataFrameStreamID) $ do
  dataFrameFlags <- getWord8 8
  trace ("frameFlags = " ++ show dataFrameFlags) $ do
  len <- fromIntegral <$> getWord32be 24
  trace ("length = " ++ show len) $ do
  dataFramePayload <- getByteString len
  trace "dataFrame done!" $ do
  return DataFrame { .. }
      
rControlFrameBody :: Word16 -> BitGet ControlFrameBody
rControlFrameBody t =
  case t of
    1 -> do _ <- getWord8 1
            streamID <- getWord32be 31
            _ <- getWord8 1
            associatedToStreamID <- getWord32be 31
            priority <- getWord8 2
            _ <- getWord16be 14
            nvhBlock <- getNVHBlock
            return SynStreamControlFrame
              { synStreamFrameStreamID = streamID
              , synStreamFrameAssociatedStreamID = associatedToStreamID
              , synStreamFramePriority = priority
              , synStreamFrameNameValueHeaderBlock = nvhBlock
              }

getNVHBlock :: BitGet NameValueHeaderBlock
getNVHBlock = do
  blocks <- fromIntegral <$> getWord16be 16
  sequence (replicate blocks getNVHEntry)

getNVHEntry :: BitGet (Text,Text)
getNVHEntry = liftM2 (,) getText16 getText16

getText16 :: BitGet Text
getText16 = do
  len <- fromIntegral <$> getWord16be 16
  bs <- getByteString len
  return (Text.decodeUtf8 bs)

putFrame :: Frame -> BitPut ()
putFrame fr =
  case fr of
    DF df -> putDataFrame df
    CF cf -> putControlFrame cf

putDataFrame :: DataFrame -> BitPut ()
putDataFrame (DataFrame {..}) = do
  putWord8 1 0
  putWord32be 31 dataFrameStreamID
  putWord8 8 dataFrameFlags
  putWord32be 24 (fromIntegral (B.length dataFramePayload))
  joinPut (Put.putByteString dataFramePayload)

putControlFrame :: ControlFrame -> BitPut ()
putControlFrame = undefined


{-
putControlFrame :: ControlFrame -> Put
putControlFrame ControlFrame {..} =
  assert (controlFrameVersion <= 2 ^ 15) $ do
  putWord16be (controlFlagMask .|. controlFrameVersion)
  let controlFrameType =
        case controlFrameBody of
          SynStreamControlFrame {} -> 1
          SynReplyControlFrame {} -> 2
          FinStreamControlFrame {} -> 3
          -- Hello -> 4
          NoopControlFrame -> 5
  putWord16be controlFrameType
  putFlagsAndLength controlFrameFlags controlFrameLength

getControlFrameBody :: Word16 -> Get ControlFrameBody
getControlFrameBody type_ = do
  case type_ of
    1 -> -- syn stream
    2 -> -- syn reply
    3 -> -- rst stream
    4 -> -- settings
    5 -> -- noop
    6 -> -- ping
    7 -> -- go away
    8 -> -- headers
    9 -> -- window update
-}
