{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module Network.SPDY.Frame where

import Data.Binary.Get ( runGet, runGetPartial, Result(..), feed, eof )
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
import Data.Char ( ord )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative ( (<$>) )
import Control.Exception (assert)
import Control.Monad ( when, liftM2 )

-- import Codec.Compression.Zlib

-- import Network.SPDY.Utils

import Debug.Trace

type StreamID = Word32

ourSPDYVersion :: Word16
ourSPDYVersion = 2

data Frame
  = DataFrame {
      dataFrameStreamID :: Word32,
      dataFrameFlags :: Word8,
      dataFramePayload :: B.ByteString }
  | SynStreamControlFrame {
      controlFrameFlags :: Word8,
      synStreamFrameStreamID :: Word32,
      synStreamFrameAssociatedStreamID :: Word32,
      synStreamFramePriority :: Word8,
      synStreamFrameNameValueHeaderBlockCompressed :: B.ByteString }
      --synStreamFrameNameValueHeaderBlock :: NameValueHeaderBlock
  | SynReplyControlFrame {
      controlFrameFlags :: Word8,
      synReplyFrameStreamID :: Word32,
      synReplyFrameNameValueHeaderBlockCompressed :: B.ByteString }
      --synReplyFrameNameValueHeaderBlock :: NameValueHeaderBlock
  | RstStreamControlFrame {
      controlFrameFlags :: Word8,
      rstStreamFrameStreamID :: Word32,
      rstStreamFrameStatusCode :: RstStreamStatusCode }
  | FinStreamControlFrame {
      controlFrameFlags :: Word8,
      finStreamFrameStreamID :: Word32,
      finStreamFrameStatus :: Word32 }
  | PingControlFrame {
      pingControlFrameId :: Word32 }
  | NoopControlFrame
  deriving (Show, Eq)

data ControlFrameHeader =
  ControlFrameHeader {
    controlFrameVersion :: Word16,
    controlFrameType :: Word16,
    controlFrameFlags_ :: Word8,
    controlFrameLength :: Word32
  } deriving (Show, Eq)

type RstStreamStatusCode = Word32

{-
data RstStreamStatusCode
  = PROTOCOL_ERROR -- ^ This is a generic error, and should only be used if a more specific error is not available.
  | INVALID_STREAM -- ^ This is returned when a frame is received for a stream which is not active.
  | REFUSED_STREAM -- ^ Indicates that the stream was refused before any processing has been done on the stream.
  | UNSUPPORTED_VERSION -- ^ Indicates that the recipient of a stream does not support the SPDY version requested.
  | CANCEL -- ^ Used by the creator of a stream to indicate that the stream is no longer needed.
  | FLOW_CONTROL_ERROR -- ^ The endpoint detected that its peer violated the flow control protocol.
  | STREAM_IN_USE -- ^ The endpoint received a SYN_REPLY for a stream already open.
  | STREAM_ALREADY_CLOSED -- ^ The endpoint received a data or SYN_REPLY frame for a stream which is half closed.
  -}

type NameValueHeaderBlock = [(Text,Text)]

-- ** Get stuff

getFrame :: BitGet Frame
getFrame = trace "getFrame" $ do
  c <- getWord8 1
  case c of
    0 -> getDataFrame
    1 -> getControlFrame

getControlFrameHeader :: BitGet ControlFrameHeader
getControlFrameHeader = do
  trace "entered controlFrame" $ do
  controlFrameVersion <- getWord16be 15
  trace ("version = " ++ show controlFrameVersion) $ do
  controlFrameType <- getWord16be 16
  trace ("frame type = " ++ show controlFrameType) $ do
  controlFrameFlags_ <- getWord8 8
  trace ("frame flags = " ++ show controlFrameFlags_) $ do
  controlFrameLength <- getWord32be 24
  trace ("frame length = " ++ show controlFrameLength) $ do
  trace ("control frame header done.") $ do
  return ControlFrameHeader { .. }

getDataFrame :: BitGet Frame
getDataFrame = do
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

getControlFrame :: BitGet Frame
getControlFrame = do
  header <- getControlFrameHeader
  getControlFrameBody header

getControlFrameBody :: ControlFrameHeader -> BitGet Frame
getControlFrameBody header =
  case controlFrameType header of
    1 -> getSynStream header
    3 -> getRstStream header
    6 -> getPing

getSynStream :: ControlFrameHeader -> BitGet Frame
getSynStream header = do
  let controlFrameFlags = controlFrameFlags_ header
  trace "parsing syn_stream" $ do
  _ <- getWord8 1
  synStreamFrameStreamID <- getWord32be 31
  trace ("stream id = " ++ show synStreamFrameStreamID) $ do
  _ <- getWord8 1
  synStreamFrameAssociatedStreamID <- getWord32be 31
  trace ("associated to stream = " ++ show synStreamFrameAssociatedStreamID) $ do
  synStreamFramePriority <- getWord8 2
  trace ("priority = " ++ show synStreamFramePriority) $ do
  _ <- getWord16be 14
  trace "discarded 14 bits" $ do
  synStreamFrameNameValueHeaderBlockCompressed <-
    getByteString (fromIntegral (controlFrameLength header) - 10)
  trace ("compressed NVH block " ++ show (B.length synStreamFrameNameValueHeaderBlockCompressed) ++ " bytes") $ do
  return SynStreamControlFrame { .. }

getRstStream :: ControlFrameHeader -> BitGet Frame
getRstStream header = do
  let controlFrameFlags = controlFrameFlags_ header
  trace "parsing rst_stream" $ do
  _ <- getWord8 1 -- skipped
  rstStreamFrameStreamID <- getWord32be 31
  rstStreamFrameStatusCode <- getWord32be 32
  return RstStreamControlFrame { .. }

getPing :: BitGet Frame
getPing = do
  trace "parsing ping" $ do
  pingControlFrameId <- getWord32be 32
  return PingControlFrame { .. }

getNVHBlock :: BitGet NameValueHeaderBlock
getNVHBlock = trace "entering getNVHBlock" $ do
  blocks <- fromIntegral <$> getWord16be 16
  trace ("number of NV pairs = " ++ show blocks) $ do
  sequence (replicate blocks getNVHEntry)

getNVHEntry :: BitGet (Text,Text)
getNVHEntry = liftM2 (,) getText16 getText16

getText16 :: BitGet Text
getText16 = do
  len <- fromIntegral <$> getWord16be 16
  bs <- getByteString len
  return (Text.decodeUtf8 bs)

-- ** Put stuff

putFrame :: Frame -> BitPut ()
putFrame frame = do
  case frame of
    DataFrame { .. } -> do
      putBool False
      putWord32be 31 dataFrameStreamID
      putWord8 8 dataFrameFlags
      putWord32be 24 (fromIntegral $ B.length dataFramePayload)
      putByteString dataFramePayload

    SynStreamControlFrame { .. } -> do
      let payload = runPut $ runBitPut $ do
            putBool False -- ignored
            putWord32be 31 synStreamFrameStreamID
            putBool False -- ignored
            putWord32be 31 synStreamFrameAssociatedStreamID
            putWord8 2 synStreamFramePriority
            putWord16be 14 0 -- ignored
            putByteString synStreamFrameNameValueHeaderBlockCompressed
          header = mkControlHeader frame payload
      putControlFrameHeader (mkControlHeader frame payload)
      mapM_ putByteString (L.toChunks payload)

    SynReplyControlFrame { .. } -> do
      let payload = runPut $ runBitPut $ do
            putBool False -- ignored
            putWord32be 31 synReplyFrameStreamID
            putWord16be 16 0 -- ignored
            putByteString synReplyFrameNameValueHeaderBlockCompressed
      putControlFrameHeader (mkControlHeader frame payload)
      mapM_ putByteString (L.toChunks payload)

    PingControlFrame { .. } -> do
      let payload = runPut $ runBitPut $ do
            putWord32be 32 pingControlFrameId
      putControlFrameHeader (mkControlHeader frame payload)
      mapM_ putByteString (L.toChunks payload)

mkControlHeader :: Frame -> L.ByteString -> ControlFrameHeader
mkControlHeader frame payload = ControlFrameHeader
  { controlFrameVersion = ourSPDYVersion
  , controlFrameType = case frame of
                         SynStreamControlFrame {} -> 1
                         SynReplyControlFrame  {} -> 2
                         PingControlFrame      {} -> 6
  , controlFrameFlags_ = case frame of
                           PingControlFrame {} -> 0
                           _                   -> controlFrameFlags frame
  , controlFrameLength = payloadLength
  }
  where
  payloadLength = fromIntegral (L.length payload)


putControlFrameHeader :: ControlFrameHeader -> BitPut ()
putControlFrameHeader (ControlFrameHeader { .. }) = do
  putBool True
  putWord16be 15 ourSPDYVersion
  putWord16be 16 controlFrameType
  putWord8 8 controlFrameFlags_
  putWord32be 24 controlFrameLength

dictionary :: B.ByteString
dictionary =
  B.pack . map (fromIntegral . ord) . concat $
    [ "optionsgetheadpostputdeletetraceacceptaccept-charsetaccept-encodingaccept-"
    , "languageauthorizationexpectfromhostif-modified-sinceif-matchif-none-matchi"
    , "f-rangeif-unmodifiedsincemax-forwardsproxy-authorizationrangerefererteuser"
    , "-agent10010120020120220320420520630030130230330430530630740040140240340440"
    , "5406407408409410411412413414415416417500501502503504505accept-rangesageeta"
    , "glocationproxy-authenticatepublicretry-afterservervarywarningwww-authentic"
    , "ateallowcontent-basecontent-encodingcache-controlconnectiondatetrailertran"
    , "sfer-encodingupgradeviawarningcontent-languagecontent-lengthcontent-locati"
    , "oncontent-md5content-rangecontent-typeetagexpireslast-modifiedset-cookieMo"
    , "ndayTuesdayWednesdayThursdayFridaySaturdaySundayJanFebMarAprMayJunJulAugSe"
    , "pOctNovDecchunkedtext/htmlimage/pngimage/jpgimage/gifapplication/xmlapplic"
    , "ation/xhtmltext/plainpublicmax-agecharset=iso-8859-1utf-8gzipdeflateHTTP/1"
    , ".1statusversionurl\0"
    ]
