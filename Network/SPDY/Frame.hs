{-# LANGUAGE RecordWildCards, ForeignFunctionInterface, OverloadedStrings #-}
module Network.SPDY.Frame where

import Data.Binary.Put ( runPut )

import Data.Text ( Text )
import qualified Data.Text.Encoding as Text

import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import Data.Word

import Control.Monad ( unless )

import Data.ByteString.Char8 () -- IsString instance
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative ( (<$>) )
import Control.Monad ( liftM2, forM_ )

type StreamID = Word32

ourSPDYVersion :: Word16
ourSPDYVersion = 2

data Frame
  = DataFrame {
      dataFrameFlags :: Word8,
      dataFrameStreamID :: Word32,
      dataFramePayload :: B.ByteString }
  | SynStreamControlFrame {
      controlFrameFlags :: Word8,
      synStreamFrameStreamID :: Word32,
      synStreamFrameAssociatedStreamID :: Word32,
      synStreamFramePriority :: Word8,
      synStreamFrameNVHCompressed :: B.ByteString }
  | SynReplyControlFrame {
      controlFrameFlags :: Word8,
      synReplyFrameStreamID :: Word32,
      synReplyFrameNVHCompressed :: B.ByteString }
  | RstStreamControlFrame {
      controlFrameFlags :: Word8,
      rstStreamFrameStreamID :: Word32,
      rstStreamFrameStatusCode :: RstStreamStatusCode }
  | PingControlFrame {
      pingControlFrameId :: Word32 }
  | GoAwayFrame {
      controlFrameFlags :: Word8,
      goAwayLastGoodStreamID :: Word32 }
  | SettingsFrame {
      controlFrameFlags :: Word8,
      settingsFrameValues :: [(Word32, Word8, Word32)] }
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
getFrame = do
  c <- getWord8 1
  case c of
    0 -> getDataFrame
    1 -> getControlFrame
    _ -> error "impossible"

getControlFrameHeader :: BitGet ControlFrameHeader
getControlFrameHeader = do
  controlFrameVersion <- getWord16be 15
  unless (controlFrameVersion == 2) $ do
    fail $ "Unsupported SPDY Frame Version: " ++ show controlFrameVersion
  controlFrameType <- getWord16be 16
  controlFrameFlags_ <- getWord8 8
  controlFrameLength <- getWord32be 24
  return ControlFrameHeader { .. }

getDataFrame :: BitGet Frame
getDataFrame = do
  dataFrameStreamID <- getWord32be 31
  dataFrameFlags <- getWord8 8
  len <- fromIntegral <$> getWord32be 24
  dataFramePayload <- getByteString len
  return DataFrame { .. }

getControlFrame :: BitGet Frame
getControlFrame = do
  header <- getControlFrameHeader
  getControlFrameBody header

getControlFrameBody :: ControlFrameHeader -> BitGet Frame
getControlFrameBody header =
  case controlFrameType header of
    1 -> getSynStream header
    2 -> getSynReplyStream header
    3 -> getRstStream header
    4 -> getSettingsFrame header
    5 -> fail "Received NOOP frame. Not implemented."
    6 -> getPing header
    7 -> getGoAway header
    8 -> fail "Received HEADERS frame. Not implemented."
    n -> fail $ "Received unknown control frame type = " ++ show n

getSynStream :: ControlFrameHeader -> BitGet Frame
getSynStream header = do
  let controlFrameFlags = controlFrameFlags_ header
  _ <- getWord8 1
  synStreamFrameStreamID <- getWord32be 31
  _ <- getWord8 1
  synStreamFrameAssociatedStreamID <- getWord32be 31
  synStreamFramePriority <- getWord8 2
  _ <- getWord16be 14
  let contentLength = fromIntegral (controlFrameLength header)
  unless (contentLength >= 11) $
    fail $ "SYN_STREAM: frame length too short, length = " ++ show contentLength
  synStreamFrameNVHCompressed <- getByteString (contentLength - 10)
  return SynStreamControlFrame { .. }

getSynReplyStream :: ControlFrameHeader -> BitGet Frame
getSynReplyStream header = do
  let controlFrameFlags = controlFrameFlags_ header
  _ <- getWord8 1 -- unused
  synReplyFrameStreamID <- getWord32be 31
  _ <- getWord16be 16 -- unused
  synReplyFrameNVHCompressed <-
    getByteString (fromIntegral (controlFrameLength header) - 6)
  return SynReplyControlFrame { .. }

getRstStream :: ControlFrameHeader -> BitGet Frame
getRstStream header = do
  let controlFrameFlags = controlFrameFlags_ header
      frameLength = controlFrameLength header
  unless (controlFrameFlags == 0) $
    fail $ "RST_STREAM: Expected Control Frame Flags to be 0, it's " ++ show controlFrameFlags
  unless (frameLength == 8) $
    fail $ "RST_STREAM: Expected Control Frame Length to be 8, it's " ++ show frameLength
  _ <- getWord8 1 -- skipped
  rstStreamFrameStreamID <- getWord32be 31
  rstStreamFrameStatusCode <- getWord32be 32
  return RstStreamControlFrame { .. }

getPing :: ControlFrameHeader -> BitGet Frame
getPing header = do
  let frameFlags  = controlFrameFlags_ header
      frameLength = controlFrameLength header
  unless (frameFlags == 0) $
    fail $ "PING: Expected Control Frame Flags to be 0, it's " ++ show frameFlags
  unless (frameLength == 4) $
    fail $ "PING: Expected Control Frame Length to be 4, it's " ++ show frameLength
  pingControlFrameId <- getWord32be 32
  return PingControlFrame { .. }

getGoAway :: ControlFrameHeader -> BitGet Frame
getGoAway header = do
  let controlFrameFlags = controlFrameFlags_ header
      frameLength = controlFrameLength header
  unless (frameLength == 4) $
    fail $ "GOAWAY: Expected Control Frame Length to be 4, it's " ++ show frameLength
  _ <- getWord8 1 -- skipped
  goAwayLastGoodStreamID <- getWord32be 31
  return GoAwayFrame { .. }

getSettingsFrame :: ControlFrameHeader -> BitGet Frame
getSettingsFrame header = do
  let controlFrameFlags = controlFrameFlags_ header
  entries <- getWord32be 32
  settingsFrameValues <- sequence $ replicate (fromIntegral entries) getSettingEntry
  return $ SettingsFrame { .. }
  
getSettingEntry :: BitGet (Word32, Word8, Word32)
getSettingEntry = do
  id_ <- getWord32be 24
  flags <- getWord8 8
  value <- getWord32be 32
  return (id_, flags, value)

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

-- ** Put stuff

putNVHBlock :: NameValueHeaderBlock -> BitPut ()
putNVHBlock nvh = do
  putWord16be 16 (fromIntegral $ length nvh)
  forM_ nvh $ \(k,v) -> do
    putText16 k
    putText16 v
  where
  putText16 s = do
    let text = Text.encodeUtf8 s
    putWord16be 16 (fromIntegral $ B.length text)
    putByteString text

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
            putByteString synStreamFrameNVHCompressed
      putControlFrameHeader (mkControlHeader frame payload)
      mapM_ putByteString (L.toChunks payload)

    SynReplyControlFrame { .. } -> do
      let payload = runPut $ runBitPut $ do
            putBool False -- ignored
            putWord32be 31 synReplyFrameStreamID
            putWord16be 16 0 -- ignored
            putByteString synReplyFrameNVHCompressed
      putControlFrameHeader (mkControlHeader frame payload)
      mapM_ putByteString (L.toChunks payload)

    RstStreamControlFrame { .. } -> do
      putControlFrameHeader (mkControlHeaderWithLength frame 8)
      putBool False -- ignored
      putWord32be 31 rstStreamFrameStreamID
      putWord32be 32 rstStreamFrameStatusCode

    PingControlFrame { .. } -> do
      putControlFrameHeader (mkControlHeaderWithLength frame 4)
      putWord32be 32 pingControlFrameId

    GoAwayFrame { .. } -> do
      putControlFrameHeader (mkControlHeaderWithLength frame 4)
      putBool False -- ignored
      putWord32be 31 goAwayLastGoodStreamID

    SettingsFrame { .. } -> do
      let payload = runPut $ runBitPut $ do
            mapM_ putValue settingsFrameValues
          putValue (id_, flags, value) = do
            putWord32be 24 id_
            putWord8 8 flags
            putWord32be 32 value
      putControlFrameHeader (mkControlHeader frame payload)
      putWord32be 32 (fromIntegral $ length settingsFrameValues)
      mapM_ putByteString (L.toChunks payload)

mkControlHeaderWithLength :: Frame -> Word32 -> ControlFrameHeader
mkControlHeaderWithLength frame payloadLength = ControlFrameHeader
  { controlFrameVersion = ourSPDYVersion
  , controlFrameType = case frame of
                         DataFrame             {} -> error "impossible"
                         SynStreamControlFrame {} -> 1
                         SynReplyControlFrame  {} -> 2
                         RstStreamControlFrame {} -> 3
                         SettingsFrame         {} -> 4
                         PingControlFrame      {} -> 6
                         GoAwayFrame           {} -> 7
  , controlFrameFlags_ = case frame of
                           PingControlFrame {} -> 0
                           _                   -> controlFrameFlags frame
  , controlFrameLength = payloadLength
  }

mkControlHeader :: Frame -> L.ByteString -> ControlFrameHeader
mkControlHeader frame payload =
  mkControlHeaderWithLength
    frame
    (fromIntegral (L.length payload))

putControlFrameHeader :: ControlFrameHeader -> BitPut ()
putControlFrameHeader (ControlFrameHeader { .. }) = do
  putBool True
  putWord16be 15 ourSPDYVersion
  putWord16be 16 controlFrameType
  putWord8 8 controlFrameFlags_
  putWord32be 24 controlFrameLength

nvhDictionary :: B.ByteString
nvhDictionary = B.concat $
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
