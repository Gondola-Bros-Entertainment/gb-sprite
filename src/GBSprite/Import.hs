-- | BMP and PNG file import.
--
-- Reads 24-bit and 32-bit BMP files and 8-bit RGB\/RGBA PNG files into
-- 'Canvas' values. Decoding functions are pure; only 'readBmp' and
-- 'readPng' perform IO.
module GBSprite.Import
  ( -- * BMP
    readBmp,
    decodeBmp,

    -- * PNG
    readPng,
    decodePng,
  )
where

import qualified Codec.Compression.Zlib as Z
import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..))

-- ---------------------------------------------------------------------------
-- Shared constants
-- ---------------------------------------------------------------------------

-- | Opaque alpha value (fully opaque).
opaqueAlpha :: Word8
opaqueAlpha = 255

-- | Bits to shift for byte 1 in multi-byte integer encoding.
shift8 :: Int
shift8 = 8

-- | Bits to shift for byte 2 in multi-byte integer encoding.
shift16 :: Int
shift16 = 16

-- | Bits to shift for byte 3 in multi-byte integer encoding.
shift24 :: Int
shift24 = 24

-- ---------------------------------------------------------------------------
-- BMP constants
-- ---------------------------------------------------------------------------

-- | Minimum BMP header size (14-byte file header + 40-byte DIB header).
bmpMinHeaderSize :: Int
bmpMinHeaderSize = 54

-- | ASCII @B@ — first byte of BMP signature.
bmpSigB :: Word8
bmpSigB = 0x42

-- | ASCII @M@ — second byte of BMP signature.
bmpSigM :: Word8
bmpSigM = 0x4D

-- | Byte offset of the pixel data pointer in the BMP file header.
bmpPixelDataOffsetPos :: Int
bmpPixelDataOffsetPos = 10

-- | Byte offset of width in the DIB header.
bmpWidthPos :: Int
bmpWidthPos = 18

-- | Byte offset of height in the DIB header.
bmpHeightPos :: Int
bmpHeightPos = 22

-- | Byte offset of bits-per-pixel in the DIB header.
bmpBppPos :: Int
bmpBppPos = 28

-- | Bits per pixel for 24-bit BMP (RGB).
bmpBpp24 :: Int
bmpBpp24 = 24

-- | Bits per pixel for 32-bit BMP (BGRA).
bmpBpp32 :: Int
bmpBpp32 = 32

-- | Bytes per source pixel in a 24-bit BMP (BGR).
bmpBytesPerPixel24 :: Int
bmpBytesPerPixel24 = 3

-- | Bytes per source pixel in a 32-bit BMP (BGRA).
bmpBytesPerPixel32 :: Int
bmpBytesPerPixel32 = 4

-- | BMP row alignment in bytes.
bmpRowAlignment :: Int
bmpRowAlignment = 4

-- | Bit mask for the sign bit in a 32-bit value.
signBit32 :: Int
signBit32 = 0x80000000

-- | Full range of a 32-bit unsigned value (for two's complement conversion).
twoComplementRange32 :: Int
twoComplementRange32 = 0x100000000

-- ---------------------------------------------------------------------------
-- PNG constants
-- ---------------------------------------------------------------------------

-- | Length of the PNG file signature in bytes.
pngSignatureLength :: Int
pngSignatureLength = 8

-- | Expected PNG file signature as a strict 'BS.ByteString'.
pngSignatureBS :: BS.ByteString
pngSignatureBS = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

-- | Size of a PNG chunk header (4-byte length + 4-byte type).
pngChunkHeaderSize :: Int
pngChunkHeaderSize = 8

-- | Size of a PNG chunk CRC footer in bytes.
pngChunkCrcSize :: Int
pngChunkCrcSize = 4

-- | Size of the IHDR chunk data in bytes.
pngIhdrDataSize :: Int
pngIhdrDataSize = 13

-- | IHDR chunk type tag.
pngIhdrTag :: BS.ByteString
pngIhdrTag = BS.pack [73, 72, 68, 82]

-- | IDAT chunk type tag.
pngIdatTag :: BS.ByteString
pngIdatTag = BS.pack [73, 68, 65, 84]

-- | IEND chunk type tag.
pngIendTag :: BS.ByteString
pngIendTag = BS.pack [73, 69, 78, 68]

-- | PNG color type for RGB (truecolor, no alpha).
pngColorTypeRGB :: Word8
pngColorTypeRGB = 2

-- | PNG color type for RGBA (truecolor with alpha).
pngColorTypeRGBA :: Word8
pngColorTypeRGBA = 6

-- | Expected bit depth: 8 bits per channel.
pngBitDepth8 :: Word8
pngBitDepth8 = 8

-- | Bytes per pixel for RGB (3 channels).
pngRGBBytesPerPixel :: Int
pngRGBBytesPerPixel = 3

-- | Bytes per pixel for RGBA (4 channels).
pngRGBABytesPerPixel :: Int
pngRGBABytesPerPixel = 4

-- | PNG filter type: None (raw bytes, no filtering).
pngFilterNone :: Word8
pngFilterNone = 0

-- | PNG filter type: Sub (difference from left neighbour).
pngFilterSub :: Word8
pngFilterSub = 1

-- | PNG filter type: Up (difference from above pixel).
pngFilterUp :: Word8
pngFilterUp = 2

-- | PNG filter type: Average (mean of left and above).
pngFilterAverage :: Word8
pngFilterAverage = 3

-- | PNG filter type: Paeth (Paeth predictor of left, above, upper-left).
pngFilterPaeth :: Word8
pngFilterPaeth = 4

-- | Divisor used in the Average filter computation.
averageDivisor :: Int
averageDivisor = 2

-- | Size of the filter type byte prepended to each PNG row.
pngFilterByteSize :: Int
pngFilterByteSize = 1

-- | Chunk length field size in bytes.
pngChunkLengthSize :: Int
pngChunkLengthSize = 4

-- | Chunk type field size in bytes.
pngChunkTypeSize :: Int
pngChunkTypeSize = 4

-- | Byte offset of the bit depth field within IHDR data.
ihdrBitDepthOffset :: Int
ihdrBitDepthOffset = 8

-- | Byte offset of the color type field within IHDR data.
ihdrColorTypeOffset :: Int
ihdrColorTypeOffset = 9

-- | Byte offset of width within IHDR data.
ihdrWidthOffset :: Int
ihdrWidthOffset = 0

-- | Byte offset of height within IHDR data.
ihdrHeightOffset :: Int
ihdrHeightOffset = 4

-- ---------------------------------------------------------------------------
-- BMP decoding
-- ---------------------------------------------------------------------------

-- | Read a BMP file and decode it into a 'Canvas'.
readBmp :: FilePath -> IO (Either String Canvas)
readBmp path = decodeBmp <$> BS.readFile path

-- | Decode a strict 'BS.ByteString' containing a BMP file into a 'Canvas'.
--
-- Supports 24-bit (RGB) and 32-bit (BGRA) uncompressed BMP files.
-- Rows may be stored bottom-up (positive height) or top-down (negative
-- height); both orientations are handled.
decodeBmp :: BS.ByteString -> Either String Canvas
decodeBmp bs = do
  validateBmpSignature bs
  width <- readLE32 bs bmpWidthPos
  rawHeight <- readLESigned32 bs bmpHeightPos
  bpp <- readLE16 bs bmpBppPos
  pixelOffset <- readLE32 bs bmpPixelDataOffsetPos
  let (!height, !topDown) =
        if rawHeight < 0
          then (negate rawHeight, True)
          else (rawHeight, False)
  validateBmpDimensions width height
  validateBmpBpp bpp
  let bytesPerPixel =
        if bpp == bmpBpp24
          then bmpBytesPerPixel24
          else bmpBytesPerPixel32
      rowDataSize = width * bytesPerPixel
      paddedRowSize = alignRow rowDataSize bmpRowAlignment
      requiredSize = pixelOffset + paddedRowSize * height
  if BS.length bs < requiredSize
    then Left "BMP: file too small for declared pixel data"
    else
      Right
        ( Canvas
            width
            height
            ( decodeBmpPixels
                bs
                pixelOffset
                width
                height
                paddedRowSize
                bytesPerPixel
                topDown
            )
        )

-- | Validate the 2-byte BMP file signature.
validateBmpSignature :: BS.ByteString -> Either String ()
validateBmpSignature bs
  | BS.length bs < bmpMinHeaderSize =
      Left "BMP: file too small for header"
  | BS.index bs 0 /= bmpSigB || BS.index bs 1 /= bmpSigM =
      Left "BMP: invalid signature (expected 'BM')"
  | otherwise = Right ()

-- | Validate that BMP dimensions are positive.
validateBmpDimensions :: Int -> Int -> Either String ()
validateBmpDimensions width height
  | width <= 0 = Left "BMP: width must be positive"
  | height <= 0 = Left "BMP: height must be positive"
  | otherwise = Right ()

-- | Validate that bits-per-pixel is a supported value (24 or 32).
validateBmpBpp :: Int -> Either String ()
validateBmpBpp bpp
  | bpp == bmpBpp24 = Right ()
  | bpp == bmpBpp32 = Right ()
  | otherwise =
      Left
        ( "BMP: unsupported bits-per-pixel: "
            ++ show bpp
            ++ " (expected 24 or 32)"
        )

-- | Align a row size up to the nearest multiple of the given alignment.
alignRow :: Int -> Int -> Int
alignRow size alignment =
  ((size + alignment - 1) `div` alignment) * alignment

-- | Decode BMP pixel data into an RGBA 'BS.ByteString' (row-major, top-down).
decodeBmpPixels ::
  BS.ByteString ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  Bool ->
  BS.ByteString
decodeBmpPixels bs pixelOffset width height paddedRowSize bytesPerPixel topDown =
  BS.concat [decodeRow row | row <- [0 .. height - 1]]
  where
    decodeRow :: Int -> BS.ByteString
    decodeRow outputRow =
      let sourceRow =
            if topDown
              then outputRow
              else height - 1 - outputRow
          rowStart = pixelOffset + sourceRow * paddedRowSize
       in BS.pack (concatMap (decodePixel rowStart) [0 .. width - 1])

    decodePixel :: Int -> Int -> [Word8]
    decodePixel rowStart col =
      let idx = rowStart + col * bytesPerPixel
          blueVal = BS.index bs idx
          greenVal = BS.index bs (idx + 1)
          redVal = BS.index bs (idx + 2)
          alphaVal =
            if bytesPerPixel == bmpBytesPerPixel32
              then BS.index bs (idx + 3)
              else opaqueAlpha
       in [redVal, greenVal, blueVal, alphaVal]

-- ---------------------------------------------------------------------------
-- PNG decoding
-- ---------------------------------------------------------------------------

-- | Read a PNG file and decode it into a 'Canvas'.
readPng :: FilePath -> IO (Either String Canvas)
readPng path = decodePng <$> BL.readFile path

-- | Decode a lazy 'BL.ByteString' containing a PNG file into a 'Canvas'.
--
-- Supports 8-bit RGB (color type 2) and 8-bit RGBA (color type 6) images
-- with all five standard filter types (None, Sub, Up, Average, Paeth).
decodePng :: BL.ByteString -> Either String Canvas
decodePng lbs = do
  validatePngSignature lbs
  (width, height, colorType) <- parseIhdr lbs
  let bytesPerPixel =
        if colorType == pngColorTypeRGBA
          then pngRGBABytesPerPixel
          else pngRGBBytesPerPixel
  idatData <- collectIdatChunks lbs
  decompressed <- decompressIdat idatData
  let rowBytes = width * bytesPerPixel
      expectedSize = height * (pngFilterByteSize + rowBytes)
  if BS.length decompressed /= expectedSize
    then
      Left
        ( "PNG: decompressed data size mismatch (expected "
            ++ show expectedSize
            ++ ", got "
            ++ show (BS.length decompressed)
            ++ ")"
        )
    else
      let reconstructed =
            reconstructFilters decompressed width height bytesPerPixel
          rgbaData = toRgba reconstructed width height colorType
       in Right (Canvas width height rgbaData)

-- | Validate the 8-byte PNG file signature.
validatePngSignature :: BL.ByteString -> Either String ()
validatePngSignature lbs
  | BL.length lbs < fromIntegral pngSignatureLength =
      Left "PNG: file too small for signature"
  | BL.toStrict (BL.take (fromIntegral pngSignatureLength) lbs) /= pngSignatureBS =
      Left "PNG: invalid file signature"
  | otherwise = Right ()

-- | Parse the IHDR chunk and return @(width, height, colorType)@.
parseIhdr :: BL.ByteString -> Either String (Int, Int, Word8)
parseIhdr lbs = do
  let chunkStart = pngSignatureLength
      minSize = chunkStart + pngChunkHeaderSize + pngIhdrDataSize
  if BL.length lbs < fromIntegral minSize
    then Left "PNG: file too small for IHDR chunk"
    else do
      let tagOffset = chunkStart + pngChunkLengthSize
          tagBytes =
            BL.toStrict
              ( BL.take
                  (fromIntegral pngChunkTypeSize)
                  (BL.drop (fromIntegral tagOffset) lbs)
              )
      if tagBytes /= pngIhdrTag
        then Left "PNG: first chunk is not IHDR"
        else do
          let dataStart = chunkStart + pngChunkHeaderSize
          width <- readBE32 lbs (dataStart + ihdrWidthOffset)
          height <- readBE32 lbs (dataStart + ihdrHeightOffset)
          let bitDepth = lazyIndex lbs (dataStart + ihdrBitDepthOffset)
              colorType = lazyIndex lbs (dataStart + ihdrColorTypeOffset)
          validateIhdrBitDepth bitDepth
          validateIhdrColorType colorType
          validatePngDimensions width height
          Right (width, height, colorType)

-- | Validate that the IHDR bit depth is 8.
validateIhdrBitDepth :: Word8 -> Either String ()
validateIhdrBitDepth bitDepth
  | bitDepth == pngBitDepth8 = Right ()
  | otherwise =
      Left
        ( "PNG: unsupported bit depth: "
            ++ show bitDepth
            ++ " (expected 8)"
        )

-- | Validate that the IHDR color type is RGB (2) or RGBA (6).
validateIhdrColorType :: Word8 -> Either String ()
validateIhdrColorType colorType
  | colorType == pngColorTypeRGB = Right ()
  | colorType == pngColorTypeRGBA = Right ()
  | otherwise =
      Left
        ( "PNG: unsupported color type: "
            ++ show colorType
            ++ " (expected 2 or 6)"
        )

-- | Validate that PNG dimensions are positive.
validatePngDimensions :: Int -> Int -> Either String ()
validatePngDimensions width height
  | width <= 0 = Left "PNG: width must be positive"
  | height <= 0 = Left "PNG: height must be positive"
  | otherwise = Right ()

-- | Collect and concatenate all IDAT chunk data from a PNG file.
collectIdatChunks :: BL.ByteString -> Either String BL.ByteString
collectIdatChunks lbs = go (fromIntegral pngSignatureLength) []
  where
    totalLen :: Int64
    totalLen = BL.length lbs

    go :: Int64 -> [BL.ByteString] -> Either String BL.ByteString
    go !offset !acc
      | offset + fromIntegral pngChunkHeaderSize > totalLen = finalize acc
      | otherwise =
          let chunkLen = readBE32Direct lbs offset
              tagStart = offset + fromIntegral pngChunkLengthSize
              tagBytes =
                BL.toStrict
                  ( BL.take
                      (fromIntegral pngChunkTypeSize)
                      (BL.drop tagStart lbs)
                  )
              dataStart = offset + fromIntegral pngChunkHeaderSize
              nextChunk =
                dataStart
                  + fromIntegral chunkLen
                  + fromIntegral pngChunkCrcSize
           in if tagBytes == pngIendTag
                then finalize acc
                else
                  if tagBytes == pngIdatTag
                    then
                      let chunk =
                            BL.take (fromIntegral chunkLen) (BL.drop dataStart lbs)
                       in go nextChunk (acc ++ [chunk])
                    else go nextChunk acc

    finalize :: [BL.ByteString] -> Either String BL.ByteString
    finalize [] = Left "PNG: no IDAT chunks found"
    finalize chunks = Right (BL.concat chunks)

-- | Decompress concatenated IDAT data using zlib.
--
-- Note: 'Z.decompress' may throw a 'Z.DecompressError' on corrupt data.
-- In the IO wrappers ('readPng') this surfaces as an IO exception.
decompressIdat :: BL.ByteString -> Either String BS.ByteString
decompressIdat compressed =
  Right (BL.toStrict (Z.decompress compressed))

-- | Reconstruct filtered PNG row data into raw pixel bytes.
reconstructFilters :: BS.ByteString -> Int -> Int -> Int -> BS.ByteString
reconstructFilters decompressed width height bytesPerPixel =
  snd (foldlRange reconstructRow (BS.empty, BS.empty) 0 height)
  where
    rowBytes :: Int
    rowBytes = width * bytesPerPixel

    rowStride :: Int
    rowStride = pngFilterByteSize + rowBytes

    reconstructRow ::
      (BS.ByteString, BS.ByteString) ->
      Int ->
      (BS.ByteString, BS.ByteString)
    reconstructRow (!prevRow, !accPixels) rowIdx =
      let rawRowStart = rowIdx * rowStride
          filterByte = BS.index decompressed rawRowStart
          filteredData =
            BS.take rowBytes (BS.drop (rawRowStart + pngFilterByteSize) decompressed)
          reconstructedRow =
            applyFilter filterByte filteredData prevRow bytesPerPixel
       in (reconstructedRow, accPixels <> reconstructedRow)

-- | Strict left fold over a range @[lo .. hi-1]@.
foldlRange :: (a -> Int -> a) -> a -> Int -> Int -> a
foldlRange f !acc0 !lo !hi = go acc0 lo
  where
    go !acc !idx
      | idx >= hi = acc
      | otherwise = go (f acc idx) (idx + 1)

-- | Apply a PNG filter to reconstruct one row of pixel data.
applyFilter ::
  Word8 -> BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
applyFilter filterType filtered prevRow bytesPerPixel =
  BS.pack (buildRow 0 [])
  where
    len :: Int
    len = BS.length filtered

    buildRow :: Int -> [Word8] -> [Word8]
    buildRow !idx !acc
      | idx >= len = reverse acc
      | otherwise =
          let rawByte = BS.index filtered idx
              reconByte =
                reconstructByte
                  filterType
                  rawByte
                  idx
                  prevRow
                  bytesPerPixel
                  acc
           in buildRow (idx + 1) (reconByte : acc)

-- | Reconstruct a single byte based on the PNG filter type.
reconstructByte ::
  Word8 ->
  Word8 ->
  Int ->
  BS.ByteString ->
  Int ->
  [Word8] ->
  Word8
reconstructByte filterType rawByte idx prevRow bytesPerPixel currentReversed
  | filterType == pngFilterNone = rawByte
  | filterType == pngFilterSub = rawByte + leftByte
  | filterType == pngFilterUp = rawByte + upByte
  | filterType == pngFilterAverage =
      let avg =
            (fromIntegral leftByte + fromIntegral upByte)
              `div` (averageDivisor :: Int)
       in rawByte + fromIntegral avg
  | filterType == pngFilterPaeth =
      rawByte
        + fromIntegral
          ( paethPredictor
              (fromIntegral leftByte)
              (fromIntegral upByte)
              (fromIntegral upLeftByte)
          )
  | otherwise = rawByte
  where
    hasPrev :: Bool
    hasPrev = BS.length prevRow > 0

    leftByte :: Word8
    leftByte
      | idx >= bytesPerPixel =
          case drop (bytesPerPixel - 1) currentReversed of
            (b : _) -> b
            [] -> 0
      | otherwise = 0

    upByte :: Word8
    upByte
      | hasPrev = BS.index prevRow idx
      | otherwise = 0

    upLeftByte :: Word8
    upLeftByte
      | idx >= bytesPerPixel && hasPrev =
          BS.index prevRow (idx - bytesPerPixel)
      | otherwise = 0

-- | Paeth predictor function used by PNG filter type 4.
paethPredictor :: Int -> Int -> Int -> Int
paethPredictor left up upperLeft =
  let estimate = left + up - upperLeft
      distLeft = abs (estimate - left)
      distUp = abs (estimate - up)
      distUpperLeft = abs (estimate - upperLeft)
   in if distLeft <= distUp && distLeft <= distUpperLeft
        then left
        else
          if distUp <= distUpperLeft
            then up
            else upperLeft

-- | Convert reconstructed pixel data to RGBA format.
--
-- For RGBA input this is a no-op. For RGB input, inserts alpha 255 after
-- each RGB triplet.
toRgba :: BS.ByteString -> Int -> Int -> Word8 -> BS.ByteString
toRgba pixels width height colorType
  | colorType == pngColorTypeRGBA = pixels
  | otherwise =
      let totalPixels = width * height
       in BS.pack (concatMap rgbToRgba [0 .. totalPixels - 1])
  where
    rgbToRgba :: Int -> [Word8]
    rgbToRgba pixelIdx =
      let offset = pixelIdx * pngRGBBytesPerPixel
          redVal = BS.index pixels offset
          greenVal = BS.index pixels (offset + 1)
          blueVal = BS.index pixels (offset + 2)
       in [redVal, greenVal, blueVal, opaqueAlpha]

-- ---------------------------------------------------------------------------
-- Little-endian helpers (BMP)
-- ---------------------------------------------------------------------------

-- | Read a little-endian unsigned 16-bit integer at a byte offset.
readLE16 :: BS.ByteString -> Int -> Either String Int
readLE16 bs offset
  | offset + 1 >= BS.length bs =
      Left ("BMP: unexpected end of file at offset " ++ show offset)
  | otherwise =
      let lo = fromIntegral (BS.index bs offset) :: Int
          hi = fromIntegral (BS.index bs (offset + 1)) :: Int
       in Right (lo .|. (hi `shiftL` shift8))

-- | Read a little-endian unsigned 32-bit integer at a byte offset.
readLE32 :: BS.ByteString -> Int -> Either String Int
readLE32 bs offset
  | offset + 3 >= BS.length bs =
      Left ("BMP: unexpected end of file at offset " ++ show offset)
  | otherwise =
      let b0 = fromIntegral (BS.index bs offset) :: Int
          b1 = fromIntegral (BS.index bs (offset + 1)) :: Int
          b2 = fromIntegral (BS.index bs (offset + 2)) :: Int
          b3 = fromIntegral (BS.index bs (offset + 3)) :: Int
       in Right
            ( b0
                .|. (b1 `shiftL` shift8)
                .|. (b2 `shiftL` shift16)
                .|. (b3 `shiftL` shift24)
            )

-- | Read a little-endian signed 32-bit integer at a byte offset.
readLESigned32 :: BS.ByteString -> Int -> Either String Int
readLESigned32 bs offset = do
  unsigned <- readLE32 bs offset
  let signed =
        if unsigned .&. signBit32 /= 0
          then unsigned - twoComplementRange32
          else unsigned
  Right signed

-- ---------------------------------------------------------------------------
-- Big-endian helpers (PNG)
-- ---------------------------------------------------------------------------

-- | Read a big-endian unsigned 32-bit integer at a byte offset from
-- a lazy 'BL.ByteString', with bounds checking.
readBE32 :: BL.ByteString -> Int -> Either String Int
readBE32 lbs offset
  | fromIntegral offset + 3 >= BL.length lbs =
      Left ("PNG: unexpected end of file at offset " ++ show offset)
  | otherwise = Right (readBE32Direct lbs (fromIntegral offset))

-- | Read a big-endian 32-bit unsigned integer at the given 'Int64' offset.
-- No bounds checking is performed.
readBE32Direct :: BL.ByteString -> Int64 -> Int
readBE32Direct lbs offset =
  let b0 = fromIntegral (BL.index lbs offset) :: Int
      b1 = fromIntegral (BL.index lbs (offset + 1)) :: Int
      b2 = fromIntegral (BL.index lbs (offset + 2)) :: Int
      b3 = fromIntegral (BL.index lbs (offset + 3)) :: Int
   in (b0 `shiftL` shift24)
        .|. (b1 `shiftL` shift16)
        .|. (b2 `shiftL` shift8)
        .|. b3

-- | Index into a lazy 'BL.ByteString' using an 'Int' offset.
lazyIndex :: BL.ByteString -> Int -> Word8
lazyIndex lbs idx = BL.index lbs (fromIntegral idx)
