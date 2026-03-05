-- | PNG file export.
--
-- Writes 32-bit RGBA PNG files using zlib compression.
-- Same philosophy as "GBSprite.BMP" — raw chunk bytes + compressed
-- pixel data, single lightweight dependency (zlib).
module GBSprite.PNG
  ( -- * Export
    writePng,
    encodePng,
  )
where

import qualified Codec.Compression.Zlib as Z
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word8)
import GBSprite.Canvas (Canvas (..))

-- | Write a canvas to a PNG file.
writePng :: FilePath -> Canvas -> IO ()
writePng path canvas = BL.writeFile path (encodePng canvas)

-- | Encode a canvas as a PNG in memory.
encodePng :: Canvas -> BL.ByteString
encodePng (Canvas w h pixels) =
  B.toLazyByteString
    ( pngSignature
        <> encodeIHDR w h
        <> encodeIDAT w h pixels
        <> encodeIEND
    )

-- ---------------------------------------------------------------------------
-- Chunks
-- ---------------------------------------------------------------------------

-- | IHDR chunk: image dimensions, bit depth, color type.
encodeIHDR :: Int -> Int -> B.Builder
encodeIHDR w h =
  let body =
        B.word32BE (fromIntegral w)
          <> B.word32BE (fromIntegral h)
          <> B.word8 pngBitDepth
          <> B.word8 pngColorTypeRGBA
          <> B.word8 0 -- compression: deflate
          <> B.word8 0 -- filter: adaptive
          <> B.word8 0 -- interlace: none
      bodyBS = BL.toStrict (B.toLazyByteString body)
   in encodeChunk ihdrTag bodyBS

-- | IDAT chunk: zlib-compressed filtered pixel data.
encodeIDAT :: Int -> Int -> VS.Vector Word8 -> B.Builder
encodeIDAT w h pixels =
  let rowSize = w * pngBytesPerPixel
      rawRows =
        mconcat
          [ B.word8 0 -- filter type: None
              <> mconcat
                [B.word8 (pixels VS.! (y * rowSize + x)) | x <- [0 .. rowSize - 1]]
          | y <- [0 .. h - 1]
          ]
      compressed = BL.toStrict (Z.compress (B.toLazyByteString rawRows))
   in encodeChunk idatTag compressed

-- | IEND chunk: empty terminator.
encodeIEND :: B.Builder
encodeIEND = encodeChunk iendTag BS.empty

-- | Encode a single PNG chunk: length ++ type ++ data ++ CRC32.
encodeChunk :: BS.ByteString -> BS.ByteString -> B.Builder
encodeChunk tag body =
  let crc = crc32 (tag <> body)
   in B.word32BE (fromIntegral (BS.length body))
        <> B.byteString tag
        <> B.byteString body
        <> B.word32BE crc

-- ---------------------------------------------------------------------------
-- CRC32 (ISO 3309 / ITU-T V.42)
-- ---------------------------------------------------------------------------

-- | CRC32 checksum over a strict 'BS.ByteString'.
crc32 :: BS.ByteString -> Word32
crc32 = xor crcMask . BS.foldl' step crcMask
  where
    step acc byte =
      let idx = fromIntegral ((acc `xor` fromIntegral byte) .&. 0xFF)
       in (acc `shiftR` 8) `xor` (crcTable VS.! idx)

-- | Precomputed CRC32 lookup table (256 entries).
crcTable :: VS.Vector Word32
crcTable = VS.generate crcTableSize gen
  where
    gen n = foldl' (\c _ -> crcStep c) (fromIntegral n) [(1 :: Int) .. crcBitsPerByte]

-- | One step of the CRC32 polynomial division.
crcStep :: Word32 -> Word32
crcStep c
  | c .&. 1 == 1 = crcPolynomial `xor` (c `shiftR` 1)
  | otherwise = c `shiftR` 1

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | PNG 8-byte file signature.
pngSignature :: B.Builder
pngSignature = B.byteString (BS.pack [137, 80, 78, 71, 13, 10, 26, 10])

-- | IHDR chunk type tag.
ihdrTag :: BS.ByteString
ihdrTag = BS.pack [73, 72, 68, 82] -- "IHDR"

-- | IDAT chunk type tag.
idatTag :: BS.ByteString
idatTag = BS.pack [73, 68, 65, 84] -- "IDAT"

-- | IEND chunk type tag.
iendTag :: BS.ByteString
iendTag = BS.pack [73, 69, 78, 68] -- "IEND"

-- | Bit depth: 8 bits per channel.
pngBitDepth :: Word8
pngBitDepth = 8

-- | Color type: RGBA (truecolor with alpha).
pngColorTypeRGBA :: Word8
pngColorTypeRGBA = 6

-- | Bytes per pixel (RGBA).
pngBytesPerPixel :: Int
pngBytesPerPixel = 4

-- | CRC32 polynomial (reversed representation).
crcPolynomial :: Word32
crcPolynomial = 0xEDB88320

-- | CRC32 initial\/final XOR mask.
crcMask :: Word32
crcMask = 0xFFFFFFFF

-- | CRC32 table size.
crcTableSize :: Int
crcTableSize = 256

-- | Bits processed per byte in CRC32 table generation.
crcBitsPerByte :: Int
crcBitsPerByte = 8
