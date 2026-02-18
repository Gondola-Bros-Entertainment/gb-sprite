-- | BMP file export.
--
-- Writes 32-bit BGRA BMP files (bottom-up row order, no compression).
-- Same philosophy as gb-synth's WAV.hs — raw header bytes + pixel data,
-- zero external dependencies beyond bytestring.
module GBSprite.BMP
  ( -- * Export
    writeBmp,
    encodeBmp,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32)
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word8)
import GBSprite.Canvas (Canvas (..))

-- | Write a canvas to a BMP file.
writeBmp :: FilePath -> Canvas -> IO ()
writeBmp path canvas = BL.writeFile path (encodeBmp canvas)

-- | Encode a canvas as a BMP in memory.
encodeBmp :: Canvas -> BL.ByteString
encodeBmp (Canvas w h pixels) =
  let rowSize = w * bmpBytesPerPixel
      -- BMP rows must be 4-byte aligned (already are since 4 bpp)
      paddedRowSize = ((rowSize + 3) `div` 4) * 4
      imageSize = paddedRowSize * h
      fileSize = bmpHeaderSize + fromIntegral imageSize :: Word32
      header =
        -- BMP file header (14 bytes)
        B.byteString (BS.pack [0x42, 0x4D]) -- "BM"
          <> B.word32LE fileSize
          <> B.word16LE 0 -- reserved1
          <> B.word16LE 0 -- reserved2
          <> B.word32LE bmpHeaderSize -- pixel data offset
          -- DIB header (BITMAPINFOHEADER, 40 bytes)
          <> B.word32LE dibHeaderSize
          <> B.int32LE (fromIntegral w) -- width
          <> B.int32LE (fromIntegral h) -- height (positive = bottom-up)
          <> B.word16LE 1 -- color planes
          <> B.word16LE 32 -- bits per pixel
          <> B.word32LE 0 -- compression (BI_RGB)
          <> B.word32LE (fromIntegral imageSize)
          <> B.int32LE bmpPelsPerMeter -- X pixels per meter
          <> B.int32LE bmpPelsPerMeter -- Y pixels per meter
          <> B.word32LE 0 -- colors in table
          <> B.word32LE 0 -- important colors
      pixelData = mconcat [encodeRow w paddedRowSize pixels (h - 1 - y) | y <- [0 .. h - 1]]
   in B.toLazyByteString (header <> pixelData)

-- | Encode one row of pixels (RGBA -> BGRA, with padding).
encodeRow :: Int -> Int -> VS.Vector Word8 -> Int -> B.Builder
encodeRow w paddedRowSize pixels y =
  let rowStart = y * w * bmpBytesPerPixel
      rowBytes =
        mconcat
          [ let idx = rowStart + x * bmpBytesPerPixel
                r = pixels VS.! idx
                g = pixels VS.! (idx + 1)
                b = pixels VS.! (idx + 2)
                a = pixels VS.! (idx + 3)
             in B.word8 b <> B.word8 g <> B.word8 r <> B.word8 a
          | x <- [0 .. w - 1]
          ]
      padBytes = paddedRowSize - w * bmpBytesPerPixel
      padding = mconcat (replicate padBytes (B.word8 0))
   in rowBytes <> padding

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Bytes per pixel (BGRA).
bmpBytesPerPixel :: Int
bmpBytesPerPixel = 4

-- | Total header size (BMP file header + DIB header).
bmpHeaderSize :: Word32
bmpHeaderSize = 54

-- | DIB header size (BITMAPINFOHEADER).
dibHeaderSize :: Word32
dibHeaderSize = 40

-- | Default pixels-per-meter (96 DPI).
bmpPelsPerMeter :: Int32
bmpPelsPerMeter = 3780
