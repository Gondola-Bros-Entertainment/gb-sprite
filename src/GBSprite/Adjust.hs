-- | Canvas-wide color adjustments and LUT color grading.
--
-- Convenience functions that apply color transforms uniformly
-- across every pixel of a canvas. Built on 'mapPixels' and the
-- color-level transforms from "GBSprite.Color".
--
-- The t'ColorLUT' type provides precomputed 3D lookup tables for
-- fast color grading with trilinear interpolation.
module GBSprite.Adjust
  ( -- * Grayscale and inversion
    grayscale,
    invertColors,

    -- * Tinting and opacity
    applyTint,

    -- * Brightness, contrast, saturation
    adjustBrightness,
    adjustContrast,
    adjustSaturation,

    -- * Hue
    shiftHue,

    -- * Color remapping
    remapColor,
    remapColors,

    -- * Stylization
    posterize,
    threshold,
    sepia,

    -- * Color LUT grading
    ColorLUT (..),
    identityLUT,
    applyLUT,
    modifyLUT,
    warmLUT,
    coolLUT,
    cinematicLUT,
  )
where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import GBSprite.Canvas
  ( Canvas,
    generatePixelData,
    mapPixels,
  )
import GBSprite.Color
  ( Color (..),
    brightenColor,
    contrastColor,
    grayscaleColor,
    invertColor,
    saturateColor,
    shiftHueColor,
    tintColor,
  )

-- ---------------------------------------------------------------------------
-- Grayscale and inversion
-- ---------------------------------------------------------------------------

-- | Convert the entire canvas to grayscale using BT.709 luminance.
grayscale :: Canvas -> Canvas
grayscale = mapPixels grayscaleColor

-- | Invert all pixel colors (negate RGB, preserve alpha).
invertColors :: Canvas -> Canvas
invertColors = mapPixels invertColor

-- ---------------------------------------------------------------------------
-- Tinting and opacity
-- ---------------------------------------------------------------------------

-- | Multiply every pixel's RGB by the tint color.
-- The tint's alpha is ignored; each pixel's alpha is preserved.
applyTint :: Color -> Canvas -> Canvas
applyTint tint = mapPixels (tintColor tint)

-- ---------------------------------------------------------------------------
-- Brightness, contrast, saturation
-- ---------------------------------------------------------------------------

-- | Adjust brightness. Amount in @[-1, 1]@.
adjustBrightness :: Double -> Canvas -> Canvas
adjustBrightness amount = mapPixels (brightenColor amount)

-- | Adjust contrast. Amount in @[-1, 1]@.
adjustContrast :: Double -> Canvas -> Canvas
adjustContrast amount = mapPixels (contrastColor amount)

-- | Adjust saturation. Amount in @[-1, 1]@: -1 desaturates, +1 boosts.
adjustSaturation :: Double -> Canvas -> Canvas
adjustSaturation amount = mapPixels (saturateColor amount)

-- ---------------------------------------------------------------------------
-- Hue
-- ---------------------------------------------------------------------------

-- | Shift the hue of every pixel by the given degrees.
shiftHue :: Double -> Canvas -> Canvas
shiftHue degrees = mapPixels (shiftHueColor degrees)

-- ---------------------------------------------------------------------------
-- Color remapping
-- ---------------------------------------------------------------------------

-- | Replace all pixels matching @src@ with @dst@.
remapColor :: Color -> Color -> Canvas -> Canvas
remapColor src dst = mapPixels (\c -> if c == src then dst else c)

-- | Replace colors according to a mapping list.
-- Each @(src, dst)@ pair replaces @src@ with @dst@. First match wins.
remapColors :: [(Color, Color)] -> Canvas -> Canvas
remapColors mapping = mapPixels (applyMapping mapping)
  where
    applyMapping [] c = c
    applyMapping ((src, dst) : rest) c
      | c == src = dst
      | otherwise = applyMapping rest c

-- ---------------------------------------------------------------------------
-- Stylization
-- ---------------------------------------------------------------------------

-- | Posterize: reduce each channel to @n@ discrete levels (2 minimum).
posterize :: Int -> Canvas -> Canvas
posterize levels = mapPixels (posterizeColor clamped)
  where
    clamped = max posterizeMinLevels levels

-- | Threshold: pixels brighter than the cutoff become white,
-- others become black. Alpha is preserved.
threshold :: Word8 -> Canvas -> Canvas
threshold cutoff = mapPixels (thresholdColor cutoff)

-- | Apply a sepia tone. Equivalent to grayscale + warm tint.
sepia :: Canvas -> Canvas
sepia = mapPixels sepiaColor

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

posterizeMinLevels :: Int
posterizeMinLevels = 2

posterizeColor :: Int -> Color -> Color
posterizeColor levels (Color r g b a) =
  Color (quantCh levels r) (quantCh levels g) (quantCh levels b) a
  where
    quantCh n ch =
      let step = channelMax `div` max 1 (n - 1)
          idx = fromIntegral ch `div` max 1 (channelMax `div` n)
          clamped = min (n - 1) idx
       in clampByte (clamped * step)

thresholdColor :: Word8 -> Color -> Color
thresholdColor cutoff (Color r g b a) =
  let lum = luminanceByte r g b
   in if lum >= cutoff
        then Color maxByte maxByte maxByte a
        else Color 0 0 0 a

sepiaColor :: Color -> Color
sepiaColor (Color r g b a) =
  let rf = fromIntegral r :: Double
      gf = fromIntegral g :: Double
      bf = fromIntegral b :: Double
      sr = clampByte (round (rf * sepiaRR + gf * sepiaRG + bf * sepiaRB))
      sg = clampByte (round (rf * sepiaGR + gf * sepiaGG + bf * sepiaGB))
      sb = clampByte (round (rf * sepiaBR + gf * sepiaBG + bf * sepiaBB))
   in Color sr sg sb a

luminanceByte :: Word8 -> Word8 -> Word8 -> Word8
luminanceByte r g b =
  clampByte
    ( round
        ( fromIntegral r * lumR
            + fromIntegral g * lumG
            + fromIntegral b * lumB
        )
    )

clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min channelMax n))

channelMax :: Int
channelMax = 255

maxByte :: Word8
maxByte = 255

lumR :: Double
lumR = 0.2126

lumG :: Double
lumG = 0.7152

lumB :: Double
lumB = 0.0722

-- Sepia tone matrix coefficients
sepiaRR, sepiaRG, sepiaRB :: Double
sepiaRR = 0.393
sepiaRG = 0.769
sepiaRB = 0.189

sepiaGR, sepiaGG, sepiaGB :: Double
sepiaGR = 0.349
sepiaGG = 0.686
sepiaGB = 0.168

sepiaBR, sepiaBG, sepiaBB :: Double
sepiaBR = 0.272
sepiaBG = 0.534
sepiaBB = 0.131

-- ---------------------------------------------------------------------------
-- Color LUT grading
-- ---------------------------------------------------------------------------

-- | A 3D color lookup table for color grading.
--
-- Maps input RGB to output RGB using a cube of @lutSize^3@ entries.
-- Input channels are quantized to @lutSize@ levels; values between
-- entries are trilinearly interpolated for smooth results.
--
-- Build LUTs with 'identityLUT', then transform with 'modifyLUT',
-- or use presets like 'warmLUT', 'coolLUT', 'cinematicLUT'.
data ColorLUT = ColorLUT
  { -- | Number of entries per channel axis
    lutSize :: !Int,
    -- | Packed RGB data: @lutSize^3 * 3@ bytes
    lutData :: !BS.ByteString
  }
  deriving (Show, Eq)

-- | Create an identity LUT (output equals input).
--
-- @identityLUT size@ creates a LUT with @size@ levels per channel.
-- Size is clamped to a minimum of 2. A size of 16 is typical.
identityLUT :: Int -> ColorLUT
identityLUT size =
  let safeSize = max lutMinSize size
      totalBytes = safeSize * safeSize * safeSize * lutBytesPerEntry
      table = generatePixelData totalBytes $ \i ->
        let entryIdx = i `div` lutBytesPerEntry
            channel = i `mod` lutBytesPerEntry
            ri = entryIdx `mod` safeSize
            gi = (entryIdx `div` safeSize) `mod` safeSize
            bi = entryIdx `div` (safeSize * safeSize)
         in indexToChannel safeSize channel ri gi bi
   in ColorLUT safeSize table

-- | Apply a LUT to every pixel of a canvas.
--
-- Each pixel's RGB is looked up in the LUT with trilinear
-- interpolation. Alpha is preserved unchanged.
applyLUT :: ColorLUT -> Canvas -> Canvas
applyLUT lut = mapPixels (lookupLUT lut)

-- | Create a new LUT by transforming every entry of an existing LUT.
--
-- @modifyLUT lut f@ applies @f@ to every color in the LUT table.
-- This enables composing multiple adjustments into a single LUT
-- for efficient per-pixel grading:
--
-- @
-- customLUT = modifyLUT (warmLUT 16) (contrastColor 0.3)
-- @
modifyLUT :: ColorLUT -> (Color -> Color) -> ColorLUT
modifyLUT (ColorLUT size table) f =
  let totalBytes = size * size * size * lutBytesPerEntry
      newTable = generatePixelData totalBytes $ \i ->
        let entryIdx = i `div` lutBytesPerEntry
            channel = i `mod` lutBytesPerEntry
            baseIdx = entryIdx * lutBytesPerEntry
            origR = BS.index table baseIdx
            origG = BS.index table (baseIdx + lutGreenOffset)
            origB = BS.index table (baseIdx + lutBlueOffset)
            Color adjR adjG adjB _ = f (Color origR origG origB maxByte)
         in lutChannelAt channel adjR adjG adjB
   in ColorLUT size newTable

-- | Warm color grading: shifts toward orange\/amber tones.
-- Good for sunset, firelight, and cozy indoor scenes.
warmLUT :: Int -> ColorLUT
warmLUT size = modifyLUT (identityLUT size) warmAdjust
  where
    warmAdjust (Color r g b a) =
      Color
        (clampByte (fromIntegral r + warmRedShift))
        (clampByte (fromIntegral g + warmGreenShift))
        (clampByte (fromIntegral b - warmBlueSuppress))
        a

-- | Cool color grading: shifts toward blue tones.
-- Good for moonlight, underwater, and winter scenes.
coolLUT :: Int -> ColorLUT
coolLUT size = modifyLUT (identityLUT size) coolAdjust
  where
    coolAdjust (Color r g b a) =
      Color
        (clampByte (fromIntegral r - coolRedSuppress))
        (clampByte (fromIntegral g + coolGreenShift))
        (clampByte (fromIntegral b + coolBlueShift))
        a

-- | Cinematic teal-and-orange color grading.
-- Shadows shift toward teal, highlights toward warm amber.
-- The classic blockbuster color grade.
cinematicLUT :: Int -> ColorLUT
cinematicLUT size = modifyLUT (identityLUT size) cinematicAdjust
  where
    cinematicAdjust (Color r g b a) =
      let lumNorm = fromIntegral (luminanceByte r g b) / channelMaxF
          rShift = cineShadowR + lumNorm * (cineHighlightR - cineShadowR)
          gShift = cineShadowG + lumNorm * (cineHighlightG - cineShadowG)
          bShift = cineShadowB + lumNorm * (cineHighlightB - cineShadowB)
       in Color
            (clampByte (round (fromIntegral r + rShift)))
            (clampByte (round (fromIntegral g + gShift)))
            (clampByte (round (fromIntegral b + bShift)))
            a

-- ---------------------------------------------------------------------------
-- LUT internal helpers
-- ---------------------------------------------------------------------------

lookupLUT :: ColorLUT -> Color -> Color
lookupLUT (ColorLUT size table) (Color r g b a) =
  let sizeF = fromIntegral (size - 1) :: Double
      -- Map [0, 255] to [0, size-1]
      rPos = fromIntegral r * sizeF / channelMaxF
      gPos = fromIntegral g * sizeF / channelMaxF
      bPos = fromIntegral b * sizeF / channelMaxF
      -- Floor indices (clamped so ceil stays in bounds)
      rFloor = min (size - lutCeilOffset) (floor rPos)
      gFloor = min (size - lutCeilOffset) (floor gPos)
      bFloor = min (size - lutCeilOffset) (floor bPos)
      -- Fractional parts
      rFrac = rPos - fromIntegral rFloor
      gFrac = gPos - fromIntegral gFloor
      bFrac = bPos - fromIntegral bFloor
      -- Trilinear interpolation per channel
      outR = trilinear rFrac gFrac bFrac table size rFloor gFloor bFloor lutRedOffset
      outG = trilinear rFrac gFrac bFrac table size rFloor gFloor bFloor lutGreenOffset
      outB = trilinear rFrac gFrac bFrac table size rFloor gFloor bFloor lutBlueOffset
   in Color (clampByte (round outR)) (clampByte (round outG)) (clampByte (round outB)) a

trilinear :: Double -> Double -> Double -> BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Double
trilinear rx gx bx table size ri gi bi offset =
  let v000 = fromIntegral (lutByte table size ri gi bi offset) :: Double
      v100 = fromIntegral (lutByte table size (ri + 1) gi bi offset) :: Double
      v010 = fromIntegral (lutByte table size ri (gi + 1) bi offset) :: Double
      v110 = fromIntegral (lutByte table size (ri + 1) (gi + 1) bi offset) :: Double
      v001 = fromIntegral (lutByte table size ri gi (bi + 1) offset) :: Double
      v101 = fromIntegral (lutByte table size (ri + 1) gi (bi + 1) offset) :: Double
      v011 = fromIntegral (lutByte table size ri (gi + 1) (bi + 1) offset) :: Double
      v111 = fromIntegral (lutByte table size (ri + 1) (gi + 1) (bi + 1) offset) :: Double
      c00 = v000 + rx * (v100 - v000)
      c10 = v010 + rx * (v110 - v010)
      c01 = v001 + rx * (v101 - v001)
      c11 = v011 + rx * (v111 - v011)
      c0 = c00 + gx * (c10 - c00)
      c1 = c01 + gx * (c11 - c01)
   in c0 + bx * (c1 - c0)

lutByte :: BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Word8
lutByte table size ri gi bi offset =
  let idx = (bi * size * size + gi * size + ri) * lutBytesPerEntry + offset
   in BS.index table idx

indexToChannel :: Int -> Int -> Int -> Int -> Int -> Word8
indexToChannel size channel ri gi bi =
  let toVal idx = clampByte (round (fromIntegral idx * channelMaxF / fromIntegral (size - 1)))
   in case channel of
        0 -> toVal ri
        1 -> toVal gi
        _ -> toVal bi

lutChannelAt :: Int -> Word8 -> Word8 -> Word8 -> Word8
lutChannelAt 0 r _ _ = r
lutChannelAt 1 _ g _ = g
lutChannelAt _ _ _ b = b

-- ---------------------------------------------------------------------------
-- LUT constants
-- ---------------------------------------------------------------------------

-- | Minimum LUT size (2 levels per channel).
lutMinSize :: Int
lutMinSize = 2

-- | Bytes per LUT entry (RGB, no alpha).
lutBytesPerEntry :: Int
lutBytesPerEntry = 3

-- | Red channel offset within a LUT entry.
lutRedOffset :: Int
lutRedOffset = 0

-- | Green channel offset within a LUT entry.
lutGreenOffset :: Int
lutGreenOffset = 1

-- | Blue channel offset within a LUT entry.
lutBlueOffset :: Int
lutBlueOffset = 2

-- | Ceil offset for clamping floor index so ceil stays in bounds.
lutCeilOffset :: Int
lutCeilOffset = 2

-- | Maximum channel value as Double.
channelMaxF :: Double
channelMaxF = 255.0

-- | Warm LUT: red channel boost.
warmRedShift :: Int
warmRedShift = 10

-- | Warm LUT: green channel boost.
warmGreenShift :: Int
warmGreenShift = 5

-- | Warm LUT: blue channel reduction.
warmBlueSuppress :: Int
warmBlueSuppress = 15

-- | Cool LUT: red channel reduction.
coolRedSuppress :: Int
coolRedSuppress = 10

-- | Cool LUT: green channel boost.
coolGreenShift :: Int
coolGreenShift = 3

-- | Cool LUT: blue channel boost.
coolBlueShift :: Int
coolBlueShift = 15

-- | Cinematic LUT: shadow teal red shift.
cineShadowR :: Double
cineShadowR = -10.0

-- | Cinematic LUT: shadow teal green shift.
cineShadowG :: Double
cineShadowG = 5.0

-- | Cinematic LUT: shadow teal blue shift.
cineShadowB :: Double
cineShadowB = 15.0

-- | Cinematic LUT: highlight warm red shift.
cineHighlightR :: Double
cineHighlightR = 10.0

-- | Cinematic LUT: highlight warm green shift.
cineHighlightG :: Double
cineHighlightG = 5.0

-- | Cinematic LUT: highlight warm blue shift.
cineHighlightB :: Double
cineHighlightB = -10.0
