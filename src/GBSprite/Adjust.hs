-- | Canvas-wide color adjustments.
--
-- Convenience functions that apply color transforms uniformly
-- across every pixel of a canvas. Built on 'mapPixels' and the
-- color-level transforms from "GBSprite.Color".
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
  )
where

import Data.Word (Word8)
import GBSprite.Canvas
  ( Canvas,
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
