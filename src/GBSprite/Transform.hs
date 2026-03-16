-- | Canvas transformations: flip, rotate, scale.
--
-- All transforms produce new canvases — the originals are unmodified.
-- Uses single-pass vector generation for O(n) performance.
module GBSprite.Transform
  ( -- * Flip
    flipH,
    flipV,

    -- * Rotate
    rotate90,
    rotate180,
    rotate270,
    rotateArbitrary,

    -- * Scale
    scaleNearest,
    scaleBilinear,
    scaleTo,

    -- * Shear
    shearH,
    shearV,

    -- * Effects
    outline,
    dropShadow,
  )
where

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), generatePixelData, getPixel)
import GBSprite.Color (Color (..), alphaBlend, lerp, transparent)

-- | Flip horizontally (mirror left-right).
flipH :: Canvas -> Canvas
flipH canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in Canvas w h $ generatePixels w h $ \x y ->
        let srcIdx = (y * w + (w - 1 - x)) * bytesPerPixel
         in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Flip vertically (mirror top-bottom).
flipV :: Canvas -> Canvas
flipV canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in Canvas w h $ generatePixels w h $ \x y ->
        let srcIdx = ((h - 1 - y) * w + x) * bytesPerPixel
         in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Rotate 90 degrees clockwise. Width and height swap.
rotate90 :: Canvas -> Canvas
rotate90 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in Canvas h w $ generatePixels h w $ \x y ->
        let srcIdx = ((h - 1 - x) * w + y) * bytesPerPixel
         in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Rotate 180 degrees.
rotate180 :: Canvas -> Canvas
rotate180 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in Canvas w h $ generatePixels w h $ \x y ->
        let srcIdx = ((h - 1 - y) * w + (w - 1 - x)) * bytesPerPixel
         in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Rotate 270 degrees clockwise (= 90 degrees counter-clockwise).
rotate270 :: Canvas -> Canvas
rotate270 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in Canvas h w $ generatePixels h w $ \x y ->
        let srcIdx = (x * w + (w - 1 - y)) * bytesPerPixel
         in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Scale using nearest-neighbor interpolation.
--
-- @scaleNearest factor canvas@ produces a canvas @factor@ times larger.
-- Factor must be >= 1 (values < 1 are treated as 1).
scaleNearest :: Int -> Canvas -> Canvas
scaleNearest factor canvas
  | factor <= 1 = canvas
  | otherwise =
      let w = cWidth canvas
          newW = w * factor
          newH = cHeight canvas * factor
          src = cPixels canvas
       in Canvas newW newH $ generatePixels newW newH $ \x y ->
            let srcIdx = ((y `div` factor) * w + (x `div` factor)) * bytesPerPixel
             in \ch -> src `unsafeIndex` (srcIdx + ch)

-- | Add an outline around non-transparent pixels.
--
-- For each transparent pixel adjacent to a non-transparent pixel,
-- fill it with the outline color.
outline :: Color -> Canvas -> Canvas
outline outlineColor canvas =
  let w = cWidth canvas
      h = cHeight canvas
      Color oR oG oB oA = outlineColor
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            pixel = getPixel canvas x y
         in if colorA pixel == 0 && hasOpaqueNeighbor w h x y
              then colorChannel channel oR oG oB oA
              else
                let srcIdx = pixIdx * bytesPerPixel + channel
                 in unsafeIndex (cPixels canvas) srcIdx
   in Canvas w h pixels
  where
    hasOpaqueNeighbor w h x y =
      checkNeighbor (x - 1) y
        || checkNeighbor (x + 1) y
        || checkNeighbor x (y - 1)
        || checkNeighbor x (y + 1)
      where
        checkNeighbor nx ny
          | nx >= 0 && nx < w && ny >= 0 && ny < h = colorA (getPixel canvas nx ny) > 0
          | otherwise = False

-- | Add a drop shadow offset by @(dx, dy)@.
--
-- The shadow is a semi-transparent version of the sprite,
-- composited behind the original.
dropShadow :: Int -> Int -> Color -> Canvas -> Canvas
dropShadow dx dy shadowColor canvas =
  let w = cWidth canvas
      h = cHeight canvas
      padW = w + abs dx
      padH = h + abs dy
      shadowOffX = max 0 dx
      shadowOffY = max 0 dy
      origOffX = max 0 (negate dx)
      origOffY = max 0 (negate dy)
      Color sR sG sB sA = shadowColor
      pixels = generatePixelData (padW * padH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            px = pixIdx `mod` padW
            py = pixIdx `div` padW
            -- Original sprite coordinates
            ox = px - origOffX
            oy = py - origOffY
            origPixel =
              if ox >= 0 && ox < w && oy >= 0 && oy < h
                then getPixel canvas ox oy
                else transparent
            -- Shadow sprite coordinates
            sx = px - shadowOffX
            sy = py - shadowOffY
            hasShadow =
              sx >= 0
                && sx < w
                && sy >= 0
                && sy < h
                && colorA (getPixel canvas sx sy) > 0
         in if colorA origPixel > 0
              then
                -- Original on top, blend over shadow if present
                if hasShadow
                  then
                    let Color bR bG bB bA = alphaBlend origPixel shadowColor
                     in colorChannel channel bR bG bB bA
                  else colorChannel channel (colorR origPixel) (colorG origPixel) (colorB origPixel) (colorA origPixel)
              else
                if hasShadow
                  then colorChannel channel sR sG sB sA
                  else 0
   in Canvas padW padH pixels

-- | Rotate by an arbitrary angle (degrees, clockwise) with bilinear
-- interpolation. The output canvas is sized to contain the full
-- rotated image.
rotateArbitrary :: Double -> Canvas -> Canvas
rotateArbitrary angleDeg canvas
  | angleDeg == 0 = canvas
  | otherwise =
      let w = cWidth canvas
          h = cHeight canvas
          radians = angleDeg * degToRad
          cosA = cos radians
          sinA = sin radians
          absCos = abs cosA
          absSin = abs sinA
          newW = ceiling (fromIntegral w * absCos + fromIntegral h * absSin :: Double)
          newH = ceiling (fromIntegral w * absSin + fromIntegral h * absCos :: Double)
          centerSrcX = fromIntegral w / 2.0 :: Double
          centerSrcY = fromIntegral h / 2.0 :: Double
          centerDstX = fromIntegral newW / 2.0 :: Double
          centerDstY = fromIntegral newH / 2.0 :: Double
       in Canvas newW newH $ generatePixels newW newH $ \ox oy ->
            let dx = fromIntegral ox - centerDstX + 0.5
                dy = fromIntegral oy - centerDstY + 0.5
                srcX = dx * cosA + dy * sinA + centerSrcX - 0.5
                srcY = negate dx * sinA + dy * cosA + centerSrcY - 0.5
                Color sr sg sb sa = bilinearSample canvas srcX srcY
             in \ch -> colorChannel ch sr sg sb sa

-- | Scale using bilinear interpolation. Works for both up and down
-- scaling. Factor must be positive (values <= 0 are treated as 1).
scaleBilinear :: Double -> Canvas -> Canvas
scaleBilinear factor canvas
  | factor <= 0.0 = canvas
  | abs (factor - 1.0) < scaleEpsilon = canvas
  | otherwise =
      let w = cWidth canvas
          h = cHeight canvas
          newW = max 1 (round (fromIntegral w * factor))
          newH = max 1 (round (fromIntegral h * factor))
       in scaleToImpl newW newH canvas

-- | Scale to exact target dimensions using bilinear interpolation.
-- Dimensions must be positive (values <= 0 are treated as 1).
scaleTo :: Int -> Int -> Canvas -> Canvas
scaleTo targetW targetH =
  scaleToImpl (max 1 targetW) (max 1 targetH)

-- | Shear horizontally. Each row is shifted by @factor * (y - height\/2)@
-- pixels. Positive values shift bottom rows right.
shearH :: Double -> Canvas -> Canvas
shearH factor canvas
  | abs factor < scaleEpsilon = canvas
  | otherwise =
      let w = cWidth canvas
          h = cHeight canvas
          maxShift = abs factor * fromIntegral h / 2.0
          newW = w + ceiling (2.0 * maxShift)
          offsetX = ceiling maxShift :: Int
       in Canvas newW h $ generatePixels newW h $ \ox oy ->
            let shift = factor * (fromIntegral oy - fromIntegral h / 2.0)
                srcX = fromIntegral ox - fromIntegral offsetX + shift
                srcY = fromIntegral oy
                Color sr sg sb sa = bilinearSample canvas srcX srcY
             in \ch -> colorChannel ch sr sg sb sa

-- | Shear vertically. Each column is shifted by @factor * (x - width\/2)@
-- pixels. Positive values shift right columns down.
shearV :: Double -> Canvas -> Canvas
shearV factor canvas
  | abs factor < scaleEpsilon = canvas
  | otherwise =
      let w = cWidth canvas
          h = cHeight canvas
          maxShift = abs factor * fromIntegral w / 2.0
          newH = h + ceiling (2.0 * maxShift)
          offsetY = ceiling maxShift :: Int
       in Canvas w newH $ generatePixels w newH $ \ox oy ->
            let shift = factor * (fromIntegral ox - fromIntegral w / 2.0)
                srcX = fromIntegral ox
                srcY = fromIntegral oy - fromIntegral offsetY + shift
                Color sr sg sb sa = bilinearSample canvas srcX srcY
             in \ch -> colorChannel ch sr sg sb sa

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

scaleEpsilon :: Double
scaleEpsilon = 1.0e-10

degToRad :: Double
degToRad = pi / 180.0

scaleToImpl :: Int -> Int -> Canvas -> Canvas
scaleToImpl newW newH canvas =
  let w = cWidth canvas
      h = cHeight canvas
      ratioX = fromIntegral w / fromIntegral newW :: Double
      ratioY = fromIntegral h / fromIntegral newH :: Double
   in Canvas newW newH $ generatePixels newW newH $ \ox oy ->
        let srcX = (fromIntegral ox + 0.5) * ratioX - 0.5
            srcY = (fromIntegral oy + 0.5) * ratioY - 0.5
            Color sr sg sb sa = bilinearSample canvas srcX srcY
         in \ch -> colorChannel ch sr sg sb sa

bilinearSample :: Canvas -> Double -> Double -> Color
bilinearSample canvas fx fy =
  let ix = floor fx :: Int
      iy = floor fy :: Int
      fracX = fx - fromIntegral ix
      fracY = fy - fromIntegral iy
      c00 = getPixel canvas ix iy
      c10 = getPixel canvas (ix + 1) iy
      c01 = getPixel canvas ix (iy + 1)
      c11 = getPixel canvas (ix + 1) (iy + 1)
      top = lerp fracX c00 c10
      bot = lerp fracX c01 c11
   in lerp fracY top bot

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Generate a pixel vector from a coordinate-to-channel function.
-- The function receives (x, y) and returns a channel reader (channelIdx -> byte).
generatePixels :: Int -> Int -> (Int -> Int -> Int -> Word8) -> BS.ByteString
generatePixels w h pixelFn = generatePixelData (w * h * bytesPerPixel) $ \i ->
  let pixIdx = i `div` bytesPerPixel
      channel = i `mod` bytesPerPixel
      x = pixIdx `mod` w
      y = pixIdx `div` w
   in pixelFn x y channel

-- | Extract an RGBA channel by index (0=R, 1=G, 2=B, 3=A).
colorChannel :: Int -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
colorChannel 0 r _ _ _ = r
colorChannel 1 _ g _ _ = g
colorChannel 2 _ _ b _ = b
colorChannel _ _ _ _ a = a
