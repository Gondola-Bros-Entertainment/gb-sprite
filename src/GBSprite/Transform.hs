-- | Canvas transformations: flip, rotate, scale.
--
-- All transforms produce new canvases — the originals are unmodified.
-- Uses single-pass pixel generation for O(n) performance.
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

import Data.ByteString.Unsafe (unsafeIndex)
import GBSprite.Canvas (Canvas (..), generateCanvasPixels, getPixel)
import GBSprite.Color (Color (..), alphaBlend, lerp, transparent)

-- | Flip horizontally (mirror left-right).
flipH :: Canvas -> Canvas
flipH canvas =
  let w = cWidth canvas
      src = cPixels canvas
   in generateCanvasPixels w (cHeight canvas) $ \x y ->
        let srcIdx = (y * w + (w - 1 - x)) * bytesPerPixel
         in Color
              (src `unsafeIndex` srcIdx)
              (src `unsafeIndex` (srcIdx + 1))
              (src `unsafeIndex` (srcIdx + 2))
              (src `unsafeIndex` (srcIdx + 3))

-- | Flip vertically (mirror top-bottom).
flipV :: Canvas -> Canvas
flipV canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in generateCanvasPixels w h $ \x y ->
        let srcIdx = ((h - 1 - y) * w + x) * bytesPerPixel
         in Color
              (src `unsafeIndex` srcIdx)
              (src `unsafeIndex` (srcIdx + 1))
              (src `unsafeIndex` (srcIdx + 2))
              (src `unsafeIndex` (srcIdx + 3))

-- | Rotate 90 degrees clockwise. Width and height swap.
rotate90 :: Canvas -> Canvas
rotate90 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in generateCanvasPixels h w $ \x y ->
        let srcIdx = ((h - 1 - x) * w + y) * bytesPerPixel
         in Color
              (src `unsafeIndex` srcIdx)
              (src `unsafeIndex` (srcIdx + 1))
              (src `unsafeIndex` (srcIdx + 2))
              (src `unsafeIndex` (srcIdx + 3))

-- | Rotate 180 degrees.
rotate180 :: Canvas -> Canvas
rotate180 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in generateCanvasPixels w h $ \x y ->
        let srcIdx = ((h - 1 - y) * w + (w - 1 - x)) * bytesPerPixel
         in Color
              (src `unsafeIndex` srcIdx)
              (src `unsafeIndex` (srcIdx + 1))
              (src `unsafeIndex` (srcIdx + 2))
              (src `unsafeIndex` (srcIdx + 3))

-- | Rotate 270 degrees clockwise (= 90 degrees counter-clockwise).
rotate270 :: Canvas -> Canvas
rotate270 canvas =
  let w = cWidth canvas
      src = cPixels canvas
   in generateCanvasPixels (cHeight canvas) w $ \x y ->
        let srcIdx = (x * w + (w - 1 - y)) * bytesPerPixel
         in Color
              (src `unsafeIndex` srcIdx)
              (src `unsafeIndex` (srcIdx + 1))
              (src `unsafeIndex` (srcIdx + 2))
              (src `unsafeIndex` (srcIdx + 3))

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
       in generateCanvasPixels newW newH $ \x y ->
            let srcIdx = ((y `div` factor) * w + (x `div` factor)) * bytesPerPixel
             in Color
                  (src `unsafeIndex` srcIdx)
                  (src `unsafeIndex` (srcIdx + 1))
                  (src `unsafeIndex` (srcIdx + 2))
                  (src `unsafeIndex` (srcIdx + 3))

-- | Add an outline around non-transparent pixels.
--
-- For each transparent pixel adjacent to a non-transparent pixel,
-- fill it with the outline color.
outline :: Color -> Canvas -> Canvas
outline outlineColor canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
   in generateCanvasPixels w h $ \x y ->
        let pixel = getPixel canvas x y
         in if colorA pixel == 0 && hasOpaqueNeighbor w h x y
              then outlineColor
              else
                let srcIdx = (y * w + x) * bytesPerPixel
                 in Color
                      (src `unsafeIndex` srcIdx)
                      (src `unsafeIndex` (srcIdx + 1))
                      (src `unsafeIndex` (srcIdx + 2))
                      (src `unsafeIndex` (srcIdx + 3))
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
   in generateCanvasPixels padW padH $ \px py ->
        let -- Original sprite coordinates
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
                if hasShadow
                  then alphaBlend origPixel shadowColor
                  else origPixel
              else
                if hasShadow
                  then shadowColor
                  else transparent

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
       in generateCanvasPixels newW newH $ \ox oy ->
            let dxF = fromIntegral ox - centerDstX + 0.5
                dyF = fromIntegral oy - centerDstY + 0.5
                srcX = dxF * cosA + dyF * sinA + centerSrcX - 0.5
                srcY = negate dxF * sinA + dyF * cosA + centerSrcY - 0.5
             in bilinearSample canvas srcX srcY

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
      let h = cHeight canvas
          maxShift = abs factor * fromIntegral h / 2.0
          newW = cWidth canvas + ceiling (2.0 * maxShift)
          offsetX = ceiling maxShift :: Int
       in generateCanvasPixels newW h $ \ox oy ->
            let shift = factor * (fromIntegral oy - fromIntegral h / 2.0)
                srcX = fromIntegral ox - fromIntegral offsetX - shift
                srcY = fromIntegral oy
             in bilinearSample canvas srcX srcY

-- | Shear vertically. Each column is shifted by @factor * (x - width\/2)@
-- pixels. Positive values shift right columns down.
shearV :: Double -> Canvas -> Canvas
shearV factor canvas
  | abs factor < scaleEpsilon = canvas
  | otherwise =
      let w = cWidth canvas
          maxShift = abs factor * fromIntegral w / 2.0
          newH = cHeight canvas + ceiling (2.0 * maxShift)
          offsetY = ceiling maxShift :: Int
       in generateCanvasPixels w newH $ \ox oy ->
            let shift = factor * (fromIntegral ox - fromIntegral w / 2.0)
                srcX = fromIntegral ox
                srcY = fromIntegral oy - fromIntegral offsetY - shift
             in bilinearSample canvas srcX srcY

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
   in generateCanvasPixels newW newH $ \ox oy ->
        let srcX = (fromIntegral ox + 0.5) * ratioX - 0.5
            srcY = (fromIntegral oy + 0.5) * ratioY - 0.5
         in bilinearSample canvas srcX srcY

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
