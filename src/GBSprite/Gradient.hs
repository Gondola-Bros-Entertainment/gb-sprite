-- | Gradient generation for backgrounds and effects.
--
-- Pure gradient constructors that produce 'Canvas' values.
-- Useful for backgrounds, UI elements, and color ramps.
module GBSprite.Gradient
  ( -- * Gradients
    linearGradient,
    radialGradient,
    diagonalGradient,
  )
where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..))
import GBSprite.Color (Color (..), lerp)

-- | Generate a linear gradient canvas.
--
-- @linearGradient width height startColor endColor horizontal@
-- produces a gradient that transitions from @startColor@ to @endColor@.
-- When @horizontal@ is 'True', the gradient goes left to right;
-- otherwise top to bottom.
linearGradient :: Int -> Int -> Color -> Color -> Bool -> Canvas
linearGradient w h startColor endColor horizontal =
  let pixels = VS.generate (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            t
              | horizontal = if w <= 1 then 0.0 else fromIntegral x / fromIntegral (w - 1)
              | h <= 1 = 0.0
              | otherwise = fromIntegral y / fromIntegral (h - 1)
            Color r g b a = lerp t startColor endColor
         in colorChannel channel r g b a
   in Canvas w h pixels

-- | Generate a radial gradient canvas.
--
-- @radialGradient width height centerX centerY radius innerColor outerColor@
-- produces a circular gradient centered at @(centerX, centerY)@.
-- Pixels beyond @radius@ are filled with @outerColor@.
radialGradient :: Int -> Int -> Int -> Int -> Int -> Color -> Color -> Canvas
radialGradient w h cx cy radius innerColor outerColor =
  let radiusF = fromIntegral (max 1 radius) :: Double
      pixels = VS.generate (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            dx = fromIntegral (x - cx) :: Double
            dy = fromIntegral (y - cy) :: Double
            dist = sqrt (dx * dx + dy * dy)
            t = min 1.0 (dist / radiusF)
            Color r g b a = lerp t innerColor outerColor
         in colorChannel channel r g b a
   in Canvas w h pixels

-- | Generate a diagonal gradient canvas.
--
-- @diagonalGradient width height startColor endColor@
-- transitions from @startColor@ at the top-left to @endColor@
-- at the bottom-right.
diagonalGradient :: Int -> Int -> Color -> Color -> Canvas
diagonalGradient w h startColor endColor =
  let maxDist = fromIntegral (max 1 (w - 1 + h - 1)) :: Double
      pixels = VS.generate (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            dist = fromIntegral (x + y) :: Double
            t = dist / maxDist
            Color r g b a = lerp t startColor endColor
         in colorChannel channel r g b a
   in Canvas w h pixels

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Extract an RGBA channel by index (0=R, 1=G, 2=B, 3=A).
colorChannel :: Int -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
colorChannel 0 r _ _ _ = r
colorChannel 1 _ g _ _ = g
colorChannel 2 _ _ b _ = b
colorChannel _ _ _ _ a = a
