-- | Gradient generation for backgrounds and effects.
--
-- Pure gradient constructors that produce t'Canvas' values.
-- Useful for backgrounds, UI elements, and color ramps.
module GBSprite.Gradient
  ( -- * Gradients
    linearGradient,
    radialGradient,
    diagonalGradient,
  )
where

import GBSprite.Canvas (Canvas, generateCanvasPixels)
import GBSprite.Color (Color, lerp)

-- | Generate a linear gradient canvas.
--
-- @linearGradient width height startColor endColor horizontal@
-- produces a gradient that transitions from @startColor@ to @endColor@.
-- When @horizontal@ is 'True', the gradient goes left to right;
-- otherwise top to bottom.
linearGradient :: Int -> Int -> Color -> Color -> Bool -> Canvas
linearGradient w h startColor endColor horizontal =
  generateCanvasPixels w h $ \x y ->
    let t
          | horizontal = if w <= 1 then 0.0 else fromIntegral x / fromIntegral (w - 1)
          | h <= 1 = 0.0
          | otherwise = fromIntegral y / fromIntegral (h - 1)
     in lerp t startColor endColor

-- | Generate a radial gradient canvas.
--
-- @radialGradient width height centerX centerY radius innerColor outerColor@
-- produces a circular gradient centered at @(centerX, centerY)@.
-- Pixels beyond @radius@ are filled with @outerColor@.
radialGradient :: Int -> Int -> Int -> Int -> Int -> Color -> Color -> Canvas
radialGradient w h cx cy radius innerColor outerColor =
  let radiusF = fromIntegral (max 1 radius) :: Double
   in generateCanvasPixels w h $ \x y ->
        let dx = fromIntegral (x - cx) :: Double
            dy = fromIntegral (y - cy) :: Double
            dist = sqrt (dx * dx + dy * dy)
            t = min 1.0 (dist / radiusF)
         in lerp t innerColor outerColor

-- | Generate a diagonal gradient canvas.
--
-- @diagonalGradient width height startColor endColor@
-- transitions from @startColor@ at the top-left to @endColor@
-- at the bottom-right.
diagonalGradient :: Int -> Int -> Color -> Color -> Canvas
diagonalGradient w h startColor endColor =
  let maxDist = fromIntegral (max 1 (w - 1 + h - 1)) :: Double
   in generateCanvasPixels w h $ \x y ->
        let dist = fromIntegral (x + y) :: Double
            t = dist / maxDist
         in lerp t startColor endColor
