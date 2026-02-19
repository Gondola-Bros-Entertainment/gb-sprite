-- | Nine-slice scaling for UI panels.
--
-- A nine-slice divides a canvas into 9 regions (4 corners, 4 edges,
-- 1 center). Corners are preserved at original size, edges are
-- stretched along one axis, and the center is stretched in both.
-- Essential for retro game UI (health bars, dialog boxes, menus).
module GBSprite.NineSlice
  ( -- * Types
    NineSlice (..),

    -- * Construction
    nineSlice,

    -- * Rendering
    renderNineSlice,
  )
where

import GBSprite.Canvas (Canvas (..), getPixel, newCanvas, setPixel)
import GBSprite.Color (transparent)

-- | A nine-slice definition: source canvas plus border insets.
data NineSlice = NineSlice
  { -- | Source canvas to slice
    nsCanvas :: !Canvas,
    -- | Left border width in pixels
    nsLeft :: !Int,
    -- | Right border width in pixels
    nsRight :: !Int,
    -- | Top border height in pixels
    nsTop :: !Int,
    -- | Bottom border height in pixels
    nsBottom :: !Int
  }
  deriving (Show, Eq)

-- | Define a nine-slice from a canvas and border insets.
--
-- @nineSlice canvas left right top bottom@ creates a 'NineSlice'
-- with the specified border widths. The insets must be non-negative
-- and fit within the canvas dimensions.
nineSlice :: Canvas -> Int -> Int -> Int -> Int -> NineSlice
nineSlice canvas left right top bottom =
  NineSlice
    { nsCanvas = canvas,
      nsLeft = max 0 left,
      nsRight = max 0 right,
      nsTop = max 0 top,
      nsBottom = max 0 bottom
    }

-- | Render a nine-slice at a target size.
--
-- @renderNineSlice ns targetWidth targetHeight@ produces a new canvas
-- of the given dimensions with the nine-slice regions properly
-- stretched.
renderNineSlice :: NineSlice -> Int -> Int -> Canvas
renderNineSlice ns targetW targetH =
  let srcW = cWidth (nsCanvas ns)
      srcH = cHeight (nsCanvas ns)
      left = min (nsLeft ns) (srcW `div` 2)
      right = min (nsRight ns) (srcW `div` 2)
      top = min (nsTop ns) (srcH `div` 2)
      bottom = min (nsBottom ns) (srcH `div` 2)
      centerSrcW = max 1 (srcW - left - right)
      centerSrcH = max 1 (srcH - top - bottom)
      centerDstW = max 0 (targetW - left - right)
      centerDstH = max 0 (targetH - top - bottom)
   in foldCoords targetW targetH (newCanvas targetW targetH transparent) $
        \canvas x y ->
          let srcX = mapCoord x left right centerSrcW centerDstW srcW targetW
              srcY = mapCoord y top bottom centerSrcH centerDstH srcH targetH
              color = getPixel (nsCanvas ns) srcX srcY
           in setPixel canvas x y color

-- | Map a destination coordinate to its source coordinate.
--
-- For coordinates in the left/top border: pass through unchanged.
-- For coordinates in the center: scale proportionally.
-- For coordinates in the right/bottom border: offset from the end.
mapCoord :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
mapCoord dst border1 border2 centerSrc centerDst srcSize dstSize
  | dst < border1 = dst
  | dst >= dstSize - border2 = srcSize - (dstSize - dst)
  | centerDst <= 0 = border1
  | otherwise =
      let localDst = dst - border1
          mapped = localDst * centerSrc `div` centerDst
       in border1 + min (centerSrc - 1) mapped

-- ---------------------------------------------------------------------------
-- Fold helper
-- ---------------------------------------------------------------------------

-- | Fold over all pixel coordinates in row-major order.
foldCoords :: Int -> Int -> Canvas -> (Canvas -> Int -> Int -> Canvas) -> Canvas
foldCoords w h initial f = go initial 0 0
  where
    go canvas x y
      | y >= h = canvas
      | x >= w = go canvas 0 (y + 1)
      | otherwise = go (f canvas x y) (x + 1) y
