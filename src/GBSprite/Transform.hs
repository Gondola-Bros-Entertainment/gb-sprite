-- | Canvas transformations: flip, rotate, scale.
--
-- All transforms produce new canvases — the originals are unmodified.
module GBSprite.Transform
  ( -- * Flip
    flipH,
    flipV,

    -- * Rotate
    rotate90,
    rotate180,
    rotate270,

    -- * Scale
    scaleNearest,

    -- * Effects
    outline,
    dropShadow,
  )
where

import GBSprite.Canvas (Canvas (..), getPixel, newCanvas, setPixel)
import GBSprite.Color (Color (..), alphaBlend, transparent)

-- | Flip horizontally (mirror left-right).
flipH :: Canvas -> Canvas
flipH canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blank = newCanvas w h transparent
   in foldPixels blank w h (\c x y -> setPixel c x y (getPixel canvas (w - 1 - x) y))

-- | Flip vertically (mirror top-bottom).
flipV :: Canvas -> Canvas
flipV canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blank = newCanvas w h transparent
   in foldPixels blank w h (\c x y -> setPixel c x y (getPixel canvas x (h - 1 - y)))

-- | Rotate 90 degrees clockwise. Width and height swap.
rotate90 :: Canvas -> Canvas
rotate90 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blank = newCanvas h w transparent
   in foldPixels blank h w (\c x y -> setPixel c x y (getPixel canvas y (h - 1 - x)))

-- | Rotate 180 degrees.
rotate180 :: Canvas -> Canvas
rotate180 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blank = newCanvas w h transparent
   in foldPixels blank w h (\c x y -> setPixel c x y (getPixel canvas (w - 1 - x) (h - 1 - y)))

-- | Rotate 270 degrees clockwise (= 90 degrees counter-clockwise).
rotate270 :: Canvas -> Canvas
rotate270 canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blank = newCanvas h w transparent
   in foldPixels blank h w (\c x y -> setPixel c x y (getPixel canvas (w - 1 - y) x))

-- | Scale using nearest-neighbor interpolation.
--
-- @scaleNearest factor canvas@ produces a canvas @factor@ times larger.
-- Factor must be >= 1 (values < 1 are treated as 1).
scaleNearest :: Int -> Canvas -> Canvas
scaleNearest factor canvas
  | factor <= 1 = canvas
  | otherwise =
      let w = cWidth canvas
          h = cHeight canvas
          newW = w * factor
          newH = h * factor
          blank = newCanvas newW newH transparent
       in foldPixels blank newW newH (\c x y -> setPixel c x y (getPixel canvas (x `div` factor) (y `div` factor)))

-- | Add an outline around non-transparent pixels.
--
-- For each transparent pixel adjacent to a non-transparent pixel,
-- fill it with the outline color.
outline :: Color -> Canvas -> Canvas
outline outlineColor canvas =
  let w = cWidth canvas
      h = cHeight canvas
   in foldPixels canvas w h (addOutline w h)
  where
    addOutline w h c x y
      | isTransparent (getPixel canvas x y) && hasOpaqueNeighbor w h x y =
          setPixel c x y outlineColor
      | otherwise = c

    hasOpaqueNeighbor w h x y =
      not (all isTransparent neighbors)
      where
        neighbors =
          [ getPixel canvas nx ny
          | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
            let nx = x + dx
                ny = y + dy,
            nx >= 0,
            nx < w,
            ny >= 0,
            ny < h
          ]

    isTransparent (Color _ _ _ a) = a == 0

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
      blank = newCanvas padW padH transparent
      -- Draw shadow first (behind)
      shadowOffX = max 0 dx
      shadowOffY = max 0 dy
      withShadow = foldPixels blank w h (drawShadowPixel shadowOffX shadowOffY)
      -- Draw original on top
      origOffX = max 0 (negate dx)
      origOffY = max 0 (negate dy)
   in foldPixels withShadow w h (drawOrigPixel origOffX origOffY)
  where
    drawShadowPixel offX offY c x y =
      let pixel = getPixel canvas x y
       in if colorA pixel > 0
            then setPixel c (x + offX) (y + offY) shadowColor
            else c

    drawOrigPixel offX offY c x y =
      let pixel = getPixel canvas x y
       in if colorA pixel > 0
            then
              let existing = getPixel c (x + offX) (y + offY)
                  blended = alphaBlend pixel existing
               in setPixel c (x + offX) (y + offY) blended
            else c

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Fold over all pixels in a width x height grid.
foldPixels :: Canvas -> Int -> Int -> (Canvas -> Int -> Int -> Canvas) -> Canvas
foldPixels canvas w h f = foldlRows canvas 0
  where
    foldlRows c y
      | y >= h = c
      | otherwise = foldlRows (foldlCols c 0 y) (y + 1)
    foldlCols c x y
      | x >= w = c
      | otherwise = foldlCols (f c x y) (x + 1) y
