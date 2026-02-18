-- | Canvas compositing: layering, stamping, and blending.
module GBSprite.Compose
  ( -- * Compositing
    stamp,
    stampAlpha,
    overlay,
    overlayAt,
  )
where

import GBSprite.Canvas (Canvas (..), getPixel, setPixel)
import GBSprite.Color (Color (..), alphaBlend)

-- | Stamp @src@ onto @dst@ at position @(x, y)@ with direct overwrite.
--
-- Only non-transparent pixels are copied.
stamp :: Canvas -> Int -> Int -> Canvas -> Canvas
stamp dst x y src =
  let srcW = cWidth src
      srcH = cHeight src
   in foldSrcPixels
        dst
        srcW
        srcH
        ( \c sx sy ->
            let pixel = getPixel src sx sy
             in if colorA pixel > 0
                  then setPixel c (x + sx) (y + sy) pixel
                  else c
        )

-- | Stamp @src@ onto @dst@ at position @(x, y)@ with alpha blending.
stampAlpha :: Canvas -> Int -> Int -> Canvas -> Canvas
stampAlpha dst x y src =
  let srcW = cWidth src
      srcH = cHeight src
   in foldSrcPixels
        dst
        srcW
        srcH
        ( \c sx sy ->
            let srcPixel = getPixel src sx sy
             in if colorA srcPixel > 0
                  then
                    let dstPixel = getPixel c (x + sx) (y + sy)
                        blended = alphaBlend srcPixel dstPixel
                     in setPixel c (x + sx) (y + sy) blended
                  else c
        )

-- | Overlay @top@ onto @bottom@ (same size), producing a new canvas.
--
-- Both canvases must have the same dimensions. If they differ,
-- the output uses the dimensions of @bottom@.
overlay :: Canvas -> Canvas -> Canvas
overlay bottom = overlayAt bottom 0 0

-- | Overlay @top@ onto @bottom@ at an offset.
overlayAt :: Canvas -> Int -> Int -> Canvas -> Canvas
overlayAt = stampAlpha

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Fold over all pixels in a source canvas.
foldSrcPixels :: Canvas -> Int -> Int -> (Canvas -> Int -> Int -> Canvas) -> Canvas
foldSrcPixels canvas srcW srcH f = foldRows canvas 0
  where
    foldRows c sy
      | sy >= srcH = c
      | otherwise = foldRows (foldCols c 0 sy) (sy + 1)
    foldCols c sx sy
      | sx >= srcW = c
      | otherwise = foldCols (f c sx sy) (sx + 1) sy
