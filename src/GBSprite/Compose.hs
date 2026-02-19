-- | Canvas compositing: layering, stamping, and blending.
--
-- Uses single-pass vector generation for O(n) performance.
module GBSprite.Compose
  ( -- * Compositing
    stamp,
    stampAlpha,
    overlay,
    overlayAt,
  )
where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..))
import GBSprite.Color (Color (..), alphaBlend)

-- | Stamp @src@ onto @dst@ at position @(x, y)@ with direct overwrite.
--
-- Only non-transparent pixels are copied.
stamp :: Canvas -> Int -> Int -> Canvas -> Canvas
stamp dst ox oy src =
  let dstW = cWidth dst
      dstH = cHeight dst
      dstPx = cPixels dst
      srcW = cWidth src
      srcH = cHeight src
      srcPx = cPixels src
      pixels = VS.generate (dstW * dstH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            dx = pixIdx `mod` dstW
            dy = pixIdx `div` dstW
            sx = dx - ox
            sy = dy - oy
         in if sx >= 0 && sx < srcW && sy >= 0 && sy < srcH
              then
                let srcIdx = (sy * srcW + sx) * bytesPerPixel
                    sa = srcPx `VS.unsafeIndex` (srcIdx + 3)
                 in if sa > 0
                      then srcPx `VS.unsafeIndex` (srcIdx + channel)
                      else dstPx `VS.unsafeIndex` i
              else dstPx `VS.unsafeIndex` i
   in dst {cPixels = pixels}

-- | Stamp @src@ onto @dst@ at position @(x, y)@ with alpha blending.
stampAlpha :: Canvas -> Int -> Int -> Canvas -> Canvas
stampAlpha dst ox oy src =
  let dstW = cWidth dst
      dstH = cHeight dst
      dstPx = cPixels dst
      srcW = cWidth src
      srcH = cHeight src
      srcPx = cPixels src
      pixels = VS.generate (dstW * dstH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            dx = pixIdx `mod` dstW
            dy = pixIdx `div` dstW
            sx = dx - ox
            sy = dy - oy
         in if sx >= 0 && sx < srcW && sy >= 0 && sy < srcH
              then
                let srcIdx = (sy * srcW + sx) * bytesPerPixel
                    sa = srcPx `VS.unsafeIndex` (srcIdx + 3)
                 in if sa > 0
                      then
                        let sr = srcPx `VS.unsafeIndex` srcIdx
                            sg = srcPx `VS.unsafeIndex` (srcIdx + 1)
                            sb = srcPx `VS.unsafeIndex` (srcIdx + 2)
                            dstIdx = pixIdx * bytesPerPixel
                            dr = dstPx `VS.unsafeIndex` dstIdx
                            dg = dstPx `VS.unsafeIndex` (dstIdx + 1)
                            db = dstPx `VS.unsafeIndex` (dstIdx + 2)
                            da = dstPx `VS.unsafeIndex` (dstIdx + 3)
                            Color bR bG bB bA =
                              alphaBlend
                                (Color sr sg sb sa)
                                (Color dr dg db da)
                         in colorChannel channel bR bG bB bA
                      else dstPx `VS.unsafeIndex` i
              else dstPx `VS.unsafeIndex` i
   in dst {cPixels = pixels}

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

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Extract an RGBA channel by index (0=R, 1=G, 2=B, 3=A).
colorChannel :: Int -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
colorChannel 0 r _ _ _ = r
colorChannel 1 _ g _ _ = g
colorChannel 2 _ _ b _ = b
colorChannel _ _ _ _ a = a
