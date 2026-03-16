-- | Canvas compositing: layering, stamping, and blending.
--
-- Uses single-pass vector generation for O(n) performance.
module GBSprite.Compose
  ( -- * Compositing
    stamp,
    stampAlpha,
    overlay,
    overlayAt,

    -- * Blend modes
    BlendMode (..),
    blendCompose,

    -- * Masking
    maskCanvas,
  )
where

import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), generatePixelData)
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
      pixels = generatePixelData (dstW * dstH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            dx = pixIdx `mod` dstW
            dy = pixIdx `div` dstW
            sx = dx - ox
            sy = dy - oy
         in if sx >= 0 && sx < srcW && sy >= 0 && sy < srcH
              then
                let srcIdx = (sy * srcW + sx) * bytesPerPixel
                    sa = srcPx `unsafeIndex` (srcIdx + 3)
                 in if sa > 0
                      then srcPx `unsafeIndex` (srcIdx + channel)
                      else dstPx `unsafeIndex` i
              else dstPx `unsafeIndex` i
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
      pixels = generatePixelData (dstW * dstH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            dx = pixIdx `mod` dstW
            dy = pixIdx `div` dstW
            sx = dx - ox
            sy = dy - oy
         in if sx >= 0 && sx < srcW && sy >= 0 && sy < srcH
              then
                let srcIdx = (sy * srcW + sx) * bytesPerPixel
                    sa = srcPx `unsafeIndex` (srcIdx + 3)
                 in if sa > 0
                      then
                        let sr = srcPx `unsafeIndex` srcIdx
                            sg = srcPx `unsafeIndex` (srcIdx + 1)
                            sb = srcPx `unsafeIndex` (srcIdx + 2)
                            dstIdx = pixIdx * bytesPerPixel
                            dr = dstPx `unsafeIndex` dstIdx
                            dg = dstPx `unsafeIndex` (dstIdx + 1)
                            db = dstPx `unsafeIndex` (dstIdx + 2)
                            da = dstPx `unsafeIndex` (dstIdx + 3)
                            Color bR bG bB bA =
                              alphaBlend
                                (Color sr sg sb sa)
                                (Color dr dg db da)
                         in colorChannel channel bR bG bB bA
                      else dstPx `unsafeIndex` i
              else dstPx `unsafeIndex` i
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
-- Blend modes
-- ---------------------------------------------------------------------------

-- | Pixel-level blend modes for compositing.
data BlendMode
  = -- | Darkens: @src * dst / 255@
    BlendMultiply
  | -- | Lightens: @255 - (255 - src) * (255 - dst) / 255@
    BlendScreen
  | -- | Combines multiply and screen based on dst brightness
    BlendOverlay
  | -- | Adds channels, clamping at 255
    BlendAdditive
  | -- | Gentle lightening with reduced contrast
    BlendSoftLight
  | -- | Absolute difference: @|src - dst|@
    BlendDifference
  deriving (Show, Eq)

-- | Compose @src@ onto @dst@ at @(ox, oy)@ using the given blend mode.
-- Source alpha is respected: transparent source pixels leave dst unchanged.
blendCompose :: BlendMode -> Canvas -> Int -> Int -> Canvas -> Canvas
blendCompose mode dst ox oy src =
  let dstW = cWidth dst
      dstH = cHeight dst
      dstPx = cPixels dst
      srcW = cWidth src
      srcH = cHeight src
      srcPx = cPixels src
      pixels = generatePixelData (dstW * dstH * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            dx = pixIdx `mod` dstW
            dy = pixIdx `div` dstW
            sx = dx - ox
            sy = dy - oy
         in if sx >= 0 && sx < srcW && sy >= 0 && sy < srcH
              then
                let srcIdx = (sy * srcW + sx) * bytesPerPixel
                    sa = srcPx `unsafeIndex` (srcIdx + 3)
                 in if sa > 0
                      then
                        let sr = srcPx `unsafeIndex` srcIdx
                            sg = srcPx `unsafeIndex` (srcIdx + 1)
                            sb = srcPx `unsafeIndex` (srcIdx + 2)
                            dstIdx = pixIdx * bytesPerPixel
                            dr = dstPx `unsafeIndex` dstIdx
                            dg = dstPx `unsafeIndex` (dstIdx + 1)
                            db = dstPx `unsafeIndex` (dstIdx + 2)
                            da = dstPx `unsafeIndex` (dstIdx + 3)
                            br = blendChannel mode sr dr
                            bg = blendChannel mode sg dg
                            bb = blendChannel mode sb db
                            -- Mix blended result with dst using src alpha
                            srcA = fromIntegral sa :: Int
                            outR = clampByte ((fromIntegral br * srcA + fromIntegral dr * (channelMax - srcA)) `div` channelMax)
                            outG = clampByte ((fromIntegral bg * srcA + fromIntegral dg * (channelMax - srcA)) `div` channelMax)
                            outB = clampByte ((fromIntegral bb * srcA + fromIntegral db * (channelMax - srcA)) `div` channelMax)
                            outA = clampByte (fromIntegral sa + fromIntegral da * (channelMax - fromIntegral sa) `div` channelMax)
                         in colorChannel channel outR outG outB outA
                      else dstPx `unsafeIndex` i
              else dstPx `unsafeIndex` i
   in dst {cPixels = pixels}

-- ---------------------------------------------------------------------------
-- Masking
-- ---------------------------------------------------------------------------

-- | Apply an alpha mask to a canvas. The mask's alpha channel
-- controls the output alpha: opaque mask = fully visible, transparent = hidden.
-- Mask and canvas must be the same size; if they differ, uses the
-- canvas dimensions and treats out-of-bounds mask pixels as opaque.
maskCanvas :: Canvas -> Canvas -> Canvas
maskCanvas canvas mask =
  let w = cWidth canvas
      h = cHeight canvas
      srcPx = cPixels canvas
      maskPx = cPixels mask
      maskW = cWidth mask
      maskH = cHeight mask
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
         in if channel == 3
              then
                let srcA = srcPx `unsafeIndex` i
                    maskAlpha =
                      if x < maskW && y < maskH
                        then
                          let mIdx = (y * maskW + x) * bytesPerPixel + 3
                           in maskPx `unsafeIndex` mIdx
                        else maxByte
                 in clampByte (fromIntegral srcA * fromIntegral maskAlpha `div` channelMax)
              else srcPx `unsafeIndex` i
   in Canvas w h pixels

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

blendChannel :: BlendMode -> Word8 -> Word8 -> Word8
blendChannel BlendMultiply s d =
  clampByte (fromIntegral s * fromIntegral d `div` channelMax)
blendChannel BlendScreen s d =
  clampByte (channelMax - (channelMax - fromIntegral s) * (channelMax - fromIntegral d) `div` channelMax)
blendChannel BlendOverlay s d =
  if d < overlayThreshold
    then clampByte (2 * fromIntegral s * fromIntegral d `div` channelMax)
    else clampByte (channelMax - 2 * (channelMax - fromIntegral s) * (channelMax - fromIntegral d) `div` channelMax)
blendChannel BlendAdditive s d =
  clampByte (fromIntegral s + fromIntegral d)
blendChannel BlendSoftLight s d =
  let sf = fromIntegral s / channelMaxF :: Double
      df = fromIntegral d / channelMaxF :: Double
      result = if sf < 0.5 then df - (1.0 - 2.0 * sf) * df * (1.0 - df) else df + (2.0 * sf - 1.0) * (sqrt df - df)
   in clampByte (round (result * channelMaxF))
blendChannel BlendDifference s d =
  fromIntegral (abs (fromIntegral s - fromIntegral d :: Int))

clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min channelMax n))

overlayThreshold :: Word8
overlayThreshold = 128

channelMax :: Int
channelMax = 255

channelMaxF :: Double
channelMaxF = 255.0

maxByte :: Word8
maxByte = 255

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Extract an RGBA channel by index (0=R, 1=G, 2=B, 3=A).
colorChannel :: Int -> Word8 -> Word8 -> Word8 -> Word8 -> Word8
colorChannel 0 r _ _ _ = r
colorChannel 1 _ g _ _ = g
colorChannel 2 _ _ b _ = b
colorChannel _ _ _ _ a = a
