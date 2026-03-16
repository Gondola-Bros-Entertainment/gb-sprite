-- | Convolution-based image filters: blur, sharpen, edge detection, bloom.
--
-- All filters are pure @Canvas -> Canvas@ transforms. Coordinates are
-- clamped at edges (repeat edge pixels) rather than returning transparent
-- for out-of-bounds samples.
module GBSprite.Filter
  ( -- * Blur
    boxBlur,
    gaussianBlur,

    -- * Sharpen
    sharpen,

    -- * Edge detection
    edgeDetect,

    -- * Bloom
    bloom,
  )
where

import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), generatePixelData)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Maximum value for a single color channel.
channelMax :: Int
channelMax = 255

-- | Number of box blur passes used to approximate a Gaussian blur.
gaussianPasses :: Int
gaussianPasses = 3

-- | Blur radius used by the sharpen unsharp mask.
sharpenBlurRadius :: Int
sharpenBlurRadius = 1

-- | Sharpening blend factor for the original image.
sharpenOriginalWeight :: Int
sharpenOriginalWeight = 2

-- | Sobel X kernel (row-major, 3x3).
--
-- @
-- [ -1  0  1 ]
-- [ -2  0  2 ]
-- [ -1  0  1 ]
-- @
sobelX :: [[Int]]
sobelX =
  [ [-1, 0, 1],
    [-2, 0, 2],
    [-1, 0, 1]
  ]

-- | Sobel Y kernel (row-major, 3x3).
--
-- @
-- [ -1 -2 -1 ]
-- [  0  0  0 ]
-- [  1  2  1 ]
-- @
sobelY :: [[Int]]
sobelY =
  [ [-1, -2, -1],
    [0, 0, 0],
    [1, 2, 1]
  ]

-- | Sobel kernel radius (1 for a 3x3 kernel).
sobelRadius :: Int
sobelRadius = 1

-- ---------------------------------------------------------------------------
-- Blur
-- ---------------------------------------------------------------------------

-- | Separable box blur with the given radius.
--
-- The window size is @2 * radius + 1@. Two passes are applied: horizontal
-- then vertical. Pixels beyond canvas bounds are clamped (edge pixels
-- are repeated). A radius of 0 or less returns the canvas unchanged.
boxBlur :: Int -> Canvas -> Canvas
boxBlur radius canvas
  | radius <= 0 = canvas
  | otherwise =
      let blurredH = blurHorizontal radius canvas
       in blurVertical radius blurredH

-- | Approximate Gaussian blur by applying 'boxBlur' three times.
--
-- Repeated box blur converges to a Gaussian per the central limit theorem.
-- A radius of 0 or less returns the canvas unchanged.
gaussianBlur :: Int -> Canvas -> Canvas
gaussianBlur radius canvas
  | radius <= 0 = canvas
  | otherwise = applyN gaussianPasses (boxBlur radius) canvas

-- ---------------------------------------------------------------------------
-- Sharpen
-- ---------------------------------------------------------------------------

-- | Sharpen using an unsharp mask.
--
-- For each pixel: @result = clamp(2 * original - blurred)@, where the
-- blur uses a small box blur radius. This enhances edges by amplifying
-- the difference between the original and a blurred copy.
sharpen :: Canvas -> Canvas
sharpen canvas =
  let w = cWidth canvas
      h = cHeight canvas
      blurred = boxBlur sharpenBlurRadius canvas
      origSrc = cPixels canvas
      blurSrc = cPixels blurred
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let channel = i `mod` bytesPerPixel
         in if channel == alphaChannel
              then origSrc `unsafeIndex` i
              else
                let origVal = fromIntegral (origSrc `unsafeIndex` i) :: Int
                    blurVal = fromIntegral (blurSrc `unsafeIndex` i) :: Int
                    sharpened = sharpenOriginalWeight * origVal - blurVal
                 in clampByte sharpened
   in Canvas w h pixels

-- ---------------------------------------------------------------------------
-- Edge detection
-- ---------------------------------------------------------------------------

-- | Sobel edge detection.
--
-- Applies Sobel X and Sobel Y convolution kernels, computes the gradient
-- magnitude @sqrt(gx^2 + gy^2)@, and outputs a grayscale image where
-- white indicates edges and black indicates flat regions. Alpha is
-- preserved from the original canvas.
edgeDetect :: Canvas -> Canvas
edgeDetect canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
         in if channel == alphaChannel
              then sampleClampedByte src w h x y alphaChannel
              else
                let gx = applyKernel3x3 sobelX src w h x y channel
                    gy = applyKernel3x3 sobelY src w h x y channel
                    magnitude = sqrt (fromIntegral (gx * gx + gy * gy) :: Double)
                 in clampByte (round magnitude)
   in Canvas w h (grayscaleFromRGB w h pixels)

-- ---------------------------------------------------------------------------
-- Bloom
-- ---------------------------------------------------------------------------

-- | Bloom effect: extract bright pixels, blur them, blend additively.
--
-- @bloom radius threshold canvas@ extracts pixels whose luminance exceeds
-- @threshold@ (a @Double@ in @[0, 1]@ mapped to @[0, 255]@), blurs the
-- bright pixels with the given radius, and additively blends the result
-- back onto the original. The result is clamped to valid color ranges.
bloom :: Int -> Double -> Canvas -> Canvas
bloom radius threshold canvas =
  let w = cWidth canvas
      h = cHeight canvas
      threshByte = round (clampUnit threshold * fromIntegral channelMax) :: Int
      brightCanvas = extractBright threshByte canvas
      blurredBright = gaussianBlur radius brightCanvas
      origSrc = cPixels canvas
      bloomSrc = cPixels blurredBright
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let channel = i `mod` bytesPerPixel
         in if channel == alphaChannel
              then origSrc `unsafeIndex` i
              else
                let origVal = fromIntegral (origSrc `unsafeIndex` i) :: Int
                    bloomVal = fromIntegral (bloomSrc `unsafeIndex` i) :: Int
                 in clampByte (origVal + bloomVal)
   in Canvas w h pixels

-- ---------------------------------------------------------------------------
-- Internal: blur passes
-- ---------------------------------------------------------------------------

-- | Horizontal box blur pass. Each pixel averages a horizontal window
-- of @2 * radius + 1@ pixels, clamping at canvas edges.
blurHorizontal :: Int -> Canvas -> Canvas
blurHorizontal radius canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      windowSize = 2 * radius + 1
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
         in if channel == alphaChannel
              then sampleClampedByte src w h x y alphaChannel
              else
                let !total = sumHorizontal src w h x y radius channel
                 in fromIntegral (total `div` windowSize)
   in Canvas w h pixels

-- | Vertical box blur pass. Each pixel averages a vertical window
-- of @2 * radius + 1@ pixels, clamping at canvas edges.
blurVertical :: Int -> Canvas -> Canvas
blurVertical radius canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      windowSize = 2 * radius + 1
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
         in if channel == alphaChannel
              then sampleClampedByte src w h x y alphaChannel
              else
                let !total = sumVertical src w h x y radius channel
                 in fromIntegral (total `div` windowSize)
   in Canvas w h pixels

-- | Sum channel values across a horizontal window centered at @(x, y)@.
sumHorizontal :: BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Int -> Int
sumHorizontal src w h cx cy radius channel =
  go 0 (cx - radius)
  where
    limit = cx + radius
    go !acc !sx
      | sx > limit = acc
      | otherwise =
          let val = fromIntegral (sampleClampedByte src w h sx cy channel) :: Int
           in go (acc + val) (sx + 1)

-- | Sum channel values across a vertical window centered at @(x, y)@.
sumVertical :: BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Int -> Int
sumVertical src w h cx cy radius channel =
  go 0 (cy - radius)
  where
    limit = cy + radius
    go !acc !sy
      | sy > limit = acc
      | otherwise =
          let val = fromIntegral (sampleClampedByte src w h cx sy channel) :: Int
           in go (acc + val) (sy + 1)

-- ---------------------------------------------------------------------------
-- Internal: Sobel convolution
-- ---------------------------------------------------------------------------

-- | Apply a 3x3 kernel to a single channel at position @(x, y)@.
applyKernel3x3 :: [[Int]] -> BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Int
applyKernel3x3 kernel src w h cx cy channel =
  go 0 (negate sobelRadius) kernel
  where
    go !acc !_ [] = acc
    go !acc !dy (row : rows) =
      let !rowSum = goRow 0 (negate sobelRadius) row
       in go (acc + rowSum) (dy + 1) rows
      where
        goRow !rowAcc !_ [] = rowAcc
        goRow !rowAcc !dx (weight : weights) =
          let sample = fromIntegral (sampleClampedByte src w h (cx + dx) (cy + dy) channel) :: Int
           in goRow (rowAcc + weight * sample) (dx + 1) weights

-- | Convert an RGB image to grayscale by averaging R, G, B per pixel.
-- Alpha is preserved. Uses the already-computed edge magnitude in each
-- RGB channel (which are identical after Sobel), so averaging yields
-- the same value.
grayscaleFromRGB :: Int -> Int -> BS.ByteString -> BS.ByteString
grayscaleFromRGB w h src =
  generatePixelData (w * h * bytesPerPixel) $ \i ->
    let pixIdx = i `div` bytesPerPixel
        channel = i `mod` bytesPerPixel
        baseIdx = pixIdx * bytesPerPixel
     in if channel == alphaChannel
          then src `unsafeIndex` (baseIdx + alphaChannel)
          else
            let rVal = fromIntegral (src `unsafeIndex` baseIdx) :: Int
                gVal = fromIntegral (src `unsafeIndex` (baseIdx + 1)) :: Int
                bVal = fromIntegral (src `unsafeIndex` (baseIdx + 2)) :: Int
                avg = (rVal + gVal + bVal) `div` rgbChannelCount
             in fromIntegral (min channelMax (max 0 avg))

-- ---------------------------------------------------------------------------
-- Internal: bloom helpers
-- ---------------------------------------------------------------------------

-- | Extract pixels above a brightness threshold. Pixels below the
-- threshold are set to black (with original alpha preserved).
extractBright :: Int -> Canvas -> Canvas
extractBright threshByte canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            baseIdx = pixIdx * bytesPerPixel
         in if channel == alphaChannel
              then src `unsafeIndex` (baseIdx + alphaChannel)
              else
                let rVal = fromIntegral (src `unsafeIndex` baseIdx) :: Double
                    gVal = fromIntegral (src `unsafeIndex` (baseIdx + 1)) :: Double
                    bVal = fromIntegral (src `unsafeIndex` (baseIdx + 2)) :: Double
                    lum = round (rVal * luminanceWeightR + gVal * luminanceWeightG + bVal * luminanceWeightB) :: Int
                 in if lum >= threshByte
                      then src `unsafeIndex` (baseIdx + channel)
                      else 0
   in Canvas w h pixels

-- ---------------------------------------------------------------------------
-- Internal: shared helpers
-- ---------------------------------------------------------------------------

-- | Alpha channel index within a pixel's RGBA byte layout.
alphaChannel :: Int
alphaChannel = 3

-- | Number of RGB channels (excluding alpha).
rgbChannelCount :: Int
rgbChannelCount = 3

-- | BT.709 luminance weight for red.
luminanceWeightR :: Double
luminanceWeightR = 0.2126

-- | BT.709 luminance weight for green.
luminanceWeightG :: Double
luminanceWeightG = 0.7152

-- | BT.709 luminance weight for blue.
luminanceWeightB :: Double
luminanceWeightB = 0.0722

-- | Read a single channel byte from the canvas, clamping coordinates
-- to the valid range. Out-of-bounds coordinates are snapped to the
-- nearest edge pixel rather than returning transparent.
sampleClampedByte :: BS.ByteString -> Int -> Int -> Int -> Int -> Int -> Word8
sampleClampedByte src w h x y channel =
  let clampedX = max 0 (min (w - 1) x)
      clampedY = max 0 (min (h - 1) y)
      idx = (clampedY * w + clampedX) * bytesPerPixel + channel
   in src `unsafeIndex` idx

-- | Clamp an integer to the valid byte range @[0, 255]@.
clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min channelMax n))

-- | Clamp a @Double@ to @[0, 1]@.
clampUnit :: Double -> Double
clampUnit x = max 0.0 (min 1.0 x)

-- | Apply a function @n@ times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f = go n
  where
    go !remaining !acc
      | remaining <= 0 = acc
      | otherwise = go (remaining - 1) (f acc)
