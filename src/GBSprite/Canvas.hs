-- | 2D pixel grid backed by strict 'Data.ByteString.ByteString'.
--
-- A t'Canvas' is a width x height RGBA pixel buffer. All drawing
-- operations are pure @Canvas -> Canvas@ transforms. Coordinates use
-- @(Int, Int)@ with origin at top-left, x increasing right, y increasing
-- down.
module GBSprite.Canvas
  ( -- * Types
    Canvas (..),

    -- * Construction
    newCanvas,
    fromPixels,

    -- * Pixel access
    getPixel,
    setPixel,

    -- * Drawing primitives
    drawLine,
    drawRect,
    fillRect,
    drawCircle,
    fillCircle,
    floodFill,
    hLine,

    -- * Transforms
    mapPixels,
    crop,
    trimTransparent,
    canvasOpacity,

    -- * Folds
    pixelFold,

    -- * Utilities
    clearCanvas,
    pixelIndex,
    inBounds,

    -- * Low-level
    generatePixelData,
    generateCanvasPixels,
    bulkSetPixels,
    bulkHSpans,
    bulkBlendPixels,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (unsafeCreate)
import Data.ByteString.Unsafe (unsafeIndex, unsafeUseAsCStringLen)
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GBSprite.Color (Color (..), alphaBlend, scaleAlpha, transparent)

-- | A 2D RGBA pixel grid.
--
-- Pixels are stored row-major, 4 bytes (RGBA) per pixel.
-- Total size is @cWidth * cHeight * 4@ bytes.
data Canvas = Canvas
  { cWidth :: !Int,
    cHeight :: !Int,
    cPixels :: !BS.ByteString
  }
  deriving (Show, Eq)

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Generate a pixel byte buffer from a flat index function.
--
-- @generatePixelData n f@ creates a 'BS.ByteString' of @n@ bytes where
-- byte @i@ is computed by @f i@. This is the core primitive for
-- single-pass O(n) canvas construction.
generatePixelData :: Int -> (Int -> Word8) -> BS.ByteString
generatePixelData n f = unsafeCreate n $ \ptr ->
  mapM_ (\i -> pokeByteOff ptr i (f i)) [0 .. n - 1]

-- | Build a canvas from a per-pixel color function.
--
-- More efficient than 'generatePixelData' as it computes the color once
-- per pixel and avoids per-byte @div@\/@mod@ overhead.
generateCanvasPixels :: Int -> Int -> (Int -> Int -> Color) -> Canvas
generateCanvasPixels w h pixelAt =
  let safeW = max 1 w
      safeH = max 1 h
   in Canvas safeW safeH $ unsafeCreate (safeW * safeH * bytesPerPixel) $ \ptr ->
        let go !offset !x !y
              | y >= safeH = return ()
              | x >= safeW = go offset 0 (y + 1)
              | otherwise = do
                  let Color r g b a = pixelAt x y
                  pokeByteOff ptr offset r
                  pokeByteOff ptr (offset + 1) g
                  pokeByteOff ptr (offset + 2) b
                  pokeByteOff ptr (offset + 3) a
                  go (offset + bytesPerPixel) (x + 1) y
         in go 0 0 0

-- | Create a canvas filled with a solid color.
-- Dimensions are clamped to a minimum of 1.
newCanvas :: Int -> Int -> Color -> Canvas
newCanvas w h color = generateCanvasPixels w h (\_ _ -> color)

-- | Create a canvas from a flat list of colors (row-major).
-- Dimensions are clamped to a minimum of 1.
fromPixels :: Int -> Int -> [Color] -> Canvas
fromPixels w h colors =
  let safeW = max 1 w
      safeH = max 1 h
      totalPixels = safeW * safeH
   in Canvas safeW safeH $ unsafeCreate (totalPixels * bytesPerPixel) $ \ptr ->
        let go !offset !remaining cs
              | remaining <= 0 = return ()
              | otherwise = case cs of
                  [] -> fillTransparent offset remaining
                  (Color r g b a : rest) -> do
                    pokeByteOff ptr offset r
                    pokeByteOff ptr (offset + 1) g
                    pokeByteOff ptr (offset + 2) b
                    pokeByteOff ptr (offset + 3) a
                    go (offset + bytesPerPixel) (remaining - 1) rest
            fillTransparent !offset !remaining
              | remaining <= 0 = return ()
              | otherwise = do
                  pokeByteOff ptr offset (0 :: Word8)
                  pokeByteOff ptr (offset + 1) (0 :: Word8)
                  pokeByteOff ptr (offset + 2) (0 :: Word8)
                  pokeByteOff ptr (offset + 3) (0 :: Word8)
                  fillTransparent (offset + bytesPerPixel) (remaining - 1)
         in go 0 totalPixels colors

-- | Byte index of a pixel at @(x, y)@.
pixelIndex :: Int -> Int -> Int -> Int
pixelIndex w x y = (y * w + x) * bytesPerPixel

-- | Check if coordinates are within canvas bounds.
inBounds :: Canvas -> Int -> Int -> Bool
inBounds (Canvas w h _) x y = x >= 0 && x < w && y >= 0 && y < h

-- | Read the color at @(x, y)@. Returns @transparent@ for out-of-bounds.
getPixel :: Canvas -> Int -> Int -> Color
getPixel canvas x y
  | inBounds canvas x y =
      let idx = pixelIndex (cWidth canvas) x y
          px = cPixels canvas
       in Color
            (px `unsafeIndex` idx)
            (px `unsafeIndex` (idx + 1))
            (px `unsafeIndex` (idx + 2))
            (px `unsafeIndex` (idx + 3))
  | otherwise = transparent

-- | Set the color at @(x, y)@. No-op for out-of-bounds.
setPixel :: Canvas -> Int -> Int -> Color -> Canvas
setPixel canvas x y color
  | inBounds canvas x y =
      withCopiedPixels canvas $ \ptr _ _ ->
        pokePixelAt ptr (pixelIndex (cWidth canvas) x y) color
  | otherwise = canvas

-- | Draw a line from @(x0, y0)@ to @(x1, y1)@ using Bresenham's algorithm.
drawLine :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawLine canvas x0 y0 x1 y1 color =
  let dx = abs (x1 - x0)
      dy = negate (abs (y1 - y0))
      sx = if x0 < x1 then 1 else -1
      sy = if y0 < y1 then 1 else -1
      initialErr = dx + dy
   in withCopiedPixels canvas $ \ptr w h ->
        let go !cx !cy !err = do
              when (cx >= 0 && cx < w && cy >= 0 && cy < h) $
                pokePixelAt ptr (pixelIndex w cx cy) color
              if cx == x1 && cy == y1
                then return ()
                else do
                  let e2 = err * 2
                      (!nextErr1, !nextX) =
                        if e2 >= dy
                          then (err + dy, cx + sx)
                          else (err, cx)
                      (!nextErr2, !nextY) =
                        if e2 <= dx
                          then (nextErr1 + dx, cy + sy)
                          else (nextErr1, cy)
                  go nextX nextY nextErr2
         in go x0 y0 initialErr

-- | Draw a rectangle outline.
drawRect :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawRect canvas x y w h color
  | w <= 0 || h <= 0 = canvas
  | otherwise =
      let x2 = x + w - 1
          y2 = y + h - 1
       in withCopiedPixels canvas $ \ptr cw ch -> do
            pokeHLine ptr cw ch x x2 y color
            pokeHLine ptr cw ch x x2 y2 color
            pokeVLine ptr cw ch x y y2 color
            pokeVLine ptr cw ch x2 y y2 color

-- | Draw a filled rectangle.
fillRect :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRect canvas x y w h color
  | w <= 0 || h <= 0 = canvas
  | otherwise =
      let cw = cWidth canvas
          ch = cHeight canvas
          minX = max 0 x
          maxX = min (cw - 1) (x + w - 1)
          minY = max 0 y
          maxY = min (ch - 1) (y + h - 1)
       in if minX > maxX || minY > maxY
            then canvas
            else withCopiedPixels canvas $ \ptr canvW _canvH ->
              let goY !row
                    | row > maxY = return ()
                    | otherwise = do
                        let goX !col
                              | col > maxX = return ()
                              | otherwise = do
                                  pokePixelAt ptr (pixelIndex canvW col row) color
                                  goX (col + 1)
                        goX minX
                        goY (row + 1)
               in goY minY

-- | Draw a circle outline using the midpoint circle algorithm.
drawCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
drawCircle canvas cx cy radius color
  | radius <= 0 = setPixel canvas cx cy color
  | otherwise = withCopiedPixels canvas $ \ptr w h ->
      let plotSafe !px !py =
            when (px >= 0 && px < w && py >= 0 && py < h) $
              pokePixelAt ptr (pixelIndex w px py) color
          go !x !y !d
            | x > y = return ()
            | otherwise = do
                plotSafe (cx + x) (cy + y)
                plotSafe (cx - x) (cy + y)
                plotSafe (cx + x) (cy - y)
                plotSafe (cx - x) (cy - y)
                plotSafe (cx + y) (cy + x)
                plotSafe (cx - y) (cy + x)
                plotSafe (cx + y) (cy - x)
                plotSafe (cx - y) (cy - x)
                let nextD =
                      if d < 0
                        then d + 2 * x + 3
                        else d + 2 * (x - y) + 5
                    nextY = if d < 0 then y else y - 1
                go (x + 1) nextY nextD
       in go 0 radius (1 - radius)

-- | Draw a filled circle.
fillCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
fillCircle canvas cx cy radius color
  | radius <= 0 = setPixel canvas cx cy color
  | otherwise = withCopiedPixels canvas $ \ptr w h ->
      let go !x !y !d
            | x > y = return ()
            | otherwise = do
                pokeHLine ptr w h (cx - x) (cx + x) (cy + y) color
                pokeHLine ptr w h (cx - x) (cx + x) (cy - y) color
                pokeHLine ptr w h (cx - y) (cx + y) (cy + x) color
                pokeHLine ptr w h (cx - y) (cx + y) (cy - x) color
                let nextD =
                      if d < 0
                        then d + 2 * x + 3
                        else d + 2 * (x - y) + 5
                    nextY = if d < 0 then y else y - 1
                go (x + 1) nextY nextD
       in go 0 radius (1 - radius)

-- | Draw a horizontal line from x0 to x1 at the given y.
hLine :: Canvas -> Int -> Int -> Int -> Color -> Canvas
hLine canvas x0 x1 y color
  | y < 0 || y >= cHeight canvas = canvas
  | otherwise =
      let w = cWidth canvas
          minX = max 0 (min x0 x1)
          maxX = min (w - 1) (max x0 x1)
       in if minX > maxX
            then canvas
            else withCopiedPixels canvas $ \ptr canvW _canvH ->
              let go !col
                    | col > maxX = return ()
                    | otherwise = do
                        pokePixelAt ptr (pixelIndex canvW col y) color
                        go (col + 1)
               in go minX

-- | Flood fill from @(x, y)@, replacing all connected pixels of the
-- same color with the fill color.
floodFill :: Canvas -> Int -> Int -> Color -> Canvas
floodFill canvas x y fillColor
  | not (inBounds canvas x y) = canvas
  | targetColor == fillColor = canvas
  | otherwise = withCopiedPixels canvas $ \ptr w h ->
      let go [] = return ()
          go ((px, py) : rest)
            | px < 0 || px >= w || py < 0 || py >= h = go rest
            | otherwise = do
                current <- peekPixelAt ptr (pixelIndex w px py)
                if current /= targetColor
                  then go rest
                  else do
                    pokePixelAt ptr (pixelIndex w px py) fillColor
                    go
                      ( (px - 1, py)
                          : (px + 1, py)
                          : (px, py - 1)
                          : (px, py + 1)
                          : rest
                      )
       in go [(x, y)]
  where
    targetColor = getPixel canvas x y

-- ---------------------------------------------------------------------------
-- Transforms
-- ---------------------------------------------------------------------------

-- | Apply a function to every pixel.
mapPixels :: (Color -> Color) -> Canvas -> Canvas
mapPixels f canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      totalPixels = w * h
   in Canvas w h $ unsafeCreate (totalPixels * bytesPerPixel) $ \ptr ->
        let go !pixIdx !offset
              | pixIdx >= totalPixels = return ()
              | otherwise = do
                  let origColor =
                        Color
                          (src `unsafeIndex` offset)
                          (src `unsafeIndex` (offset + 1))
                          (src `unsafeIndex` (offset + 2))
                          (src `unsafeIndex` (offset + 3))
                      Color nr ng nb na = f origColor
                  pokeByteOff ptr offset nr
                  pokeByteOff ptr (offset + 1) ng
                  pokeByteOff ptr (offset + 2) nb
                  pokeByteOff ptr (offset + 3) na
                  go (pixIdx + 1) (offset + bytesPerPixel)
         in go 0 0

-- | Extract a rectangular region. Out-of-bounds pixels are transparent.
crop :: Int -> Int -> Int -> Int -> Canvas -> Canvas
crop cx cy cw ch canvas
  | cw <= 0 || ch <= 0 = newCanvas 1 1 transparent
  | otherwise = generateCanvasPixels cw ch $ \x y ->
      getPixel canvas (x + cx) (y + cy)

-- | Remove transparent borders, cropping to the bounding box of
-- non-transparent pixels. Returns a 1x1 transparent canvas if
-- all pixels are transparent.
trimTransparent :: Canvas -> Canvas
trimTransparent canvas =
  let w = cWidth canvas
      h = cHeight canvas
      (bMinX, bMinY, bMaxX, bMaxY) =
        pixelFold findBounds (w, h, negate 1, negate 1) canvas
   in if bMaxX < 0 || bMaxY < 0
        then newCanvas 1 1 transparent
        else crop bMinX bMinY (bMaxX - bMinX + 1) (bMaxY - bMinY + 1) canvas
  where
    findBounds (!accMinX, !accMinY, !accMaxX, !accMaxY) x y (Color _ _ _ a)
      | a == 0 = (accMinX, accMinY, accMaxX, accMaxY)
      | otherwise = (min accMinX x, min accMinY y, max accMaxX x, max accMaxY y)

-- | Scale the alpha of every pixel by a factor in @[0, 1]@.
canvasOpacity :: Double -> Canvas -> Canvas
canvasOpacity factor = mapPixels (scaleAlpha factor)

-- ---------------------------------------------------------------------------
-- Folds
-- ---------------------------------------------------------------------------

-- | Strict left fold over every pixel with its coordinates.
pixelFold :: (a -> Int -> Int -> Color -> a) -> a -> Canvas -> a
pixelFold f initial canvas =
  let w = cWidth canvas
      h = cHeight canvas
      src = cPixels canvas
      go !acc !offset !x !y
        | y >= h = acc
        | x >= w = go acc offset 0 (y + 1)
        | otherwise =
            let color =
                  Color
                    (src `unsafeIndex` offset)
                    (src `unsafeIndex` (offset + 1))
                    (src `unsafeIndex` (offset + 2))
                    (src `unsafeIndex` (offset + 3))
             in go (f acc x y color) (offset + bytesPerPixel) (x + 1) y
   in go initial 0 0 0

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Clear a canvas to a solid color.
clearCanvas :: Canvas -> Color -> Canvas
clearCanvas canvas = newCanvas (cWidth canvas) (cHeight canvas)

-- ---------------------------------------------------------------------------
-- Bulk operations
-- ---------------------------------------------------------------------------

-- | Write pixels to a canvas in a single buffer copy.
--
-- @bulkSetPixels canvas writes@ copies the pixel buffer once, then
-- overwrites each @(x, y, color)@ entry. Out-of-bounds writes are
-- silently ignored. This is O(n + k) where n is the canvas size and
-- k is the number of writes, versus O(n * k) for repeated 'setPixel'.
bulkSetPixels :: Canvas -> [(Int, Int, Color)] -> Canvas
bulkSetPixels canvas [] = canvas
bulkSetPixels canvas writes = withCopiedPixels canvas $ \ptr w h ->
  mapM_
    ( \(x, y, pixColor) ->
        when (x >= 0 && x < w && y >= 0 && y < h) $
          pokePixelAt ptr (pixelIndex w x y) pixColor
    )
    writes

-- | Fill horizontal spans in a single buffer copy.
--
-- Each span is @(x0, x1, y)@. More efficient than repeated 'hLine'
-- calls for shapes composed of horizontal runs (filled polygons,
-- filled ellipses).
bulkHSpans :: Canvas -> Color -> [(Int, Int, Int)] -> Canvas
bulkHSpans canvas _ [] = canvas
bulkHSpans canvas color spans = withCopiedPixels canvas $ \ptr w h ->
  mapM_
    ( \(x0, x1, y) ->
        when (y >= 0 && y < h) $ do
          let minX = max 0 (min x0 x1)
              maxX = min (w - 1) (max x0 x1)
              goX !col
                | col > maxX = return ()
                | otherwise = do
                    pokePixelAt ptr (pixelIndex w col y) color
                    goX (col + 1)
          goX minX
    )
    spans

-- | Blend pixels onto a canvas using alpha compositing.
--
-- Each @(x, y, color)@ entry is alpha-blended with the existing
-- pixel at that position. Useful for anti-aliased drawing.
bulkBlendPixels :: Canvas -> [(Int, Int, Color)] -> Canvas
bulkBlendPixels canvas [] = canvas
bulkBlendPixels canvas writes = withCopiedPixels canvas $ \ptr w h ->
  mapM_
    ( \(x, y, srcColor) ->
        when (x >= 0 && x < w && y >= 0 && y < h) $ do
          let idx = pixelIndex w x y
          dstColor <- peekPixelAt ptr idx
          pokePixelAt ptr idx (alphaBlend srcColor dstColor)
    )
    writes

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Write a pixel's RGBA bytes at the given byte offset.
pokePixelAt :: Ptr Word8 -> Int -> Color -> IO ()
pokePixelAt ptr offset (Color r g b a) = do
  pokeByteOff ptr offset r
  pokeByteOff ptr (offset + 1) g
  pokeByteOff ptr (offset + 2) b
  pokeByteOff ptr (offset + 3) a

-- | Read a pixel's RGBA bytes from the given byte offset.
peekPixelAt :: Ptr Word8 -> Int -> IO Color
peekPixelAt ptr offset = do
  r <- peekByteOff ptr offset
  g <- peekByteOff ptr (offset + 1)
  b <- peekByteOff ptr (offset + 2)
  a <- peekByteOff ptr (offset + 3)
  return (Color r g b a)

-- | Copy canvas pixel data to a fresh mutable buffer, apply
-- modifications via the IO callback, return the modified canvas.
-- This is the core copy-and-modify primitive that makes drawing
-- operations O(n + k) instead of O(n * k).
withCopiedPixels :: Canvas -> (Ptr Word8 -> Int -> Int -> IO ()) -> Canvas
withCopiedPixels canvas action =
  let w = cWidth canvas
      h = cHeight canvas
      totalBytes = w * h * bytesPerPixel
      src = cPixels canvas
   in canvas
        { cPixels = unsafeCreate totalBytes $ \dstPtr -> do
            unsafeUseAsCStringLen src $ \(srcPtr, _) ->
              copyBytes dstPtr (castPtr srcPtr) totalBytes
            action dstPtr w h
        }

-- | Poke a horizontal line into a mutable pixel buffer.
pokeHLine :: Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> Color -> IO ()
pokeHLine ptr w h x0 x1 y color
  | y < 0 || y >= h = return ()
  | otherwise =
      let minX = max 0 (min x0 x1)
          maxX = min (w - 1) (max x0 x1)
          go !col
            | col > maxX = return ()
            | otherwise = do
                pokePixelAt ptr (pixelIndex w col y) color
                go (col + 1)
       in go minX

-- | Poke a vertical line into a mutable pixel buffer.
pokeVLine :: Ptr Word8 -> Int -> Int -> Int -> Int -> Int -> Color -> IO ()
pokeVLine ptr w h x y0 y1 color
  | x < 0 || x >= w = return ()
  | otherwise =
      let minY = max 0 (min y0 y1)
          maxY = min (h - 1) (max y0 y1)
          go !row
            | row > maxY = return ()
            | otherwise = do
                pokePixelAt ptr (pixelIndex w x row) color
                go (row + 1)
       in go minY
