-- | 2D pixel grid backed by 'Data.Vector.Storable'.
--
-- A 'Canvas' is a mutable-width x height RGBA pixel buffer. All drawing
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

    -- * Utilities
    clearCanvas,
    pixelIndex,
    inBounds,
  )
where

import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GBSprite.Color (Color (..), transparent)

-- | A 2D RGBA pixel grid.
--
-- Pixels are stored row-major, 4 bytes (RGBA) per pixel.
-- Total size is @cWidth * cHeight * 4@ bytes.
data Canvas = Canvas
  { cWidth :: !Int,
    cHeight :: !Int,
    cPixels :: !(VS.Vector Word8)
  }
  deriving (Show, Eq)

-- | Number of bytes per pixel (RGBA).
bytesPerPixel :: Int
bytesPerPixel = 4

-- | Create a canvas filled with a solid color.
newCanvas :: Int -> Int -> Color -> Canvas
newCanvas w h color =
  Canvas
    { cWidth = w,
      cHeight = h,
      cPixels = VS.generate (w * h * bytesPerPixel) (pixelByte color w)
    }

-- | Create a canvas from a flat list of colors (row-major).
fromPixels :: Int -> Int -> [Color] -> Canvas
fromPixels w h colors =
  Canvas
    { cWidth = w,
      cHeight = h,
      cPixels = VS.fromList (concatMap colorToBytes (take (w * h) padded))
    }
  where
    padded = colors ++ repeat transparent

-- | Byte index of a pixel at @(x, y)@.
pixelIndex :: Int -> Int -> Int -> Int
pixelIndex w x y = (y * w + x) * bytesPerPixel

-- | Check if coordinates are within canvas bounds.
inBounds :: Canvas -> Int -> Int -> Bool
inBounds (Canvas w h _) x y = x >= 0 && x < w && y >= 0 && y < h

-- | Read the color at @(x, y)@. Returns 'transparent' for out-of-bounds.
getPixel :: Canvas -> Int -> Int -> Color
getPixel canvas x y
  | inBounds canvas x y =
      let idx = pixelIndex (cWidth canvas) x y
          px = cPixels canvas
       in Color
            (px VS.! idx)
            (px VS.! (idx + 1))
            (px VS.! (idx + 2))
            (px VS.! (idx + 3))
  | otherwise = transparent

-- | Set the color at @(x, y)@. No-op for out-of-bounds.
setPixel :: Canvas -> Int -> Int -> Color -> Canvas
setPixel canvas x y color
  | inBounds canvas x y =
      let idx = pixelIndex (cWidth canvas) x y
       in canvas
            { cPixels =
                cPixels canvas
                  VS.// [ (idx, colorR color),
                          (idx + 1, colorG color),
                          (idx + 2, colorB color),
                          (idx + 3, colorA color)
                        ]
            }
  | otherwise = canvas

-- | Draw a line from @(x0, y0)@ to @(x1, y1)@ using Bresenham's algorithm.
drawLine :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawLine canvas x0 y0 x1 y1 color =
  let dx = abs (x1 - x0)
      dy = negate (abs (y1 - y0))
      sx = if x0 < x1 then 1 else -1
      sy = if y0 < y1 then 1 else -1
      initialErr = dx + dy
   in bresenham canvas x0 y0 dx dy sx sy initialErr
  where
    bresenham c cx cy dx dy sx sy err
      | cx == x1 && cy == y1 = setPixel c cx cy color
      | otherwise =
          let drawn = setPixel c cx cy color
              e2 = err * 2
              (nextErr1, nextX) =
                if e2 >= dy
                  then (err + dy, cx + sx)
                  else (err, cx)
              (nextErr2, nextY) =
                if e2 <= dx
                  then (nextErr1 + dx, cy + sy)
                  else (nextErr1, cy)
           in bresenham drawn nextX nextY dx dy sx sy nextErr2

-- | Draw a rectangle outline.
drawRect :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawRect canvas x y w h color
  | w <= 0 || h <= 0 = canvas
  | otherwise =
      let x2 = x + w - 1
          y2 = y + h - 1
          top = drawLine canvas x y x2 y color
          bottom = drawLine top x y2 x2 y2 color
          left = drawLine bottom x y x y2 color
          right = drawLine left x2 y x2 y2 color
       in right

-- | Draw a filled rectangle.
fillRect :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRect canvas x y w h color
  | w <= 0 || h <= 0 = canvas
  | otherwise = foldlRows canvas [y .. y + h - 1]
  where
    foldlRows c [] = c
    foldlRows c (row : rows) =
      let filled = foldlCols c row [x .. x + w - 1]
       in foldlRows filled rows

    foldlCols c _ [] = c
    foldlCols c row (col : cols) =
      let drawn = setPixel c col row color
       in foldlCols drawn row cols

-- | Draw a circle outline using the midpoint circle algorithm.
drawCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
drawCircle canvas cx cy radius color
  | radius <= 0 = setPixel canvas cx cy color
  | otherwise = midpointCircle canvas cx cy radius color False

-- | Draw a filled circle.
fillCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
fillCircle canvas cx cy radius color
  | radius <= 0 = setPixel canvas cx cy color
  | otherwise = midpointCircle canvas cx cy radius color True

-- | Midpoint circle algorithm (Bresenham variant).
midpointCircle :: Canvas -> Int -> Int -> Int -> Color -> Bool -> Canvas
midpointCircle canvas cx cy radius color filled =
  go canvas 0 radius (1 - radius)
  where
    go c x y d
      | x > y = c
      | otherwise =
          let drawn =
                if filled
                  then fillCirclePoints c cx cy x y color
                  else drawCirclePoints c cx cy x y color
              nextD =
                if d < 0
                  then d + 2 * x + 3
                  else d + 2 * (x - y) + 5
              nextY = if d < 0 then y else y - 1
           in go drawn (x + 1) nextY nextD

-- | Plot 8 symmetric circle points (outline).
drawCirclePoints :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawCirclePoints c cx cy x y color =
  setPixel
    ( setPixel
        ( setPixel
            ( setPixel
                ( setPixel
                    ( setPixel
                        ( setPixel
                            (setPixel c (cx + x) (cy + y) color)
                            (cx - x)
                            (cy + y)
                            color
                        )
                        (cx + x)
                        (cy - y)
                        color
                    )
                    (cx - x)
                    (cy - y)
                    color
                )
                (cx + y)
                (cy + x)
                color
            )
            (cx - y)
            (cy + x)
            color
        )
        (cx + y)
        (cy - x)
        color
    )
    (cx - y)
    (cy - x)
    color

-- | Fill horizontal spans for each circle octant pair.
fillCirclePoints :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillCirclePoints c cx cy x y color =
  let c1 = hLine c (cx - x) (cx + x) (cy + y) color
      c2 = hLine c1 (cx - x) (cx + x) (cy - y) color
      c3 = hLine c2 (cx - y) (cx + y) (cy + x) color
      c4 = hLine c3 (cx - y) (cx + y) (cy - x) color
   in c4

-- | Draw a horizontal line from x0 to x1 at the given y.
hLine :: Canvas -> Int -> Int -> Int -> Color -> Canvas
hLine canvas x0 x1 y color = go canvas (min x0 x1)
  where
    maxX = max x0 x1
    go c x
      | x > maxX = c
      | otherwise = go (setPixel c x y color) (x + 1)

-- | Flood fill from @(x, y)@, replacing all connected pixels of the
-- same color with the fill color.
floodFill :: Canvas -> Int -> Int -> Color -> Canvas
floodFill canvas x y fillColor
  | not (inBounds canvas x y) = canvas
  | targetColor == fillColor = canvas
  | otherwise = go canvas [(x, y)]
  where
    targetColor = getPixel canvas x y

    go c [] = c
    go c ((px, py) : rest)
      | not (inBounds c px py) = go c rest
      | getPixel c px py /= targetColor = go c rest
      | otherwise =
          let filled = setPixel c px py fillColor
              neighbors =
                [ (px - 1, py),
                  (px + 1, py),
                  (px, py - 1),
                  (px, py + 1)
                ]
           in go filled (neighbors ++ rest)

-- | Clear a canvas to a solid color.
clearCanvas :: Canvas -> Color -> Canvas
clearCanvas canvas color =
  canvas
    { cPixels = VS.generate (cWidth canvas * cHeight canvas * bytesPerPixel) (pixelByte color (cWidth canvas))
    }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Generate a byte at a flat index for a solid-color canvas.
pixelByte :: Color -> Int -> Int -> Word8
pixelByte (Color r g b a) _w idx =
  case idx `mod` bytesPerPixel of
    0 -> r
    1 -> g
    2 -> b
    _ -> a

-- | Convert a 'Color' to its byte representation.
colorToBytes :: Color -> [Word8]
colorToBytes (Color r g b a) = [r, g, b, a]
