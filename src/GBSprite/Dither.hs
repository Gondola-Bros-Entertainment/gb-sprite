-- | Ordered dithering for palette reduction.
--
-- Reduce a full-color canvas to a limited palette using Bayer
-- matrix ordered dithering. Produces the classic retro "cross-hatch"
-- pattern seen in Game Boy and early PC graphics.
module GBSprite.Dither
  ( -- * Types
    DitherMatrix (..),

    -- * Dithering
    orderedDither,
  )
where

import Data.List (foldl')
import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), getPixel, newCanvas, setPixel)
import GBSprite.Color (Color (..))
import GBSprite.Palette (Palette (..))

-- | Available Bayer dither matrix sizes.
data DitherMatrix
  = -- | 2x2 Bayer matrix (coarsest dithering)
    Bayer2
  | -- | 4x4 Bayer matrix (moderate dithering)
    Bayer4
  | -- | 8x8 Bayer matrix (finest dithering)
    Bayer8
  deriving (Show, Eq)

-- | Apply ordered dithering to reduce a canvas to a palette.
--
-- @orderedDither matrix palette canvas@ maps each pixel to the
-- nearest palette color, with a Bayer matrix threshold offset to
-- produce smooth gradients via dithering patterns.
orderedDither :: DitherMatrix -> Palette -> Canvas -> Canvas
orderedDither _ (Palette []) canvas = canvas
orderedDither matrix palette canvas =
  foldCoords w h (newCanvas w h (Color 0 0 0 maxAlpha)) $ \result x y ->
    let pixel = getPixel canvas x y
        threshold = bayerThreshold matrix x y
        adjusted = adjustColor threshold pixel
        closest = findClosest palette adjusted
     in setPixel result x y closest
  where
    w = cWidth canvas
    h = cHeight canvas

-- | Get the Bayer threshold at a pixel position, normalized to [-0.5, 0.5].
bayerThreshold :: DitherMatrix -> Int -> Int -> Double
bayerThreshold Bayer2 x y =
  let idx = (y `mod` bayer2Size) * bayer2Size + (x `mod` bayer2Size)
      val = bayer2 !! idx
   in (fromIntegral val + 0.5) / fromIntegral (bayer2Size * bayer2Size) - 0.5
bayerThreshold Bayer4 x y =
  let idx = (y `mod` bayer4Size) * bayer4Size + (x `mod` bayer4Size)
      val = bayer4 !! idx
   in (fromIntegral val + 0.5) / fromIntegral (bayer4Size * bayer4Size) - 0.5
bayerThreshold Bayer8 x y =
  let idx = (y `mod` bayer8Size) * bayer8Size + (x `mod` bayer8Size)
      val = bayer8 !! idx
   in (fromIntegral val + 0.5) / fromIntegral (bayer8Size * bayer8Size) - 0.5

-- | Adjust a pixel color by the dither threshold.
adjustColor :: Double -> Color -> Color
adjustColor threshold (Color r g b a) =
  Color
    (adjustChannel threshold r)
    (adjustChannel threshold g)
    (adjustChannel threshold b)
    a

-- | Adjust a single channel by the threshold value.
adjustChannel :: Double -> Word8 -> Word8
adjustChannel threshold ch =
  let val = fromIntegral ch + threshold * ditherStrength
   in clampByte (round val)

-- | Find the closest color in the palette (minimum RGB distance).
findClosest :: Palette -> Color -> Color
findClosest (Palette []) color = color
findClosest (Palette (first : rest)) target =
  foldl'
    ( \best candidate ->
        if colorDistance target candidate < colorDistance target best
          then candidate
          else best
    )
    first
    rest

-- | Squared Euclidean distance between two colors in RGB space.
colorDistance :: Color -> Color -> Int
colorDistance (Color r1 g1 b1 _) (Color r2 g2 b2 _) =
  let dr = fromIntegral r1 - fromIntegral r2 :: Int
      dg = fromIntegral g1 - fromIntegral g2 :: Int
      db = fromIntegral b1 - fromIntegral b2 :: Int
   in dr * dr + dg * dg + db * db

-- ---------------------------------------------------------------------------
-- Bayer matrices
-- ---------------------------------------------------------------------------

-- | 2x2 Bayer matrix size.
bayer2Size :: Int
bayer2Size = 2

-- | 2x2 Bayer matrix.
bayer2 :: [Int]
bayer2 = [0, 2, 3, 1]

-- | 4x4 Bayer matrix size.
bayer4Size :: Int
bayer4Size = 4

-- | 4x4 Bayer matrix.
bayer4 :: [Int]
bayer4 =
  [ 0,
    8,
    2,
    10,
    12,
    4,
    14,
    6,
    3,
    11,
    1,
    9,
    15,
    7,
    13,
    5
  ]

-- | 8x8 Bayer matrix size.
bayer8Size :: Int
bayer8Size = 8

-- | 8x8 Bayer matrix.
bayer8 :: [Int]
bayer8 =
  [ 0,
    32,
    8,
    40,
    2,
    34,
    10,
    42,
    48,
    16,
    56,
    24,
    50,
    18,
    58,
    26,
    12,
    44,
    4,
    36,
    14,
    46,
    6,
    38,
    60,
    28,
    52,
    20,
    62,
    30,
    54,
    22,
    3,
    35,
    11,
    43,
    1,
    33,
    9,
    41,
    51,
    19,
    59,
    27,
    49,
    17,
    57,
    25,
    15,
    47,
    7,
    39,
    13,
    45,
    5,
    37,
    63,
    31,
    55,
    23,
    61,
    29,
    53,
    21
  ]

-- | Dither strength (how much the threshold affects the color).
ditherStrength :: Double
ditherStrength = 64.0

-- | Maximum alpha value.
maxAlpha :: Word8
maxAlpha = 255

-- | Clamp an integer to valid byte range.
clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min 255 n))

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
