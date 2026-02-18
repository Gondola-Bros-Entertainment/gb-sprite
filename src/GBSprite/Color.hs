-- | RGBA color type and color math.
--
-- All colors are 8-bit per channel RGBA. Arithmetic operations
-- (lerp, multiply, alpha blend) work in @[0, 255]@ integer space
-- to match the canvas pixel format.
module GBSprite.Color
  ( -- * Types
    Color (..),

    -- * Named colors
    transparent,
    black,
    white,
    red,
    green,
    blue,
    yellow,
    cyan,
    magenta,
    orange,
    purple,
    pink,
    gray,
    darkGray,
    lightGray,

    -- * Color math
    lerp,
    multiply,
    alphaBlend,
    withAlpha,
    scaleAlpha,
  )
where

import Data.Word (Word8)

-- | An RGBA color with 8 bits per channel.
data Color = Color
  { colorR :: !Word8,
    colorG :: !Word8,
    colorB :: !Word8,
    colorA :: !Word8
  }
  deriving (Show, Eq, Ord)

-- ---------------------------------------------------------------------------
-- Named colors
-- ---------------------------------------------------------------------------

-- | Fully transparent (alpha = 0).
transparent :: Color
transparent = Color 0 0 0 0

-- | Opaque black.
black :: Color
black = Color 0 0 0 maxAlpha

-- | Opaque white.
white :: Color
white = Color maxChannel maxChannel maxChannel maxAlpha

-- | Opaque red.
red :: Color
red = Color maxChannel 0 0 maxAlpha

-- | Opaque green.
green :: Color
green = Color 0 maxChannel 0 maxAlpha

-- | Opaque blue.
blue :: Color
blue = Color 0 0 maxChannel maxAlpha

-- | Opaque yellow.
yellow :: Color
yellow = Color maxChannel maxChannel 0 maxAlpha

-- | Opaque cyan.
cyan :: Color
cyan = Color 0 maxChannel maxChannel maxAlpha

-- | Opaque magenta.
magenta :: Color
magenta = Color maxChannel 0 maxChannel maxAlpha

-- | Opaque orange.
orange :: Color
orange = Color maxChannel 165 0 maxAlpha

-- | Opaque purple.
purple :: Color
purple = Color 128 0 128 maxAlpha

-- | Opaque pink.
pink :: Color
pink = Color maxChannel 192 203 maxAlpha

-- | Opaque 50% gray.
gray :: Color
gray = Color 128 128 128 maxAlpha

-- | Opaque 25% gray.
darkGray :: Color
darkGray = Color 64 64 64 maxAlpha

-- | Opaque 75% gray.
lightGray :: Color
lightGray = Color 192 192 192 maxAlpha

-- ---------------------------------------------------------------------------
-- Color math
-- ---------------------------------------------------------------------------

-- | Linear interpolation between two colors.
--
-- @lerp t a b@ blends from @a@ (at @t=0.0@) to @b@ (at @t=1.0@).
-- The parameter @t@ is clamped to @[0, 1]@.
lerp :: Double -> Color -> Color -> Color
lerp t (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  let tc = clampUnit t
   in Color
        (lerpChannel tc r1 r2)
        (lerpChannel tc g1 g2)
        (lerpChannel tc b1 b2)
        (lerpChannel tc a1 a2)

-- | Component-wise multiply (modulate).
--
-- Each channel is multiplied and divided by 255, so
-- @multiply white c == c@ and @multiply black c == black@.
multiply :: Color -> Color -> Color
multiply (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  Color
    (mulChannel r1 r2)
    (mulChannel g1 g2)
    (mulChannel b1 b2)
    (mulChannel a1 a2)

-- | Alpha-blend @src@ over @dst@ using standard Porter-Duff "over".
--
-- @alphaBlend src dst@ composites @src@ on top of @dst@.
-- When @src@ is fully opaque, result is @src@.
-- When @src@ is fully transparent, result is @dst@.
alphaBlend :: Color -> Color -> Color
alphaBlend (Color sr sg sb sa) (Color dr dg db da)
  | sa == maxAlpha = Color sr sg sb sa
  | sa == 0 = Color dr dg db da
  | otherwise =
      let srcA = fromIntegral sa :: Int
          dstA = fromIntegral da :: Int
          invSrcA = channelMax - srcA
          outA = srcA + (dstA * invSrcA `div` channelMax)
       in if outA == 0
            then transparent
            else
              Color
                (blendChannel srcA invSrcA outA sr dr)
                (blendChannel srcA invSrcA outA sg dg)
                (blendChannel srcA invSrcA outA sb db)
                (fromIntegral outA)

-- | Set the alpha channel of a color, preserving RGB.
withAlpha :: Word8 -> Color -> Color
withAlpha a (Color r g b _) = Color r g b a

-- | Scale the alpha channel by a factor in @[0, 1]@.
scaleAlpha :: Double -> Color -> Color
scaleAlpha factor (Color r g b a) =
  let scaled = round (fromIntegral a * clampUnit factor) :: Int
   in Color r g b (fromIntegral (min channelMax (max 0 scaled)))

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

channelMax :: Int
channelMax = 255

maxAlpha :: Word8
maxAlpha = 255

maxChannel :: Word8
maxChannel = 255

clampUnit :: Double -> Double
clampUnit x = max 0.0 (min 1.0 x)

lerpChannel :: Double -> Word8 -> Word8 -> Word8
lerpChannel t a b =
  let fa = fromIntegral a :: Double
      fb = fromIntegral b :: Double
      result = fa + t * (fb - fa)
   in round (max 0.0 (min 255.0 result))

mulChannel :: Word8 -> Word8 -> Word8
mulChannel a b =
  let ia = fromIntegral a :: Int
      ib = fromIntegral b :: Int
   in fromIntegral (ia * ib `div` channelMax)

blendChannel :: Int -> Int -> Int -> Word8 -> Word8 -> Word8
blendChannel srcA invSrcA outA s d =
  let is = fromIntegral s :: Int
      id_ = fromIntegral d :: Int
      result = (is * srcA + id_ * invSrcA) `div` outA
   in fromIntegral (min channelMax (max 0 result))
