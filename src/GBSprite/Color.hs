-- | RGBA color type, color math, and color space conversions.
--
-- All colors are 8-bit per channel RGBA. Arithmetic operations
-- (lerp, multiply, alpha blend) work in @[0, 255]@ integer space
-- to match the canvas pixel format. HSL and HSV conversions are
-- provided for hue-based operations.
module GBSprite.Color
  ( -- * Types
    Color (..),
    HSL (..),
    HSV (..),

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

    -- * Color space conversions
    toHSL,
    fromHSL,
    toHSV,
    fromHSV,

    -- * Color transforms
    tintColor,
    invertColor,
    grayscaleColor,
    brightenColor,
    contrastColor,
    saturateColor,
    shiftHueColor,
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

-- | Hue-saturation-lightness color representation.
--
-- * @hslH@ — hue in degrees @[0, 360)@
-- * @hslS@ — saturation in @[0, 1]@
-- * @hslL@ — lightness in @[0, 1]@
data HSL = HSL
  { hslH :: !Double,
    hslS :: !Double,
    hslL :: !Double
  }
  deriving (Show, Eq)

-- | Hue-saturation-value color representation.
--
-- * @hsvH@ — hue in degrees @[0, 360)@
-- * @hsvS@ — saturation in @[0, 1]@
-- * @hsvV@ — value (brightness) in @[0, 1]@
data HSV = HSV
  { hsvH :: !Double,
    hsvS :: !Double,
    hsvV :: !Double
  }
  deriving (Show, Eq)

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
                (blendChannel srcA dstA invSrcA outA sr dr)
                (blendChannel srcA dstA invSrcA outA sg dg)
                (blendChannel srcA dstA invSrcA outA sb db)
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
-- Color space conversions
-- ---------------------------------------------------------------------------

-- | Convert an RGBA color to HSL. Alpha is discarded.
toHSL :: Color -> HSL
toHSL (Color r g b _) =
  let rf = fromIntegral r / channelMaxF
      gf = fromIntegral g / channelMaxF
      bf = fromIntegral b / channelMaxF
      cmax = max rf (max gf bf)
      cmin = min rf (min gf bf)
      delta = cmax - cmin
      lightness = (cmax + cmin) / 2.0
      saturation
        | delta < colorEpsilon = 0.0
        | otherwise =
            let denom = 1.0 - abs (2.0 * lightness - 1.0)
             in if denom < colorEpsilon then 0.0 else delta / denom
      hue = computeHue rf gf bf cmax delta
   in HSL hue (clampUnit saturation) (clampUnit lightness)

-- | Convert HSL to an opaque RGBA color.
fromHSL :: HSL -> Color
fromHSL (HSL h s l) =
  let chroma = (1.0 - abs (2.0 * l - 1.0)) * s
      hPrime = wrapHue h / hueSegmentSize
      sector = floor hPrime :: Int
      frac = hPrime - fromIntegral sector
      secondary = chroma * (if even sector then frac else 1.0 - frac)
      m = l - chroma / 2.0
      (rf, gf, bf) = hueComponents (sector `mod` hueSectors) chroma secondary
   in Color
        (clampByte (round ((rf + m) * channelMaxF)))
        (clampByte (round ((gf + m) * channelMaxF)))
        (clampByte (round ((bf + m) * channelMaxF)))
        maxAlpha

-- | Convert an RGBA color to HSV. Alpha is discarded.
toHSV :: Color -> HSV
toHSV (Color r g b _) =
  let rf = fromIntegral r / channelMaxF
      gf = fromIntegral g / channelMaxF
      bf = fromIntegral b / channelMaxF
      cmax = max rf (max gf bf)
      cmin = min rf (min gf bf)
      delta = cmax - cmin
      value = cmax
      saturation
        | cmax < colorEpsilon = 0.0
        | otherwise = delta / cmax
      hue = computeHue rf gf bf cmax delta
   in HSV hue saturation value

-- | Convert HSV to an opaque RGBA color.
fromHSV :: HSV -> Color
fromHSV (HSV h s v) =
  let chroma = v * s
      hPrime = wrapHue h / hueSegmentSize
      sector = floor hPrime :: Int
      frac = hPrime - fromIntegral sector
      secondary = chroma * (if even sector then frac else 1.0 - frac)
      m = v - chroma
      (rf, gf, bf) = hueComponents (sector `mod` hueSectors) chroma secondary
   in Color
        (clampByte (round ((rf + m) * channelMaxF)))
        (clampByte (round ((gf + m) * channelMaxF)))
        (clampByte (round ((bf + m) * channelMaxF)))
        maxAlpha

-- ---------------------------------------------------------------------------
-- Color transforms
-- ---------------------------------------------------------------------------

-- | Tint a color by multiplying RGB channels. The tint's alpha is ignored;
-- the target's alpha is preserved.
tintColor :: Color -> Color -> Color
tintColor (Color tr tg tb _) (Color cr cg cb ca) =
  Color (mulChannel tr cr) (mulChannel tg cg) (mulChannel tb cb) ca

-- | Invert RGB channels (negate). Alpha is preserved.
invertColor :: Color -> Color
invertColor (Color r g b a) =
  Color (maxChannel - r) (maxChannel - g) (maxChannel - b) a

-- | Convert to grayscale using BT.709 luminance weights.
-- Alpha is preserved.
grayscaleColor :: Color -> Color
grayscaleColor (Color r g b a) =
  let lum = luminance r g b
   in Color lum lum lum a

-- | Adjust brightness by lerping towards white (positive) or black
-- (negative). Amount is clamped to @[-1, 1]@.
brightenColor :: Double -> Color -> Color
brightenColor amount (Color r g b a)
  | amount >= 0 =
      let t = clampUnit amount
       in Color (liftChannel t r) (liftChannel t g) (liftChannel t b) a
  | otherwise =
      let t = clampUnit (negate amount)
       in Color (dimChannel t r) (dimChannel t g) (dimChannel t b) a
  where
    liftChannel t ch =
      clampByte (round (fromIntegral ch + t * (channelMaxF - fromIntegral ch)))
    dimChannel t ch =
      clampByte (round (fromIntegral ch * (1.0 - t)))

-- | Adjust contrast by scaling distance from the midpoint.
-- Amount is clamped to @[-1, 1]@: negative reduces, positive increases.
contrastColor :: Double -> Color -> Color
contrastColor amount (Color r g b a) =
  let factor = 1.0 + clampSigned amount
   in Color
        (adjustCh factor r)
        (adjustCh factor g)
        (adjustCh factor b)
        a
  where
    adjustCh f ch =
      clampByte (round (channelMid + (fromIntegral ch - channelMid) * f))

-- | Adjust saturation. Amount in @[-1, 1]@: -1 fully desaturates,
-- 0 unchanged, positive boosts.
saturateColor :: Double -> Color -> Color
saturateColor amount (Color r g b a) =
  let lum = fromIntegral (luminance r g b) :: Double
      factor = 1.0 + clampSigned amount
   in Color
        (clampByte (round (lum + (fromIntegral r - lum) * factor)))
        (clampByte (round (lum + (fromIntegral g - lum) * factor)))
        (clampByte (round (lum + (fromIntegral b - lum) * factor)))
        a

-- | Shift hue by the given number of degrees. Alpha is preserved.
shiftHueColor :: Double -> Color -> Color
shiftHueColor degrees color =
  let HSL h s l = toHSL color
      shifted = HSL (wrapHue (h + degrees)) s l
   in withAlpha (colorA color) (fromHSL shifted)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

channelMax :: Int
channelMax = 255

channelMaxF :: Double
channelMaxF = 255.0

channelMid :: Double
channelMid = 128.0

maxAlpha :: Word8
maxAlpha = 255

maxChannel :: Word8
maxChannel = 255

colorEpsilon :: Double
colorEpsilon = 1.0e-10

fullCircle :: Double
fullCircle = 360.0

hueSegmentSize :: Double
hueSegmentSize = 60.0

hueSectors :: Int
hueSectors = 6

luminanceWeightR :: Double
luminanceWeightR = 0.2126

luminanceWeightG :: Double
luminanceWeightG = 0.7152

luminanceWeightB :: Double
luminanceWeightB = 0.0722

clampUnit :: Double -> Double
clampUnit x = max 0.0 (min 1.0 x)

clampSigned :: Double -> Double
clampSigned x = max (-1.0) (min 1.0 x)

clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min channelMax n))

lerpChannel :: Double -> Word8 -> Word8 -> Word8
lerpChannel t a b =
  let fa = fromIntegral a :: Double
      fb = fromIntegral b :: Double
      result = fa + t * (fb - fa)
   in round (max 0.0 (min channelMaxF result))

mulChannel :: Word8 -> Word8 -> Word8
mulChannel a b =
  let ia = fromIntegral a :: Int
      ib = fromIntegral b :: Int
   in fromIntegral (ia * ib `div` channelMax)

blendChannel :: Int -> Int -> Int -> Int -> Word8 -> Word8 -> Word8
blendChannel srcA dstA invSrcA outA s d =
  let is = fromIntegral s :: Int
      id_ = fromIntegral d :: Int
      dstContrib = id_ * dstA * invSrcA `div` channelMax
      result = (is * srcA + dstContrib) `div` outA
   in fromIntegral (min channelMax (max 0 result))

luminance :: Word8 -> Word8 -> Word8 -> Word8
luminance r g b =
  clampByte
    ( round
        ( fromIntegral r * luminanceWeightR
            + fromIntegral g * luminanceWeightG
            + fromIntegral b * luminanceWeightB
        )
    )

computeHue :: Double -> Double -> Double -> Double -> Double -> Double
computeHue rf gf bf cmax delta
  | delta < colorEpsilon = 0.0
  | abs (cmax - rf) < colorEpsilon =
      wrapHue (hueSegmentSize * (gf - bf) / delta)
  | abs (cmax - gf) < colorEpsilon =
      wrapHue (hueSegmentSize * ((bf - rf) / delta + 2.0))
  | otherwise =
      wrapHue (hueSegmentSize * ((rf - gf) / delta + 4.0))

wrapHue :: Double -> Double
wrapHue h =
  let wrapped = h - fullCircle * fromIntegral (floor (h / fullCircle) :: Int)
   in if wrapped < 0.0 then wrapped + fullCircle else wrapped

hueComponents :: Int -> Double -> Double -> (Double, Double, Double)
hueComponents 0 c x = (c, x, 0)
hueComponents 1 c x = (x, c, 0)
hueComponents 2 c x = (0, c, x)
hueComponents 3 c x = (0, x, c)
hueComponents 4 c x = (x, 0, c)
hueComponents _ c x = (c, 0, x)
