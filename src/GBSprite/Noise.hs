-- | Procedural noise generation.
--
-- Deterministic value noise and fractal Brownian motion for textures,
-- terrain, and procedural backgrounds. Uses an LCG for reproducible
-- output from a seed.
module GBSprite.Noise
  ( -- * Value noise
    valueNoise,
    valueNoiseColor,

    -- * Fractal noise
    fbm,
  )
where

import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), newCanvas, setPixel)
import GBSprite.Color (Color (..), lerp)

-- | Generate a grayscale value noise canvas.
--
-- @valueNoise width height seed scale@ produces a canvas of smoothly
-- varying grayscale values. The @scale@ parameter controls the feature
-- size (larger = smoother).
valueNoise :: Int -> Int -> Int -> Double -> Canvas
valueNoise w h seed scale =
  valueNoiseColor w h seed scale black white
  where
    black :: Color
    black = Color 0 0 0 maxAlpha
    white :: Color
    white = Color 255 255 255 maxAlpha

-- | Generate value noise between two colors.
--
-- Like 'valueNoise', but interpolates between @colorA@ and @colorB@
-- instead of black and white.
valueNoiseColor :: Int -> Int -> Int -> Double -> Color -> Color -> Canvas
valueNoiseColor w h seed scale startColor endColor =
  foldCoords w h (newCanvas w h startColor) $ \canvas x y ->
    let noiseVal = sampleNoise seed scale x y
        color = lerp noiseVal startColor endColor
     in setPixel canvas x y color

-- | Generate fractal Brownian motion noise.
--
-- @fbm width height seed octaves scale@ layers multiple octaves of
-- value noise with decreasing amplitude and increasing frequency.
-- More octaves produce more detail (2–6 is typical).
fbm :: Int -> Int -> Int -> Int -> Double -> Canvas
fbm w h seed octaves scale =
  foldCoords w h (newCanvas w h black) $ \canvas x y ->
    let noiseVal = fbmSample seed (max 1 octaves) scale x y
        gray = clampByte (round (noiseVal * channelMaxF))
        color = Color gray gray gray maxAlpha
     in setPixel canvas x y color
  where
    black :: Color
    black = Color 0 0 0 maxAlpha

-- ---------------------------------------------------------------------------
-- Noise sampling
-- ---------------------------------------------------------------------------

-- | Sample value noise at a point using bilinear interpolation of
-- lattice values.
sampleNoise :: Int -> Double -> Int -> Int -> Double
sampleNoise seed scale x y =
  let fx = fromIntegral x / max 1.0 scale
      fy = fromIntegral y / max 1.0 scale
      ix = floor fx :: Int
      iy = floor fy :: Int
      fracX = fx - fromIntegral ix
      fracY = fy - fromIntegral iy
      smoothX = smoothstep fracX
      smoothY = smoothstep fracY
      v00 = latticeValue seed ix iy
      v10 = latticeValue seed (ix + 1) iy
      v01 = latticeValue seed ix (iy + 1)
      v11 = latticeValue seed (ix + 1) (iy + 1)
      top = lerpD smoothX v00 v10
      bot = lerpD smoothX v01 v11
   in lerpD smoothY top bot

-- | FBM: sum multiple octaves of noise with decreasing amplitude.
fbmSample :: Int -> Int -> Double -> Int -> Int -> Double
fbmSample seed octaves scale x y =
  let go acc amp freq oct
        | oct >= octaves = acc / totalAmp
        | otherwise =
            let val = sampleNoise (seed + oct * octaveSeedOffset) (scale / freq) x y
             in go (acc + val * amp) (amp * persistence) (freq * lacunarity) (oct + 1)
      totalAmp = (1.0 - persistence ** fromIntegral octaves) / (1.0 - persistence)
   in go 0.0 1.0 1.0 0

-- | Smoothstep interpolation (cubic Hermite).
smoothstep :: Double -> Double
smoothstep t = t * t * (3.0 - 2.0 * t)

-- | Linear interpolation for doubles.
lerpD :: Double -> Double -> Double -> Double
lerpD t a b = a + t * (b - a)

-- | Deterministic lattice value from coordinates and seed.
latticeValue :: Int -> Int -> Int -> Double
latticeValue seed x y =
  let hash = lcgHash (seed + x * latticeHashX + y * latticeHashY)
   in fromIntegral hash / lcgMaxF

-- | LCG hash function for deterministic pseudo-random values.
lcgHash :: Int -> Int
lcgHash s =
  let step v = (v * lcgMultiplier + lcgIncrement) `mod` lcgModulus
   in step (step (step s))

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

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Maximum alpha value.
maxAlpha :: Word8
maxAlpha = 255

-- | Maximum channel value as Double.
channelMaxF :: Double
channelMaxF = 255.0

-- | LCG multiplier.
lcgMultiplier :: Int
lcgMultiplier = 1103515245

-- | LCG increment.
lcgIncrement :: Int
lcgIncrement = 12345

-- | LCG modulus.
lcgModulus :: Int
lcgModulus = 2147483648

-- | LCG max value as Double (for normalization).
lcgMaxF :: Double
lcgMaxF = 2147483647.0

-- | Hash multiplier for X coordinate.
latticeHashX :: Int
latticeHashX = 374761393

-- | Hash multiplier for Y coordinate.
latticeHashY :: Int
latticeHashY = 668265263

-- | Seed offset between FBM octaves.
octaveSeedOffset :: Int
octaveSeedOffset = 1337

-- | FBM persistence (amplitude falloff per octave).
persistence :: Double
persistence = 0.5

-- | FBM lacunarity (frequency multiplier per octave).
lacunarity :: Double
lacunarity = 2.0

-- | Clamp an integer to valid byte range.
clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min 255 n))
