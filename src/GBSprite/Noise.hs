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

    -- * Perlin noise
    perlinNoise,

    -- * Worley noise
    worleyNoise,

    -- * Turbulence
    turbulence,
  )
where

import Data.Word (Word8)
import GBSprite.Canvas (Canvas (..), generatePixelData)
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
  let pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            noiseVal = sampleNoise seed scale x y
            Color r g b a = lerp noiseVal startColor endColor
         in colorChannel channel r g b a
   in Canvas w h pixels

-- | Generate fractal Brownian motion noise.
--
-- @fbm width height seed octaves scale@ layers multiple octaves of
-- value noise with decreasing amplitude and increasing frequency.
-- More octaves produce more detail (2–6 is typical).
fbm :: Int -> Int -> Int -> Int -> Double -> Canvas
fbm w h seed octaves scale =
  let clampedOctaves = max 1 octaves
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            noiseVal = fbmSample seed clampedOctaves scale x y
            gray = clampByte (round (noiseVal * channelMaxF))
         in colorChannel channel gray gray gray maxAlpha
   in Canvas w h pixels

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
clampByte n = fromIntegral (max 0 (min channelMaxI n))

channelMaxI :: Int
channelMaxI = 255

-- ---------------------------------------------------------------------------
-- Perlin noise
-- ---------------------------------------------------------------------------

-- | Generate Perlin-style gradient noise.
--
-- Uses gradient vectors at lattice points and dot-product interpolation
-- for smoother, more organic results than value noise.
perlinNoise :: Int -> Int -> Int -> Double -> Canvas
perlinNoise w h seed scale =
  let pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            noiseVal = samplePerlin seed scale x y
            gray = clampByte (round (noiseVal * channelMaxF))
         in colorChannel channel gray gray gray maxAlpha
   in Canvas w h pixels

samplePerlin :: Int -> Double -> Int -> Int -> Double
samplePerlin seed scale x y =
  let fx = fromIntegral x / max 1.0 scale
      fy = fromIntegral y / max 1.0 scale
      ix = floor fx :: Int
      iy = floor fy :: Int
      fracX = fx - fromIntegral ix
      fracY = fy - fromIntegral iy
      smoothX = smoothstep fracX
      smoothY = smoothstep fracY
      -- Gradient dot products at four corners
      g00 = gradDot seed ix iy fracX fracY
      g10 = gradDot seed (ix + 1) iy (fracX - 1.0) fracY
      g01 = gradDot seed ix (iy + 1) fracX (fracY - 1.0)
      g11 = gradDot seed (ix + 1) (iy + 1) (fracX - 1.0) (fracY - 1.0)
      top = lerpD smoothX g00 g10
      bot = lerpD smoothX g01 g11
      raw = lerpD smoothY top bot
   in (raw + 1.0) / 2.0 -- Normalize from [-1,1] to [0,1]

gradDot :: Int -> Int -> Int -> Double -> Double -> Double
gradDot seed gx gy dx dy =
  let hash = lcgHash (seed + gx * latticeHashX + gy * latticeHashY)
      gradIdx = hash `mod` gradientCount
      (gradX, gradY) = gradientVector gradIdx
   in gradX * dx + gradY * dy

gradientCount :: Int
gradientCount = 8

gradientVector :: Int -> (Double, Double)
gradientVector 0 = (1.0, 0.0)
gradientVector 1 = (0.707, 0.707)
gradientVector 2 = (0.0, 1.0)
gradientVector 3 = (-0.707, 0.707)
gradientVector 4 = (-1.0, 0.0)
gradientVector 5 = (-0.707, -0.707)
gradientVector 6 = (0.0, -1.0)
gradientVector _ = (0.707, -0.707)

-- ---------------------------------------------------------------------------
-- Worley noise (cellular)
-- ---------------------------------------------------------------------------

-- | Generate Worley (cellular / Voronoi) noise.
--
-- @worleyNoise width height seed pointCount scale@ scatters
-- @pointCount@ feature points per cell and computes distance-based
-- intensity. Produces organic cell patterns.
worleyNoise :: Int -> Int -> Int -> Int -> Double -> Canvas
worleyNoise w h seed pointCount scale =
  let clampedPoints = max 1 pointCount
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            noiseVal = sampleWorley seed clampedPoints scale x y
            gray = clampByte (round (noiseVal * channelMaxF))
         in colorChannel channel gray gray gray maxAlpha
   in Canvas w h pixels

sampleWorley :: Int -> Int -> Double -> Int -> Int -> Double
sampleWorley seed pointCount scale x y =
  let fx = fromIntegral x / max 1.0 scale
      fy = fromIntegral y / max 1.0 scale
      cellX = floor fx :: Int
      cellY = floor fy :: Int
      minDist = foldNeighborCells seed pointCount fx fy cellX cellY
   in min 1.0 minDist

foldNeighborCells :: Int -> Int -> Double -> Double -> Int -> Int -> Double
foldNeighborCells seed pointCount fx fy cellX cellY =
  let offsets = [negate 1, 0, 1]
      cells = [(cellX + ox, cellY + oy) | ox <- offsets, oy <- offsets]
   in foldlStrict
        (\acc (cx, cy) -> foldCellPoints seed pointCount fx fy cx cy acc)
        worleyMaxDist
        cells

foldCellPoints :: Int -> Int -> Double -> Double -> Int -> Int -> Double -> Double
foldCellPoints seed pointCount fx fy cx cy acc =
  foldlStrict
    ( \best pidx ->
        let cellSeed = seed + cx * latticeHashX + cy * latticeHashY + pidx * octaveSeedOffset
            px = fromIntegral cx + latticeValue cellSeed cx (cy + pidx)
            py = fromIntegral cy + latticeValue (cellSeed + worleyPointOffset) (cx + pidx) cy
            dx = fx - px
            dy = fy - py
            dist = sqrt (dx * dx + dy * dy)
         in min best dist
    )
    acc
    [0 .. pointCount - 1]

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict _ !z [] = z
foldlStrict f !z (x : xs) = foldlStrict f (f z x) xs

worleyMaxDist :: Double
worleyMaxDist = 999.0

worleyPointOffset :: Int
worleyPointOffset = 7919

-- ---------------------------------------------------------------------------
-- Turbulence
-- ---------------------------------------------------------------------------

-- | Generate turbulence noise (absolute-value FBM).
--
-- @turbulence width height seed octaves scale@ sums
-- @abs(noise)@ at multiple octaves for fire, cloud, and water textures.
turbulence :: Int -> Int -> Int -> Int -> Double -> Canvas
turbulence w h seed octaves scale =
  let clampedOctaves = max 1 octaves
      pixels = generatePixelData (w * h * bytesPerPixel) $ \i ->
        let pixIdx = i `div` bytesPerPixel
            channel = i `mod` bytesPerPixel
            x = pixIdx `mod` w
            y = pixIdx `div` w
            noiseVal = turbulenceSample seed clampedOctaves scale x y
            gray = clampByte (round (noiseVal * channelMaxF))
         in colorChannel channel gray gray gray maxAlpha
   in Canvas w h pixels

turbulenceSample :: Int -> Int -> Double -> Int -> Int -> Double
turbulenceSample seed octaves scale x y =
  let go acc amp freq oct
        | oct >= octaves = acc / totalAmp
        | otherwise =
            let val = samplePerlin (seed + oct * octaveSeedOffset) (scale / freq) x y
                absVal = abs (val * 2.0 - 1.0)
             in go (acc + absVal * amp) (amp * persistence) (freq * lacunarity) (oct + 1)
      totalAmp = (1.0 - persistence ** fromIntegral octaves) / (1.0 - persistence)
   in go 0.0 1.0 1.0 0
