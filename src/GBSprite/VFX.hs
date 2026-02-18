-- | Procedural visual effect generators.
--
-- Each generator produces a list of 'Canvas' frames representing
-- an animated effect. All generators are pure — no randomness
-- beyond deterministic seeds.
module GBSprite.VFX
  ( -- * Config types
    ExplosionConfig (..),
    RingConfig (..),
    GlowConfig (..),
    TrailConfig (..),
    SparksConfig (..),

    -- * Generators
    explosionFrames,
    ringExpandFrames,
    glowPulseFrames,
    trailFrames,
    flashFrames,
    sparksFrames,
  )
where

import GBSprite.Canvas (Canvas (..), fillCircle, newCanvas)
import GBSprite.Color (Color (..), scaleAlpha, transparent)

-- | Configuration for explosion effects.
data ExplosionConfig = ExplosionConfig
  { -- | Size of each frame (square)
    expSize :: !Int,
    -- | Number of frames
    expFrameCount :: !Int,
    -- | Number of particles
    expParticleCount :: !Int,
    -- | Particle color
    expColor :: !Color,
    -- | Random seed
    expSeed :: !Int
  }
  deriving (Show, Eq)

-- | Configuration for expanding ring effects.
data RingConfig = RingConfig
  { -- | Size of each frame (square)
    ringSize :: !Int,
    -- | Number of frames
    ringFrameCount :: !Int,
    -- | Ring color
    ringColor :: !Color,
    -- | Ring thickness
    ringThickness :: !Int
  }
  deriving (Show, Eq)

-- | Configuration for glow/pulse effects.
data GlowConfig = GlowConfig
  { -- | Size of each frame (square)
    glowSize :: !Int,
    -- | Number of frames per cycle
    glowFrameCount :: !Int,
    -- | Glow color
    glowColor :: !Color
  }
  deriving (Show, Eq)

-- | Configuration for trail effects.
data TrailConfig = TrailConfig
  { -- | Size of each frame
    trailWidth :: !Int,
    -- | Height of each frame
    trailHeight :: !Int,
    -- | Number of frames
    trailFrameCount :: !Int,
    -- | Trail color
    trailColor :: !Color,
    -- | Trail dot radius
    trailDotRadius :: !Int
  }
  deriving (Show, Eq)

-- | Configuration for directional spark burst effects.
data SparksConfig = SparksConfig
  { -- | Size of each frame (square)
    sparksSize :: !Int,
    -- | Number of frames
    sparksFrameCount :: !Int,
    -- | Number of sparks
    sparksCount :: !Int,
    -- | Spark color
    sparksColor :: !Color,
    -- | Random seed
    sparksSeed :: !Int
  }
  deriving (Show, Eq)

-- | Generate explosion frames: radial particle burst.
explosionFrames :: ExplosionConfig -> [Canvas]
explosionFrames config =
  [ renderExplosionFrame config frameIdx
  | frameIdx <- [0 .. expFrameCount config - 1]
  ]

-- | Generate expanding ring frames.
ringExpandFrames :: RingConfig -> [Canvas]
ringExpandFrames config =
  [ renderRingFrame config frameIdx
  | frameIdx <- [0 .. ringFrameCount config - 1]
  ]

-- | Generate glow pulse frames (fading in and out).
glowPulseFrames :: GlowConfig -> [Canvas]
glowPulseFrames config =
  [ renderGlowFrame config frameIdx
  | frameIdx <- [0 .. glowFrameCount config - 1]
  ]

-- | Generate trail frames (dots fading behind).
trailFrames :: TrailConfig -> [Canvas]
trailFrames config =
  [ renderTrailFrame config frameIdx
  | frameIdx <- [0 .. trailFrameCount config - 1]
  ]

-- | Generate flash frames (solid color fading out).
flashFrames :: Int -> Color -> [Canvas]
flashFrames count color =
  [ let progress = fromIntegral i / fromIntegral (max 1 (count - 1)) :: Double
        faded = scaleAlpha (1.0 - progress) color
     in newCanvas flashSize flashSize faded
  | i <- [0 .. count - 1]
  ]
  where
    flashSize :: Int
    flashSize = 32

-- | Generate directional spark burst frames.
sparksFrames :: SparksConfig -> [Canvas]
sparksFrames config =
  [ renderSparksFrame config frameIdx
  | frameIdx <- [0 .. sparksFrameCount config - 1]
  ]

-- ---------------------------------------------------------------------------
-- Internal renderers
-- ---------------------------------------------------------------------------

renderExplosionFrame :: ExplosionConfig -> Int -> Canvas
renderExplosionFrame config frameIdx =
  let size = expSize config
      center = size `div` 2
      progress = fromIntegral frameIdx / fromIntegral (max 1 (expFrameCount config - 1)) :: Double
      maxRadius = fromIntegral center :: Double
      blank = newCanvas size size transparent
   in foldl (drawParticle center progress maxRadius) blank [0 .. expParticleCount config - 1]
  where
    drawParticle center progress maxRadius canvas particleIdx =
      let angle = fromIntegral (particleIdx * lcgStep + expSeed config) * goldenAngle :: Double
          dist = progress * maxRadius * (0.5 + 0.5 * lcgFrac (particleIdx + expSeed config))
          px = center + round (cos angle * dist)
          py = center + round (sin angle * dist)
          alpha = max 0.0 (1.0 - progress)
          particleColor = scaleAlpha alpha (expColor config)
          particleRadius = max 1 (round (fromIntegral (expSize config `div` particleRadiusDivisor) * (1.0 - progress * 0.5)))
       in fillCircle canvas px py particleRadius particleColor

    particleRadiusDivisor :: Int
    particleRadiusDivisor = 16

renderRingFrame :: RingConfig -> Int -> Canvas
renderRingFrame config frameIdx =
  let size = ringSize config
      center = size `div` 2
      progress = fromIntegral frameIdx / fromIntegral (max 1 (ringFrameCount config - 1)) :: Double
      maxRadius = fromIntegral center :: Double
      radius = round (progress * maxRadius)
      alpha = max 0.0 (1.0 - progress)
      ringCol = scaleAlpha alpha (ringColor config)
      blank = newCanvas size size transparent
      thickness = ringThickness config
   in drawRing blank center center radius thickness ringCol

drawRing :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawRing canvas cx cy radius thickness color =
  foldl (\c r -> drawCircleOutline c cx cy r color) canvas [max 0 (radius - thickness `div` 2) .. radius + thickness `div` 2]

drawCircleOutline :: Canvas -> Int -> Int -> Int -> Color -> Canvas
drawCircleOutline canvas cx cy radius color
  | radius <= 0 = canvas
  | otherwise = go canvas 0 radius (1 - radius)
  where
    go c x y d
      | x > y = c
      | otherwise =
          let drawn = plotOctants c cx cy x y color
              nextD = if d < 0 then d + 2 * x + 3 else d + 2 * (x - y) + 5
              nextY = if d < 0 then y else y - 1
           in go drawn (x + 1) nextY nextD

    plotOctants c cx_ cy_ x y col =
      foldl
        (\acc (px, py) -> safeSetPixel acc px py col)
        c
        [ (cx_ + x, cy_ + y),
          (cx_ - x, cy_ + y),
          (cx_ + x, cy_ - y),
          (cx_ - x, cy_ - y),
          (cx_ + y, cy_ + x),
          (cx_ - y, cy_ + x),
          (cx_ + y, cy_ - x),
          (cx_ - y, cy_ - x)
        ]

safeSetPixel :: Canvas -> Int -> Int -> Color -> Canvas
safeSetPixel canvas x y color
  | x >= 0 && x < cWidth_ canvas && y >= 0 && y < cHeight_ canvas =
      fillCircle canvas x y 0 color
  | otherwise = canvas
  where
    cWidth_ (Canvas w _ _) = w
    cHeight_ (Canvas _ h _) = h

renderGlowFrame :: GlowConfig -> Int -> Canvas
renderGlowFrame config frameIdx =
  let size = glowSize config
      center = size `div` 2
      cyclePos = fromIntegral frameIdx / fromIntegral (max 1 (glowFrameCount config)) :: Double
      -- Sine wave for smooth pulse
      intensity = (sin (cyclePos * 2.0 * pi) + 1.0) / 2.0
      radius = round (fromIntegral center * (0.3 + intensity * 0.7))
      glowCol = scaleAlpha intensity (glowColor config)
      blank = newCanvas size size transparent
   in fillCircle blank center center radius glowCol

renderTrailFrame :: TrailConfig -> Int -> Canvas
renderTrailFrame config frameIdx =
  let w = trailWidth config
      h = trailHeight config
      progress = fromIntegral frameIdx / fromIntegral (max 1 (trailFrameCount config - 1)) :: Double
      dotX = round (progress * fromIntegral (w - 1))
      dotY = h `div` 2
      blank = newCanvas w h transparent
      trailDots = trailDotCount
   in foldl (drawTrailDot w dotX dotY progress) blank [0 .. trailDots - 1]
  where
    trailDotCount :: Int
    trailDotCount = 5

    drawTrailDot w headX headY _progress canvas dotIdx =
      let offset = fromIntegral dotIdx * (fromIntegral w * 0.1) :: Double
          dx = headX - round offset
          alpha = max 0.0 (1.0 - fromIntegral dotIdx / fromIntegral trailDotCount)
          dotColor = scaleAlpha alpha (trailColor config)
       in if dx >= 0
            then fillCircle canvas dx headY (trailDotRadius config) dotColor
            else canvas

renderSparksFrame :: SparksConfig -> Int -> Canvas
renderSparksFrame config frameIdx =
  let size = sparksSize config
      center = size `div` 2
      progress = fromIntegral frameIdx / fromIntegral (max 1 (sparksFrameCount config - 1)) :: Double
      blank = newCanvas size size transparent
   in foldl (drawSpark center progress) blank [0 .. sparksCount config - 1]
  where
    drawSpark center progress canvas sparkIdx =
      let angle = fromIntegral (sparkIdx * lcgStep + sparksSeed config) * goldenAngle :: Double
          speed = 0.5 + lcgFrac (sparkIdx + sparksSeed config * 3)
          dist = progress * fromIntegral center * speed
          px = center + round (cos angle * dist)
          py = center + round (sin angle * dist)
          alpha = max 0.0 (1.0 - progress)
          sparkColor = scaleAlpha alpha (sparksColor config)
       in fillCircle canvas px py sparkRadius sparkColor

    sparkRadius :: Int
    sparkRadius = 1

-- ---------------------------------------------------------------------------
-- Deterministic pseudo-random helpers
-- ---------------------------------------------------------------------------

-- | Golden angle for even angular distribution.
goldenAngle :: Double
goldenAngle = 2.399963

-- | LCG step for seed variation.
lcgStep :: Int
lcgStep = 7

-- | Deterministic fractional value in [0, 1) from a seed.
lcgFrac :: Int -> Double
lcgFrac seed =
  let hashed = (seed * lcgMul + lcgInc) `mod` lcgMod
   in fromIntegral (abs hashed) / fromIntegral lcgMod
  where
    lcgMul :: Int
    lcgMul = 1103515245

    lcgInc :: Int
    lcgInc = 12345

    lcgMod :: Int
    lcgMod = 2147483648
