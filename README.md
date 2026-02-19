<div align="center">
<h1>gb-sprite</h1>
<p><strong>Procedural 2D Sprite & VFX Generation</strong></p>
<p>Pure Haskell — no image files, no textures, no asset pipeline. Just math.</p>
<p><a href="#overview">Overview</a> · <a href="#architecture">Architecture</a> · <a href="#usage">Usage</a> · <a href="#api">API</a> · <a href="#example">Example</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/gb-sprite/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/gb-sprite/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/gb-sprite.svg)](https://hackage.haskell.org/package/gb-sprite)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)

</p>
</div>

---

## Overview

gb-sprite is a pure Haskell library for procedurally generating 2D sprites, animations, and visual effects. Define pixel art, VFX, and sprite sheets programmatically — render to BMP with zero external image dependencies.

Companion to [gb-synth](https://github.com/Gondola-Bros-Entertainment/gb-synth) (procedural audio).

**Features:**
- Vector-backed RGBA canvas with drawing primitives (lines, circles, polygons, Bezier curves)
- Transforms: flip, rotate, scale, outline, drop shadow
- Alpha-blended compositing and layering
- Sprite sheet packing (shelf bin packing) with metadata
- Built-in 8x8 pixel font (full printable ASCII)
- Procedural VFX generators (explosion, ring, glow, trail, flash, sparks)
- Procedural noise (value noise, fractal Brownian motion)
- Gradients (linear, radial, diagonal)
- Nine-slice UI panel scaling
- Ordered Bayer dithering for palette reduction
- Tilemap rendering from sprite atlas
- BMP export (32-bit BGRA, zero deps beyond bytestring)
- Optional PNG export via JuicyPixels (`juicy-pixels` cabal flag)

**Core dependencies:** `base`, `bytestring`, `vector` — that's it.

---

## Architecture

```
src/GBSprite/
├── Color.hs       RGBA type, named colors, lerp, multiply, alpha blend
├── Canvas.hs      2D pixel grid (Vector.Storable), drawing primitives
├── Draw.hs        Thick lines, polygons, ellipses, arcs, Bezier curves
├── Palette.hs     Indexed palettes (gameboy, NES), palette swap
├── Sprite.hs      Named sprite with origin, frames, bounding box
├── Animation.hs   Loop/Once/PingPong frame sequencing
├── Transform.hs   Flip, rotate, scale, outline, drop shadow
├── Compose.hs     Alpha-blended layering and stamping
├── Sheet.hs       Shelf bin packing into sprite atlas
├── Text.hs        Built-in 8x8 pixel font rendering
├── Tilemap.hs     Tile-based map rendering from atlas
├── VFX.hs         Procedural effects (explosion, ring, glow, trail, etc.)
├── Noise.hs       Value noise, fractal Brownian motion
├── Gradient.hs    Linear, radial, diagonal gradients
├── NineSlice.hs   UI panel scaling with border preservation
├── Dither.hs      Ordered Bayer dithering for palette reduction
├── BMP.hs         32-bit BGRA BMP export
├── Export.hs      BMP export API (always available)
└── Export/PNG.hs  PNG export via JuicyPixels (optional flag)
```

### Pipeline

```
Canvas → Draw/Transform/Compose → Sprite → Animation → Sheet (atlas) → BMP/PNG
                                                  ↗
VFX generators → [Canvas] frames ─────────────────┘
```

---

## Usage

### As a dependency

Add to your `.cabal` file:

```cabal
build-depends: gb-sprite >= 0.2
```

For PNG export support:

```cabal
build-depends: gb-sprite >= 0.2

flags: +juicy-pixels
```

### Generating sprites

```haskell
import GBSprite.Canvas (newCanvas, fillCircle)
import GBSprite.Color (red, transparent)
import GBSprite.BMP (writeBmp)

main :: IO ()
main = do
  let canvas = fillCircle (newCanvas 32 32 transparent) 16 16 12 red
  writeBmp "sprite.bmp" canvas
```

---

## API

### Color

```haskell
data Color = Color { colorR, colorG, colorB, colorA :: !Word8 }

-- Named colors
red, green, blue, white, black, transparent :: Color

-- Math
lerp       :: Double -> Color -> Color -> Color   -- linear interpolation
multiply   :: Color -> Color -> Color              -- component-wise multiply
alphaBlend :: Color -> Color -> Color              -- Porter-Duff "over"
scaleAlpha :: Double -> Color -> Color             -- scale alpha channel
```

### Canvas

```haskell
data Canvas = Canvas { cWidth :: !Int, cHeight :: !Int, cPixels :: !(VS.Vector Word8) }

newCanvas  :: Int -> Int -> Color -> Canvas        -- blank canvas filled with color
setPixel   :: Canvas -> Int -> Int -> Color -> Canvas
getPixel   :: Canvas -> Int -> Int -> Color
drawLine   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas  -- Bresenham
drawRect   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRect   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
fillCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
floodFill  :: Canvas -> Int -> Int -> Color -> Canvas
```

### Draw

```haskell
drawThickLine :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
drawPolygon   :: Canvas -> [(Int, Int)] -> Color -> Canvas
fillPolygon   :: Canvas -> [(Int, Int)] -> Color -> Canvas  -- scanline fill
drawEllipse   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillEllipse   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawBezier    :: Canvas -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Color -> Canvas
drawRoundRect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRoundRect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
```

### Transform

```haskell
flipH, flipV         :: Canvas -> Canvas
rotate90, rotate180, rotate270 :: Canvas -> Canvas
scaleNearest :: Int -> Canvas -> Canvas            -- nearest-neighbor upscale
outline      :: Color -> Canvas -> Canvas          -- outline non-transparent pixels
dropShadow   :: Int -> Int -> Color -> Canvas -> Canvas
```

### Compose

```haskell
stamp      :: Canvas -> Int -> Int -> Canvas -> Canvas  -- direct overwrite
stampAlpha :: Canvas -> Int -> Int -> Canvas -> Canvas   -- alpha blended
overlay    :: Canvas -> Canvas -> Canvas                 -- same-size blend
overlayAt  :: Canvas -> Int -> Int -> Canvas -> Canvas   -- offset blend
```

### Noise

Deterministic procedural noise for textures and terrain:

```haskell
valueNoise      :: Int -> Int -> Int -> Double -> Canvas              -- width, height, seed, scale
valueNoiseColor :: Int -> Int -> Int -> Double -> Color -> Color -> Canvas  -- noise between two colors
fbm             :: Int -> Int -> Int -> Int -> Double -> Canvas       -- fractal Brownian motion
```

### Gradient

```haskell
linearGradient   :: Int -> Int -> Color -> Color -> Bool -> Canvas  -- horizontal or vertical
radialGradient   :: Int -> Int -> Int -> Int -> Int -> Color -> Color -> Canvas  -- center, radius
diagonalGradient :: Int -> Int -> Color -> Color -> Canvas          -- top-left to bottom-right
```

### NineSlice

Scale UI panels while preserving borders:

```haskell
data NineSlice = NineSlice
  { nsCanvas :: !Canvas, nsLeft :: !Int, nsRight :: !Int, nsTop :: !Int, nsBottom :: !Int }

nineSlice       :: Canvas -> Int -> Int -> Int -> Int -> NineSlice  -- define border regions
renderNineSlice :: NineSlice -> Int -> Int -> Canvas                -- render at target size
```

### Dither

Ordered dithering for retro palette reduction:

```haskell
data DitherMatrix = Bayer2 | Bayer4 | Bayer8

orderedDither :: DitherMatrix -> Palette -> Canvas -> Canvas  -- reduce to palette with dithering
```

### Sheet

```haskell
data SpriteSheet = SpriteSheet { sheetCanvas :: !Canvas, sheetEntries :: ![SheetEntry] }
data SheetEntry  = SheetEntry  { entryName :: !String, entryX, entryY, entryWidth, entryHeight :: !Int }

packSheet :: Int -> [(String, Canvas)] -> SpriteSheet   -- shelf bin packing
```

### Text

```haskell
defaultFont :: Font                                     -- built-in 8x8 pixel font
renderText  :: Font -> Color -> String -> Canvas
renderChar  :: Font -> Color -> Char -> Canvas
textWidth   :: Font -> String -> Int
```

### VFX

```haskell
explosionFrames  :: ExplosionConfig -> [Canvas]   -- radial particle burst
ringExpandFrames :: RingConfig -> [Canvas]         -- expanding ring
glowPulseFrames  :: GlowConfig -> [Canvas]        -- pulsing aura
trailFrames      :: TrailConfig -> [Canvas]        -- fading trail
flashFrames      :: Int -> Color -> [Canvas]       -- solid color fade-out
sparksFrames     :: SparksConfig -> [Canvas]       -- directional spark burst
```

### Animation

```haskell
data PlayMode = Loop | Once | PingPong

animationFrame :: PlayMode -> Int -> Int -> Int  -- mode, total frames, tick → frame index
animationDone  :: PlayMode -> Int -> Int -> Bool -- mode, total frames, tick → finished?
```

### BMP / Export

```haskell
encodeBmp :: Canvas -> BS.ByteString               -- pure: canvas → BMP bytes
writeBmp  :: FilePath -> Canvas -> IO ()            -- write BMP file

-- With juicy-pixels flag (GBSprite.Export.PNG):
exportPng :: FilePath -> Canvas -> IO ()            -- write PNG file
```

---

## Example

Generate a sprite sheet with explosion VFX and a dithered background:

```haskell
import GBSprite.Canvas (newCanvas, fillCircle)
import GBSprite.Color (red, yellow, transparent)
import GBSprite.Dither (DitherMatrix (..), orderedDither)
import GBSprite.Gradient (linearGradient)
import GBSprite.Palette (gameboy)
import GBSprite.Sheet (SpriteSheet (..), packSheet)
import GBSprite.VFX (ExplosionConfig (..), explosionFrames)
import GBSprite.BMP (writeBmp)

main :: IO ()
main = do
  -- Dithered background using Game Boy palette
  let gradient = linearGradient 64 64 red yellow True
      background = orderedDither Bayer4 gameboy gradient

  -- Simple character sprite
  let character = fillCircle (newCanvas 16 16 transparent) 8 8 6 red

  -- Generate explosion frames
  let explosion = explosionFrames ExplosionConfig
        { expSize = 32
        , expFrameCount = 8
        , expParticleCount = 20
        , expColor = yellow
        , expSeed = 42
        }

  -- Pack everything into a sprite sheet
  let items = ("background", background)
            : ("character", character)
            : zip (map (\i -> "explosion_" ++ show i) [0::Int ..]) explosion
      sheet = packSheet 1 items

  -- Export the atlas
  writeBmp "sprites.bmp" (sheetCanvas sheet)
```

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.6.

```bash
cabal build                              # Build library
cabal test                               # Run all tests (105 pure tests)
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal haddock                            # Generate docs
```

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
