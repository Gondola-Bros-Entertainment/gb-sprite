<div align="center">
<h1>gb-sprite</h1>
<p><strong>Pure Haskell 2D Raster Graphics Engine</strong></p>
<p>Pure Haskell — no image files, no textures, no asset pipeline. Just math.</p>
<p><a href="#overview">Overview</a> · <a href="#architecture">Architecture</a> · <a href="#usage">Usage</a> · <a href="#api">API</a> · <a href="#example">Example</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/gb-sprite/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/gb-sprite/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/gb-sprite.svg)](https://hackage.haskell.org/package/gb-sprite)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)

</p>
</div>

---

## Overview

gb-sprite is a pure Haskell 2D raster graphics engine for procedurally generating sprites, animations, and visual effects. Draw primitives, compose layers, apply filters and noise, render isometric tilemaps, pack sprite sheets, and grade colors — all without external image libraries. Export to BMP and PNG natively.

Companion to [gb-synth](https://github.com/Gondola-Bros-Entertainment/gb-synth) (procedural audio) and [gb-vector](https://github.com/Gondola-Bros-Entertainment/gb-vector) (SVG generation).

**Features:**
- ByteString-backed RGBA canvas with drawing primitives (lines, circles, polygons, Bezier curves, Catmull-Rom splines, anti-aliased lines)
- Transforms: flip, rotate (90/180/270/arbitrary), scale (nearest/bilinear), shear, outline, drop shadow
- Alpha-blended compositing with blend modes (multiply, screen, overlay, additive, soft light, difference)
- Masking and canvas-wide opacity
- Isometric projection: coordinate conversion, diamond hit testing, depth sorting, tilemap rendering
- Color adjustments: brightness, contrast, saturation, hue shift, tinting, grayscale, invert, posterize, threshold, sepia
- HSL/HSV color space conversions
- Image filters: box blur, Gaussian blur, sharpen, edge detection, bloom
- Sprite sheet packing (shelf bin packing) with metadata
- Built-in 8x8 pixel font (full printable ASCII)
- Procedural VFX generators (explosion, ring, glow, trail, flash, sparks)
- Procedural noise (value noise, Perlin noise, simplex noise, Worley/cellular noise, FBM, turbulence)
- Gradients (linear, radial, diagonal)
- Nine-slice UI panel scaling
- Ordered Bayer dithering for palette reduction
- Palette extraction (median cut) and color quantization
- LUT color grading with trilinear interpolation (identity, warm, cool, cinematic presets)
- BMP and PNG import/export (32-bit RGBA, native — no external image libraries)

**Core dependencies:** `base`, `bytestring`, `zlib`.

---

## Architecture

```
src/GBSprite/
├── Color.hs       RGBA type, HSL/HSV, named colors, lerp, multiply, alpha blend, color transforms
├── Canvas.hs      2D pixel grid (strict ByteString), drawing primitives, flood fill
├── Draw.hs        Thick lines, polygons, ellipses, arcs, Bezier/Catmull-Rom curves, AA lines
├── Adjust.hs      Canvas-wide color adjustments, LUT color grading (brightness, contrast, saturation, etc.)
├── Filter.hs      Convolution filters (box/Gaussian blur, sharpen, edge detection, bloom)
├── Palette.hs     Indexed palettes, palette swap, extraction (median cut), quantization
├── Sprite.hs      Named sprite with origin, frames, bounding box, mirroring, trimming
├── Animation.hs   Loop/Once/PingPong frame sequencing, frame blending
├── Transform.hs   Flip, rotate (90/180/270/arbitrary), scale (nearest/bilinear), shear, outline, shadow
├── Compose.hs     Alpha-blended layering, blend modes, masking
├── Sheet.hs       Shelf bin packing into sprite atlas
├── Text.hs        Built-in 8x8 pixel font rendering
├── Isometric.hs   2:1 isometric projection, coordinate conversion, hit testing, depth sorting
├── Tilemap.hs     Tile-based map rendering from atlas
├── VFX.hs         Procedural effects (explosion, ring, glow, trail, flash, sparks)
├── Noise.hs       Value noise, Perlin noise, simplex noise, Worley noise, FBM, turbulence
├── Gradient.hs    Linear, radial, diagonal gradients
├── NineSlice.hs   UI panel scaling with border preservation
├── Dither.hs      Ordered Bayer dithering for palette reduction
├── Import.hs      BMP and PNG file decoding
├── BMP.hs         32-bit BGRA BMP export
├── PNG.hs         32-bit RGBA PNG export (zlib compressed)
└── Export.hs      Unified export API (BMP + PNG)
```

### Pipeline

```
Canvas → Draw/Transform/Compose → Sprite → Animation → Sheet (atlas) → BMP/PNG
                                                  ↗
VFX generators → [Canvas] frames ─────────────────┘
                                                  ↗
Import (BMP/PNG) → Canvas ────────────────────────┘
```

---

## Usage

### As a dependency

Add to your `.cabal` file:

```cabal
build-depends: gb-sprite >= 0.5
```

### Generating sprites

```haskell
import GBSprite.Canvas (newCanvas, fillCircle)
import GBSprite.Color (red, transparent)
import GBSprite.Export (writeBmp)

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
data HSL = HSL { hslH, hslS, hslL :: !Double }
data HSV = HSV { hsvH, hsvS, hsvV :: !Double }

-- Named colors
red, green, blue, white, black, transparent :: Color

-- Math
lerp       :: Double -> Color -> Color -> Color   -- linear interpolation
multiply   :: Color -> Color -> Color              -- component-wise multiply
alphaBlend :: Color -> Color -> Color              -- Porter-Duff "over"
withAlpha  :: Word8 -> Color -> Color              -- replace alpha channel
scaleAlpha :: Double -> Color -> Color             -- scale alpha channel

-- Color space
toHSL  :: Color -> HSL
fromHSL :: HSL -> Color
toHSV  :: Color -> HSV
fromHSV :: HSV -> Color

-- Color transforms
tintColor      :: Color -> Color -> Color           -- multiply RGB channels
invertColor    :: Color -> Color                    -- negate RGB
grayscaleColor :: Color -> Color                    -- BT.709 luminance
brightenColor  :: Double -> Color -> Color          -- toward white/black
contrastColor  :: Double -> Color -> Color          -- scale from midpoint
saturateColor  :: Double -> Color -> Color          -- boost/reduce saturation
shiftHueColor  :: Double -> Color -> Color          -- rotate hue by degrees
```

### Canvas

```haskell
data Canvas = Canvas { cWidth :: !Int, cHeight :: !Int, cPixels :: !BS.ByteString }

newCanvas  :: Int -> Int -> Color -> Canvas        -- blank canvas filled with color
fromPixels :: Int -> Int -> [Color] -> Canvas      -- from color list (row-major)
setPixel   :: Canvas -> Int -> Int -> Color -> Canvas
getPixel   :: Canvas -> Int -> Int -> Color
drawLine   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas  -- Bresenham
drawRect   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRect   :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
fillCircle :: Canvas -> Int -> Int -> Int -> Color -> Canvas
floodFill  :: Canvas -> Int -> Int -> Color -> Canvas
crop            :: Int -> Int -> Int -> Int -> Canvas -> Canvas
trimTransparent :: Canvas -> Canvas
canvasOpacity   :: Double -> Canvas -> Canvas
mapPixels       :: (Color -> Color) -> Canvas -> Canvas
pixelFold       :: (a -> Int -> Int -> Color -> a) -> a -> Canvas -> a
```

### Draw

```haskell
drawThickLine  :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
drawPolygon    :: Canvas -> [(Int, Int)] -> Color -> Canvas
fillPolygon    :: Canvas -> [(Int, Int)] -> Color -> Canvas  -- scanline fill
drawEllipse    :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillEllipse    :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawArc        :: Canvas -> Int -> Int -> Int -> Int -> Double -> Double -> Color -> Canvas
drawBezier     :: Canvas -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Color -> Canvas
drawCubicBezier :: Canvas -> (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Color -> Canvas
drawCatmullRom :: Canvas -> [(Int, Int)] -> Color -> Canvas
drawAALine     :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas  -- Wu's algorithm
patternFill    :: Canvas -> Int -> Int -> Int -> Int -> Canvas -> Canvas
drawRoundRect  :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRoundRect  :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
```

### Transform

```haskell
flipH, flipV         :: Canvas -> Canvas
rotate90, rotate180, rotate270 :: Canvas -> Canvas
rotateArbitrary :: Double -> Canvas -> Canvas        -- degrees, bilinear interpolation
scaleNearest    :: Int -> Canvas -> Canvas            -- nearest-neighbor upscale
scaleBilinear   :: Double -> Canvas -> Canvas         -- bilinear up/down
scaleTo         :: Int -> Int -> Canvas -> Canvas     -- target dimensions, bilinear
shearH, shearV  :: Double -> Canvas -> Canvas         -- horizontal/vertical shear
outline         :: Color -> Canvas -> Canvas
dropShadow      :: Int -> Int -> Color -> Canvas -> Canvas
```

### Compose

```haskell
data BlendMode = BlendMultiply | BlendScreen | BlendOverlay
               | BlendAdditive | BlendSoftLight | BlendDifference

stamp        :: Canvas -> Int -> Int -> Canvas -> Canvas  -- direct overwrite
stampAlpha   :: Canvas -> Int -> Int -> Canvas -> Canvas   -- alpha blended
overlay      :: Canvas -> Canvas -> Canvas                 -- same-size blend
overlayAt    :: Canvas -> Int -> Int -> Canvas -> Canvas   -- offset blend
blendCompose :: BlendMode -> Canvas -> Int -> Int -> Canvas -> Canvas
maskCanvas   :: Canvas -> Canvas -> Canvas                 -- alpha masking
```

### Adjust

```haskell
grayscale        :: Canvas -> Canvas
invertColors     :: Canvas -> Canvas
applyTint        :: Color -> Canvas -> Canvas
adjustBrightness :: Double -> Canvas -> Canvas
adjustContrast   :: Double -> Canvas -> Canvas
adjustSaturation :: Double -> Canvas -> Canvas
shiftHue         :: Double -> Canvas -> Canvas
remapColor       :: Color -> Color -> Canvas -> Canvas
remapColors      :: [(Color, Color)] -> Canvas -> Canvas
posterize        :: Int -> Canvas -> Canvas
threshold        :: Word8 -> Canvas -> Canvas
sepia            :: Canvas -> Canvas

-- LUT color grading
data ColorLUT = ColorLUT { lutSize :: !Int, lutData :: !BS.ByteString }

identityLUT  :: Int -> ColorLUT                         -- no-op LUT
applyLUT     :: ColorLUT -> Canvas -> Canvas            -- trilinear interpolation
modifyLUT    :: ColorLUT -> (Color -> Color) -> ColorLUT -- compose adjustments
warmLUT      :: Int -> ColorLUT                         -- sunset/firelight
coolLUT      :: Int -> ColorLUT                         -- moonlight/underwater
cinematicLUT :: Int -> ColorLUT                         -- teal-and-orange
```

### Filter

```haskell
boxBlur      :: Int -> Canvas -> Canvas              -- separable box blur
gaussianBlur :: Int -> Canvas -> Canvas              -- 3-pass box blur approximation
sharpen      :: Canvas -> Canvas                     -- unsharp mask
edgeDetect   :: Canvas -> Canvas                     -- Sobel gradient magnitude
bloom        :: Int -> Double -> Canvas -> Canvas    -- bright extraction + blur + additive
```

### Isometric

```haskell
data IsoConfig = IsoConfig { isoTileWidth :: !Int, isoTileHeight :: !Int }

defaultIsoConfig :: IsoConfig                        -- 64x32 standard 2:1

worldToScreen    :: IsoConfig -> Int -> Int -> (Int, Int)
screenToWorld    :: IsoConfig -> Int -> Int -> (Int, Int)
pointInDiamond   :: IsoConfig -> Int -> Int -> Int -> Int -> Bool
isoDepthCompare  :: (Int, Int) -> (Int, Int) -> Ordering
drawDiamond      :: Canvas -> IsoConfig -> Int -> Int -> Color -> Canvas
fillDiamond      :: Canvas -> IsoConfig -> Int -> Int -> Color -> Canvas
renderIsoMap     :: IsoConfig -> [(Int, Canvas)] -> [[Int]] -> Canvas
```

### Noise

```haskell
valueNoise      :: Int -> Int -> Int -> Double -> Canvas
valueNoiseColor :: Int -> Int -> Int -> Double -> Color -> Color -> Canvas
fbm             :: Int -> Int -> Int -> Int -> Double -> Canvas
perlinNoise     :: Int -> Int -> Int -> Double -> Canvas
simplexNoise    :: Int -> Int -> Int -> Double -> Canvas
worleyNoise     :: Int -> Int -> Int -> Int -> Double -> Canvas
turbulence      :: Int -> Int -> Int -> Int -> Double -> Canvas
```

### Gradient

```haskell
linearGradient   :: Int -> Int -> Color -> Color -> Bool -> Canvas
radialGradient   :: Int -> Int -> Int -> Int -> Int -> Color -> Color -> Canvas
diagonalGradient :: Int -> Int -> Color -> Color -> Canvas
```

### NineSlice

```haskell
data NineSlice = NineSlice
  { nsCanvas :: !Canvas, nsLeft :: !Int, nsRight :: !Int, nsTop :: !Int, nsBottom :: !Int }

nineSlice       :: Canvas -> Int -> Int -> Int -> Int -> NineSlice
renderNineSlice :: NineSlice -> Int -> Int -> Canvas
```

### Palette

```haskell
newtype Palette = Palette { paletteColors :: [Color] }

-- Built-in palettes
grayscale4, grayscale8 :: Palette
gameboy              :: Palette
nes                  :: Palette

-- Operations
fromColors     :: [Color] -> Palette
paletteColor   :: Palette -> Int -> Color
paletteSwap    :: Palette -> Palette -> Color -> Color
paletteLerp    :: Double -> Palette -> Palette -> Palette
extractPalette :: Int -> [Color] -> Palette            -- median cut extraction
quantizeColor  :: Palette -> Color -> Color
```

### Sprite

```haskell
data Sprite = Sprite
  { spriteName :: !String, spriteOriginX :: !Int, spriteOriginY :: !Int
  , spriteFrames :: ![Canvas], spriteBounds :: !(Maybe BoundingBox) }

singleFrame  :: String -> Canvas -> Sprite
multiFrame   :: String -> Int -> Int -> [Canvas] -> Sprite
spriteWidth  :: Sprite -> Int
spriteHeight :: Sprite -> Int
frameCount   :: Sprite -> Int
getFrame     :: Sprite -> Int -> Maybe Canvas
mirrorSprite :: Sprite -> Sprite
trimSprite   :: Sprite -> Sprite
```

### Animation

```haskell
data LoopMode = Loop | Once | PingPong

data Animation = Animation
  { animFrameDelay :: !Int, animFrameCount :: !Int, animLoopMode :: !LoopMode }

animation         :: Int -> Int -> LoopMode -> Animation
loopAnimation     :: Int -> Int -> Animation
onceAnimation     :: Int -> Int -> Animation
pingPongAnimation :: Int -> Int -> Animation
animationFrame    :: Animation -> Int -> Int
animationDone     :: Animation -> Int -> Bool
blendFrames       :: Double -> Canvas -> Canvas -> Canvas
```

### Dither

```haskell
data DitherMatrix = Bayer2 | Bayer4 | Bayer8

orderedDither :: DitherMatrix -> Palette -> Canvas -> Canvas
```

### Sheet

```haskell
data SpriteSheet = SpriteSheet { sheetCanvas :: !Canvas, sheetEntries :: ![SheetEntry] }
data SheetEntry  = SheetEntry  { entryName :: !String, entryX, entryY, entryWidth, entryHeight :: !Int }

packSheet :: Int -> [(String, Canvas)] -> SpriteSheet
```

### Text

```haskell
defaultFont :: Font
renderText  :: Font -> Color -> String -> Canvas
renderChar  :: Font -> Color -> Char -> Canvas
textWidth   :: Font -> String -> Int
textHeight  :: Font -> Int
```

### VFX

```haskell
explosionFrames  :: ExplosionConfig -> [Canvas]
ringExpandFrames :: RingConfig -> [Canvas]
glowPulseFrames  :: GlowConfig -> [Canvas]
trailFrames      :: TrailConfig -> [Canvas]
flashFrames      :: Int -> Color -> [Canvas]
sparksFrames     :: SparksConfig -> [Canvas]
```

### Tilemap

```haskell
data TilemapConfig = TilemapConfig
  { tmTileWidth :: !Int, tmTileHeight :: !Int
  , tmGridWidth :: !Int, tmGridHeight :: !Int
  , tmTiles :: ![Int] }

renderTilemap :: SpriteSheet -> TilemapConfig -> Canvas
```

### Import

```haskell
readBmp   :: FilePath -> IO (Either String Canvas)
decodeBmp :: BS.ByteString -> Either String Canvas
readPng   :: FilePath -> IO (Either String Canvas)
decodePng :: BL.ByteString -> Either String Canvas
```

### BMP / PNG / Export

```haskell
encodeBmp :: Canvas -> BL.ByteString
writeBmp  :: FilePath -> Canvas -> IO ()
encodePng :: Canvas -> BL.ByteString
writePng  :: FilePath -> Canvas -> IO ()
exportBmp :: FilePath -> Canvas -> IO ()
exportPng :: FilePath -> Canvas -> IO ()
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
import GBSprite.Export (writeBmp)

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

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.8.

```bash
cabal build                              # Build library
cabal test                               # Run all tests (615 pure tests)
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal haddock                            # Generate docs
```

---

<p align="center">
  <sub>BSD-3-Clause License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
