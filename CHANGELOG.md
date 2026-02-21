# Changelog

## 0.2.1.1

- Remove unused `containers` dependency.

## 0.2.1.0

### Performance

Library-wide elimination of O(n²) per-pixel `setPixel`/`VS.//` mutation. All pixel-producing functions now use single-pass `VS.generate` for O(n) performance.

- **NineSlice**: `renderNineSlice` — rewritten for large target sizes that previously stalled.
- **Transform**: `flipH`, `flipV`, `rotate90`, `rotate180`, `rotate270`, `scaleNearest`, `outline`, `dropShadow` — all rewritten with direct index computation.
- **Compose**: `stamp`, `stampAlpha` — single-pass with inline source/destination byte reads.
- **Gradient**: `linearGradient`, `radialGradient`, `diagonalGradient` — generate directly from coordinate math.
- **Noise**: `valueNoiseColor`, `fbm` — generate noise samples directly into output vector.
- **Dither**: `orderedDither` — single-pass palette reduction.

## 0.2.0.0

### New Modules

- **GBSprite.Noise**: Procedural noise generation — `valueNoise`, `valueNoiseColor`, `fbm` (fractal Brownian motion). Deterministic LCG with bilinear interpolation and smoothstep.
- **GBSprite.Gradient**: Gradient generation — `linearGradient` (horizontal/vertical), `radialGradient`, `diagonalGradient`.
- **GBSprite.NineSlice**: UI panel scaling — `NineSlice` type with configurable border regions, `renderNineSlice` preserves corners and edges at any target size.
- **GBSprite.Dither**: Ordered dithering — `orderedDither` with Bayer2, Bayer4, Bayer8 matrices for palette reduction. Classic retro cross-hatch patterns.
- **GBSprite.Export.PNG**: Separated from Export.hs, eliminating CPP. Only built when `juicy-pixels` flag is enabled.

### Bug Fixes

- Eliminate CPP from Export.hs — split into `GBSprite.Export` (BMP, always available) and `GBSprite.Export.PNG` (optional).

### Internal

- 105 pure tests (up from 62)
- 12 new Draw function tests (drawThickLine, drawPolygon, fillPolygon, drawEllipse, fillEllipse, drawBezier, drawRoundRect, fillRoundRect)
- CI: ormolu glob `src/**/*.hs` replaces hardcoded file list
- CI: jobs chained so lint/format gate build (saves Actions minutes on failure)
- Metadata: cabal-version 3.0, stability experimental, CHANGELOG.md

## 0.1.0.0

Initial release.

### Canvas & Color
- `Canvas` type: width, height, RGBA pixel vector
- `Color` type with `lerp`, `multiply`, `alphaBlend`, `withAlpha`, `scaleAlpha`
- Bounds-checked `getPixel` / `setPixel` (out-of-bounds returns transparent)

### Drawing Primitives
- Lines: `drawLine`, `drawThickLine`
- Rectangles: `drawRect`, `fillRect`, `drawRoundRect`, `fillRoundRect`
- Circles: `drawCircle`, `fillCircle`
- Ellipses: `drawEllipse`, `fillEllipse`
- Polygons: `drawPolygon`, `fillPolygon`
- Curves: `drawBezier`, `drawArc`

### Transforms
- `flipH`, `flipV`, `rotate90`, `rotate180`, `rotate270`
- `scaleNearest`: nearest-neighbour scaling

### Composition
- `stamp`: blit with transparency
- `overlay`: layer multiple canvases

### Animation
- `Animation` type with `Loop`, `Once`, `PingPong` modes
- Frame indexing with `animationDone` predicate

### Sprites & Sheets
- `Sprite` type: multi-frame sprite container
- `packSheet`: bin-packing sprite sheet atlas

### Text
- `renderText`: bitmap font text rendering
- `textWidth`: measure text width in pixels

### VFX
- `explosion`: animated explosion effect
- `ring`: expanding ring effect
- `flash`: screen flash frames

### Palettes
- `Palette` type with indexed color lookup
- `paletteSwap`: remap colors
- Built-in: `gameboy` (4-color Game Boy palette)

### Export
- `exportBmp`: native BMP file export (always available)
- `exportPng`: PNG export via JuicyPixels (optional `juicy-pixels` flag)
