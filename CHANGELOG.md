# Changelog

## 0.4.0.0

### Breaking Changes

- **Drop `vector` dependency.** Canvas pixel storage changed from `Data.Vector.Storable.Vector Word8` to strict `ByteString`. Users pattern-matching on `cPixels` will need to update. All public API functions are unchanged.

### New

- **Native PNG export** (`GBSprite.PNG`): hand-rolled 32-bit RGBA PNG encoder using zlib for DEFLATE compression and a pure CRC32 implementation. PNG export is now always available — no flags, no JuicyPixels.
- `GBSprite.Export` now re-exports both `exportBmp` and `exportPng`.
- 10 new PNG tests (366 total, up from 356).

### Removed

- **`juicy-pixels` flag and `GBSprite.Export.PNG`** — replaced by native `GBSprite.PNG`.
- **`vector` dependency** — pixel data backed by strict `ByteString` instead of `Vector.Storable`. Same performance characteristics, fewer dependencies.

### Dependencies

- `base`, `bytestring`, `zlib` — that's it. Down from `base`, `bytestring`, `vector`, optional `JuicyPixels`.

## 0.3.0.1

### Fixes

- Fix BMP roundtrip test leaking temp files — tests now clean up after themselves.
- Replace partial `!!` with safe indexing in BMP test.
- Fix CI: run haddock before build to avoid stale `.hi` files causing 0% doc coverage.
- Fix README: correct `encodeBmp` return type, add missing API sections (Palette, Sprite, Tilemap), add missing exported functions.

## 0.3.0.0

### Bug Fixes

- Fix `drawEllipse` midpoint algorithm: region 2 now continues from where region 1 ended instead of restarting at `(rx, 0)`. Ellipses now correctly plot all four cardinal edge pixels.
- Fix `safeSetPixel` in VFX.hs: use `setPixel` directly instead of `fillCircle` with radius 0.
- Replace partial `minimum`/`maximum` with safe folds in `fillPolygon` (Draw.hs) and `computeMaxWidth` (Sheet.hs).
- Fix `grayscale4` palette: was a duplicate of `gameboy` (green-tinted). Now uses evenly-spaced true grayscale (0, 85, 170, 255).

### Performance

- Replace 16 lazy `foldl` with strict `foldl'` across Draw.hs, VFX.hs, Sheet.hs, Tilemap.hs, and Text.hs.

### Internal

- 356 tests (up from 105): comprehensive coverage of all modules.
- CI and cabal: `tested-with` upgraded from GHC 9.6 to GHC 9.8.
- Publish workflow: replace sed version-patching with tag/cabal version verification.

## 0.2.1.2

- Change license from MIT to BSD-3-Clause.

### Bug Fixes

- Fix `maximum` crash on empty list in `packSheet` (Sheet.hs).
- Replace partial `!!` with safe indexing in Bayer dithering (Dither.hs).
- Fix `-Wname-shadowing` warning in test suite.

### Documentation

- Fix README Animation API section (wrong type name and signatures).
- Add gb-vector as ecosystem companion in README.

### Internal

- CI: cross-platform build matrix (Linux, macOS, Windows).

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
