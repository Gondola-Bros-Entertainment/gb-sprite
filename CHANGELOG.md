# Changelog

## 0.5.2.0

### Performance

Library-wide optimization pass targeting large canvas rendering (4K / procedural generation). Drawing primitives are now **orders of magnitude faster** on large canvases.

- **Canvas: copy-and-poke drawing** (`Canvas`): All drawing primitives (`drawLine`, `drawRect`, `fillRect`, `drawCircle`, `fillCircle`, `hLine`, `floodFill`) now copy the pixel buffer once and write directly into the copy. Previously, each pixel write copied the entire buffer via `ByteString` split/concat. A `fillCircle` of radius 100 on a 1000×1000 canvas went from ~31,000 full buffer copies to 1.
- **Canvas: `mapPixels` calls function once per pixel** — was 4× (once per RGBA channel). Functions like `grayscale`, `adjustBrightness`, `shiftHue`, `applyLUT`, etc. all benefit.
- **Canvas: `getPixel` uses `unsafeIndex`** — eliminates 4 redundant bounds checks per read (after `inBounds` check). Benefits bilinear sampling in scaling/rotation.
- **Canvas: `pixelFold` tracks coordinates directly** — eliminates `div`/`mod` per pixel.
- **Canvas: `fromPixels` uses direct pokes** — no intermediate list allocation.
- **Draw: single-copy polygon/ellipse/curve rendering** (`Draw`): `fillPolygon`, `fillEllipse`, `drawEllipse`, `drawPolygon`, `drawArc`, `drawBezier`, `drawCubicBezier`, `drawCatmullRom`, `drawAALine`, and `patternFill` now collect all pixels/spans and apply in a single buffer copy via the new bulk APIs.
- **Transform: `generateCanvasPixels` eliminates per-byte division** (`Transform`): All transforms (`flipH`/`flipV`, `rotate*`, `scaleNearest`, `scaleBilinear`, `scaleTo`, `shearH`/`shearV`, `outline`, `dropShadow`) now compute one color per pixel instead of dispatching per byte. Eliminates ~16 integer divisions per pixel — significant at 4K (8M+ pixels).
- **Noise: bitmask LCG hash** (`Noise`): LCG hash uses bitwise `.&.` instead of `mod 2^31` (~10–20× faster per hash). Perlin gradient index uses `.&.` for power-of-2 table size. All noise generators use `generateCanvasPixels` for the same div/mod elimination.
- **VFX: bulk ring drawing** (`VFX`): Ring outlines use `bulkSetPixels` (1 buffer copy) instead of per-pixel `setPixel` chains. LCG uses bitmask.
- **Gradient: `generateCanvasPixels`** (`Gradient`): Same per-pixel div/mod elimination.
- **Dither: `generateCanvasPixels`** (`Dither`): Same per-pixel div/mod elimination.
- **Tilemap: bulk tile stamping** (`Tilemap`): `stampTile` uses `bulkSetPixels` instead of per-pixel `setPixel` loop.
- **Text: bulk glyph rendering** (`Text`): Glyph bit-blitting uses `bulkSetPixels` instead of per-bit `setPixel` loop.

### New Features

- **Bulk drawing APIs** (`Canvas`): `generateCanvasPixels`, `bulkSetPixels`, `bulkHSpans`, `bulkBlendPixels` — low-level batch operations for efficient custom drawing. Used internally by all optimized primitives, also exported for user code.

### Tests

- 615 tests (all passing — exact pixel-level output preserved).

## 0.5.1.0

### New Features

- **Simplex noise** (`Noise`): `simplexNoise` — triangular-grid noise with fewer directional artifacts than Perlin. Same API as `perlinNoise`.
- **LUT color grading** (`Adjust`): `ColorLUT` type with trilinear interpolation. `identityLUT`, `applyLUT`, `modifyLUT` for composable grading, plus presets `warmLUT`, `coolLUT`, `cinematicLUT`.

### Tests

- 615 tests.

## 0.5.0.0

### Breaking Changes

- **`BMP.hs` and `PNG.hs` are now pure.** `writeBmp` and `writePng` have moved to `GBSprite.Export`. Import from `GBSprite.Export` instead of `GBSprite.BMP` or `GBSprite.PNG` for file I/O. The pure encoders `encodeBmp` and `encodePng` remain in their original modules.
- **Eliminated `unsafePerformIO`** from `Import.hs`. PNG decompression now uses the zlib streaming API (`Codec.Compression.Zlib.Internal`) for fully pure, exception-free error handling.

### New Modules

- **`GBSprite.Adjust`** — Canvas-wide color adjustments: grayscale, invert, tint, brightness, contrast, saturation, hue shift, color remapping, posterize, threshold, sepia.
- **`GBSprite.Filter`** — Convolution filters: box blur, Gaussian blur (3-pass approximation), sharpen (unsharp mask), Sobel edge detection, bloom (bright extract + blur + additive blend).
- **`GBSprite.Import`** — Native BMP and PNG decoding. Read 24/32-bit BMP and 8-bit RGB/RGBA PNG files back into `Canvas`. Supports all 5 PNG filter types (None, Sub, Up, Average, Paeth). Pure decoders (`decodeBmp`, `decodePng`) and IO wrappers (`readBmp`, `readPng`).
- **`GBSprite.Isometric`** — Isometric projection for 2:1 diamond tiles. World/screen coordinate conversion, point-in-diamond hit testing, depth sorting, diamond drawing/filling, and isometric tilemap rendering.

### New Features

- **HSL/HSV color space** (`Color`): `toHSL`, `fromHSL`, `toHSV`, `fromHSV`, and derived transforms (`tintColor`, `invertColor`, `grayscaleColor`, `brightenColor`, `contrastColor`, `saturateColor`, `shiftHueColor`).
- **Arbitrary rotation** (`Transform`): `rotateArbitrary` with bilinear interpolation for any angle in degrees.
- **Bilinear scaling** (`Transform`): `scaleBilinear` for smooth up/down scaling; `scaleTo` for exact target dimensions.
- **Shear transforms** (`Transform`): `shearH` and `shearV`.
- **Anti-aliased lines** (`Draw`): `drawAALine` using Wu's algorithm.
- **Cubic Bezier curves** (`Draw`): `drawCubicBezier`.
- **Catmull-Rom splines** (`Draw`): `drawCatmullRom` — smooth curves through control points.
- **Pattern fill** (`Draw`): `patternFill` — fill a rectangular area with a repeating tile.
- **Blend modes** (`Compose`): `BlendMultiply`, `BlendScreen`, `BlendOverlay`, `BlendAdditive`, `BlendSoftLight`, `BlendDifference` via `blendCompose`.
- **Alpha masking** (`Compose`): `maskCanvas` — clip rendering to a mask's alpha channel.
- **Perlin noise** (`Noise`): `perlinNoise` — gradient noise with dot-product interpolation.
- **Worley noise** (`Noise`): `worleyNoise` — cellular/Voronoi patterns.
- **Turbulence** (`Noise`): `turbulence` — absolute-value FBM for fire/cloud/water textures.
- **Palette lerp** (`Palette`): `paletteLerp` — interpolate between two palettes for day/night transitions.
- **Palette extraction** (`Palette`): `extractPalette` — derive a palette from a canvas using median cut.
- **Color quantization** (`Palette`): `quantizeColor` — map a color to the nearest palette entry.
- **Sprite mirroring** (`Sprite`): `mirrorSprite` — horizontal flip of all frames.
- **Sprite trimming** (`Sprite`): `trimSprite` — crop transparent borders from all frames.
- **Animation blending** (`Animation`): `blendFrames` — cross-fade between two canvases.
- **Canvas crop/trim** (`Canvas`): `crop`, `trimTransparent`, `canvasOpacity`.
- Canvas dimension clamping: `newCanvas` and `fromPixels` now clamp dimensions to minimum 1.
- Empty string guard: `renderText` returns a valid 1-pixel-wide canvas for empty input.

### Bug Fixes

- **Fix Porter-Duff alpha blend** (`Color`): `alphaBlend` now correctly weights the destination channel by `dstA` when compositing semi-transparent colors. Previously, the destination RGB contribution was over-weighted when `dstA < 255`.
- **Fix isometric diamond off-by-one** (`Isometric`): `drawDiamond` and `fillDiamond` now draw within the `tw x th` bounding box (previously drew `(tw+1) x (th+1)` pixels).
- **Fix shear direction** (`Transform`): `shearH` and `shearV` now shift in the documented direction (positive values shift bottom rows right / right columns down).
- **Fix PNG decompression exception** (`Import`): `decodePng` uses the zlib streaming API for pure error handling — no `unsafePerformIO`, no caught exceptions.
- **Fix bloom luminance** (`Filter`): Bloom threshold now uses BT.709 perceptual weights instead of simple RGB average.
- **Fix sprite sheet vertical padding** (`Sheet`): Shelf packing now applies padding between shelves vertically, not just horizontally.
- **Fix Perlin gradient vectors** (`Noise`): Diagonal gradients now use exact `1/sqrt(2)` instead of truncated `0.707`.
- **Fix dither performance** (`Dither`): Bayer matrices backed by `ByteString` for O(1) lookup (was O(n) list traversal per pixel). `findClosest` now carries best distance in accumulator (halves comparisons).
- **Fix `wrapHue` performance** (`Color`): O(1) modular arithmetic replaces recursive subtract/add loop.
- **Fix `maskCanvas` docstring** (`Compose`): Corrected to say "alpha channel" (was incorrectly "luminance").

### Tests

- 591 tests. Full coverage of all new modules and bug fixes.

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
