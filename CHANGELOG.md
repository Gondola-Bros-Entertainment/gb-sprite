# Changelog

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

### Procedural Noise
- `valueNoise`: deterministic grayscale noise with bilinear interpolation
- `valueNoiseColor`: noise between two colors
- `fbm`: fractal Brownian motion (octave layering)

### Gradients
- `linearGradient`: horizontal or vertical color transitions
- `radialGradient`: circular gradient from center point
- `diagonalGradient`: top-left to bottom-right transition

### Nine-Slice Scaling
- `NineSlice` type with configurable border regions
- `renderNineSlice`: scale UI panels preserving corners and edges

### Ordered Dithering
- `DitherMatrix`: Bayer2, Bayer4, Bayer8
- `orderedDither`: reduce canvas to palette with Bayer matrix dithering

### Export
- `exportBmp`: native BMP file export (always available)
- `exportPng`: PNG export via JuicyPixels (optional `juicy-pixels` flag)

### Internal
- 105 pure tests: color math, canvas operations, drawing primitives, transforms, composition, animation, sprites, sheets, text, VFX, BMP encoding, noise, gradients, nine-slice, dithering
- All partial functions eliminated
