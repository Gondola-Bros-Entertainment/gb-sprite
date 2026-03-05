-- | gb-sprite test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gbnet-hs.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GBSprite.Animation (Animation (..), LoopMode (..), animation, animationDone, animationFrame, loopAnimation, onceAnimation, pingPongAnimation)
import GBSprite.BMP (encodeBmp, writeBmp)
import GBSprite.Canvas (Canvas (..), clearCanvas, drawCircle, drawLine, drawRect, fillCircle, fillRect, floodFill, fromPixels, getPixel, hLine, inBounds, newCanvas, pixelIndex, setPixel)
import GBSprite.Color (Color (..), alphaBlend, black, blue, cyan, darkGray, gray, green, lerp, lightGray, magenta, multiply, orange, pink, purple, red, scaleAlpha, transparent, white, withAlpha, yellow)
import GBSprite.Compose (overlay, overlayAt, stamp, stampAlpha)
import GBSprite.Dither (DitherMatrix (..), orderedDither)
import GBSprite.Draw (drawArc, drawBezier, drawEllipse, drawPolygon, drawRoundRect, drawThickLine, fillEllipse, fillPolygon, fillRoundRect)
import GBSprite.Gradient (diagonalGradient, linearGradient, radialGradient)
import GBSprite.NineSlice (NineSlice (..), nineSlice, renderNineSlice)
import GBSprite.Noise (fbm, valueNoise, valueNoiseColor)
import GBSprite.PNG (encodePng, writePng)
import GBSprite.Palette (Palette (..), fromColors, gameboy, grayscale4, grayscale8, nes, paletteColor, paletteSwap)
import GBSprite.Sheet (SheetEntry (..), SpriteSheet (..), packSheet)
import GBSprite.Sprite (BoundingBox (..), Sprite (..), frameCount, getFrame, multiFrame, singleFrame, spriteHeight, spriteWidth)
import GBSprite.Text (Font (..), defaultFont, renderChar, renderText, textHeight, textWidth)
import GBSprite.Tilemap (TilemapConfig (..), renderTilemap)
import GBSprite.Transform (dropShadow, flipH, flipV, outline, rotate180, rotate270, rotate90, scaleNearest)
import GBSprite.VFX (ExplosionConfig (..), GlowConfig (..), RingConfig (..), SparksConfig (..), TrailConfig (..), explosionFrames, flashFrames, glowPulseFrames, ringExpandFrames, sparksFrames, trailFrames)
import System.Directory (removeFile)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hClose, hFlush, openTempFile, stdout)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

type TestResult = Either String ()

assertEqual :: (Show a, Eq a) => String -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected "
            ++ show expected
            ++ ", got "
            ++ show actual
        )

assertTrue :: String -> Bool -> TestResult
assertTrue _ True = Right ()
assertTrue label False = Left (label ++ ": expected True")

runTests :: [(String, TestResult)] -> IO ()
runTests tests = go tests (0 :: Int) (0 :: Int)
  where
    go [] passed total = do
      putStrLn ""
      putStrLn
        ( show passed
            ++ "/"
            ++ show total
            ++ " tests passed."
        )
      if passed == total then exitSuccess else exitFailure
    go ((name, result) : rest) passed total = do
      case result of
        Right () -> do
          putStrLn ("  PASS: " ++ name)
          hFlush stdout
          go rest (passed + 1) (total + 1)
        Left msg -> do
          putStrLn ("  FAIL: " ++ name ++ " - " ++ msg)
          hFlush stdout
          exitFailure

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "gb-sprite tests"
  putStrLn (replicate 40 '-')
  bmpTests <- testBmpRoundtrip
  pngTests <- testPngRoundtrip
  runTests
    ( testColor
        ++ testColorNamed
        ++ testColorInstances
        ++ testCanvas
        ++ testCanvasAdvanced
        ++ testTransform
        ++ testTransformEffects
        ++ testCompose
        ++ testComposeAdvanced
        ++ testAnimation
        ++ testAnimationAdvanced
        ++ testPalette
        ++ testPaletteBuiltins
        ++ testSprite
        ++ testSpriteAdvanced
        ++ testSheet
        ++ testSheetAdvanced
        ++ testText
        ++ testTextAdvanced
        ++ testVFX
        ++ testVFXGlow
        ++ testVFXTrail
        ++ testVFXSparks
        ++ bmpTests
        ++ testBmpEncode
        ++ pngTests
        ++ testPngEncode
        ++ testDraw
        ++ testDrawAdvanced
        ++ testEllipse
        ++ testNoise
        ++ testNoiseAdvanced
        ++ testGradient
        ++ testGradientAdvanced
        ++ testNineSlice
        ++ testNineSliceAdvanced
        ++ testDither
        ++ testDitherAdvanced
        ++ testTilemap
    )

-- ---------------------------------------------------------------------------
-- Color tests
-- ---------------------------------------------------------------------------

testColor :: [(String, TestResult)]
testColor =
  [ ( "lerp t=0 gives start",
      assertEqual "lerp 0" red (lerp 0.0 red blue)
    ),
    ( "lerp t=1 gives end",
      assertEqual "lerp 1" blue (lerp 1.0 red blue)
    ),
    ( "lerp t=0.5 midpoint",
      let mid = lerp 0.5 black white
       in assertTrue "midpoint gray" (colorR mid >= 127 && colorR mid <= 128)
    ),
    ( "lerp clamps below 0",
      assertEqual "lerp negative" red (lerp (-1.0) red blue)
    ),
    ( "lerp clamps above 1",
      assertEqual "lerp over 1" blue (lerp 2.0 red blue)
    ),
    ( "lerp same color",
      assertEqual "lerp same" red (lerp 0.5 red red)
    ),
    ( "multiply white identity",
      assertEqual "mul white" red (multiply white red)
    ),
    ( "multiply black zero",
      assertEqual "mul black" (Color 0 0 0 255) (multiply black red)
    ),
    ( "multiply self",
      let result = multiply red red
       in assertTrue "mul self" (colorR result == 255 && colorA result == 255)
    ),
    ( "multiply commutative",
      assertEqual "mul commutative" (multiply red blue) (multiply blue red)
    ),
    ( "alphaBlend opaque over = top",
      assertEqual "opaque over" red (alphaBlend red blue)
    ),
    ( "alphaBlend transparent over = bottom",
      assertEqual "transparent over" blue (alphaBlend transparent blue)
    ),
    ( "alphaBlend semi-transparent",
      let semi = withAlpha 128 red
          blended = alphaBlend semi blue
       in assertTrue "semi blend" (colorR blended > 0 && colorB blended > 0)
    ),
    ( "alphaBlend transparent over transparent",
      assertEqual "both transparent" transparent (alphaBlend transparent transparent)
    ),
    ( "withAlpha preserves RGB",
      let halfAlpha = withAlpha 128 red
       in assertTrue "RGB preserved" (colorR halfAlpha == 255 && colorG halfAlpha == 0 && colorB halfAlpha == 0)
    ),
    ( "withAlpha 0",
      assertEqual "zero alpha" 0 (colorA (withAlpha 0 red))
    ),
    ( "withAlpha 255",
      assertEqual "full alpha" 255 (colorA (withAlpha 255 red))
    ),
    ( "scaleAlpha 0 gives 0 alpha",
      assertEqual "zero alpha" 0 (colorA (scaleAlpha 0.0 red))
    ),
    ( "scaleAlpha 1 preserves alpha",
      assertEqual "full alpha" 255 (colorA (scaleAlpha 1.0 red))
    ),
    ( "scaleAlpha 0.5 halves alpha",
      let scaled = scaleAlpha 0.5 red
       in assertTrue "half alpha" (colorA scaled >= 127 && colorA scaled <= 128)
    ),
    ( "scaleAlpha preserves RGB",
      let scaled = scaleAlpha 0.5 red
       in assertTrue "RGB preserved" (colorR scaled == 255 && colorG scaled == 0 && colorB scaled == 0)
    ),
    ( "scaleAlpha clamps negative",
      assertEqual "clamp neg" 0 (colorA (scaleAlpha (-1.0) red))
    )
  ]

testColorNamed :: [(String, TestResult)]
testColorNamed =
  [ ( "transparent is (0,0,0,0)",
      assertEqual "transparent" (Color 0 0 0 0) transparent
    ),
    ( "black is (0,0,0,255)",
      assertEqual "black" (Color 0 0 0 255) black
    ),
    ( "white is (255,255,255,255)",
      assertEqual "white" (Color 255 255 255 255) white
    ),
    ( "red RGB",
      assertTrue "red" (colorR red == 255 && colorG red == 0 && colorB red == 0)
    ),
    ( "green RGB",
      assertTrue "green" (colorR green == 0 && colorG green == 255 && colorB green == 0)
    ),
    ( "blue RGB",
      assertTrue "blue" (colorR blue == 0 && colorG blue == 0 && colorB blue == 255)
    ),
    ( "yellow RGB",
      assertTrue "yellow" (colorR yellow == 255 && colorG yellow == 255 && colorB yellow == 0)
    ),
    ( "cyan RGB",
      assertTrue "cyan" (colorR cyan == 0 && colorG cyan == 255 && colorB cyan == 255)
    ),
    ( "magenta RGB",
      assertTrue "magenta" (colorR magenta == 255 && colorG magenta == 0 && colorB magenta == 255)
    ),
    ( "orange has red and green",
      assertTrue "orange" (colorR orange == 255 && colorG orange > 0 && colorB orange == 0)
    ),
    ( "purple RGB",
      assertTrue "purple" (colorR purple == 128 && colorG purple == 0 && colorB purple == 128)
    ),
    ( "pink has red",
      assertTrue "pink" (colorR pink == 255 && colorA pink == 255)
    ),
    ( "gray is (128,128,128,255)",
      assertEqual "gray" (Color 128 128 128 255) gray
    ),
    ( "darkGray is (64,64,64,255)",
      assertEqual "darkGray" (Color 64 64 64 255) darkGray
    ),
    ( "lightGray is (192,192,192,255)",
      assertEqual "lightGray" (Color 192 192 192 255) lightGray
    ),
    ( "all named colors are opaque",
      assertTrue
        "opaque"
        ( all
            (\c -> colorA c == 255)
            [black, white, red, green, blue, yellow, cyan, magenta, orange, purple, pink, gray, darkGray, lightGray]
        )
    )
  ]

testColorInstances :: [(String, TestResult)]
testColorInstances =
  [ ( "Color Show roundtrip",
      assertTrue "show" (not (null (show red)))
    ),
    ( "Color Eq reflexive",
      assertTrue "eq refl" (red == red)
    ),
    ( "Color Eq different",
      assertTrue "eq diff" (red /= blue)
    ),
    ( "Color Ord works",
      assertTrue "ord" (black /= white)
    )
  ]

-- ---------------------------------------------------------------------------
-- Canvas tests
-- ---------------------------------------------------------------------------

testCanvas :: [(String, TestResult)]
testCanvas =
  [ ( "newCanvas dimensions correct",
      let c = newCanvas canvasSize canvasSize transparent
       in assertEqual "canvas size" (canvasSize, canvasSize) (cWidth c, cHeight c)
    ),
    ( "newCanvas pixel count correct",
      let c = newCanvas canvasSize canvasSize transparent
       in assertEqual "pixel bytes" (canvasSize * canvasSize * 4) (BS.length (cPixels c))
    ),
    ( "setPixel then getPixel roundtrip",
      let c = setPixel (newCanvas canvasSize canvasSize transparent) 5 5 red
       in assertEqual "pixel roundtrip" red (getPixel c 5 5)
    ),
    ( "getPixel out of bounds returns transparent",
      let c = newCanvas canvasSize canvasSize red
       in assertEqual "oob pixel" transparent (getPixel c (-1) 0)
    ),
    ( "setPixel out of bounds is no-op",
      let c = newCanvas canvasSize canvasSize transparent
          modified = setPixel c (-1) 0 red
       in assertEqual "oob setPixel" c modified
    ),
    ( "inBounds positive",
      assertTrue "in bounds" (inBounds (newCanvas canvasSize canvasSize transparent) 0 0)
    ),
    ( "inBounds negative",
      assertTrue "out of bounds" (not (inBounds (newCanvas canvasSize canvasSize transparent) canvasSize 0))
    ),
    ( "fillRect fills correct area",
      let c = fillRect (newCanvas canvasSize canvasSize transparent) 0 0 4 4 red
          filled = length [() | x <- [0 .. 3], y <- [0 .. 3], getPixel c x y == red]
       in assertEqual "fill count" 16 filled
    ),
    ( "drawLine endpoints correct",
      let c = drawLine (newCanvas canvasSize canvasSize transparent) 0 0 9 0 red
       in assertTrue "line start" (getPixel c 0 0 == red && getPixel c 9 0 == red)
    ),
    ( "drawRect perimeter only",
      let c = drawRect (newCanvas canvasSize canvasSize transparent) 0 0 canvasSize canvasSize red
          interior = getPixel c (canvasSize `div` 2) (canvasSize `div` 2)
       in assertEqual "interior transparent" transparent interior
    ),
    ( "fillCircle fills center",
      let c = fillCircle (newCanvas canvasSize canvasSize transparent) 5 5 3 red
       in assertEqual "circle center" red (getPixel c 5 5)
    ),
    ( "drawCircle doesn't fill center",
      let c = drawCircle (newCanvas 20 20 transparent) 10 10 5 red
       in assertEqual "circle center empty" transparent (getPixel c 10 10)
    )
  ]
  where
    canvasSize :: Int
    canvasSize = 16

testCanvasAdvanced :: [(String, TestResult)]
testCanvasAdvanced =
  [ ( "fromPixels creates correct canvas",
      let c = fromPixels 2 2 [red, green, blue, white]
       in assertEqual "fromPixels TL" red (getPixel c 0 0)
    ),
    ( "fromPixels TR pixel",
      let c = fromPixels 2 2 [red, green, blue, white]
       in assertEqual "fromPixels TR" green (getPixel c 1 0)
    ),
    ( "fromPixels BL pixel",
      let c = fromPixels 2 2 [red, green, blue, white]
       in assertEqual "fromPixels BL" blue (getPixel c 0 1)
    ),
    ( "fromPixels BR pixel",
      let c = fromPixels 2 2 [red, green, blue, white]
       in assertEqual "fromPixels BR" white (getPixel c 1 1)
    ),
    ( "fromPixels pads short list",
      let c = fromPixels 2 2 [red]
       in assertEqual "fromPixels pad" transparent (getPixel c 1 1)
    ),
    ( "clearCanvas replaces all pixels",
      let c = clearCanvas (newCanvas 4 4 red) blue
       in assertEqual "clear center" blue (getPixel c 2 2)
    ),
    ( "clearCanvas preserves dimensions",
      let c = clearCanvas (newCanvas 4 4 red) blue
       in assertEqual "clear dims" (4, 4) (cWidth c, cHeight c)
    ),
    ( "pixelIndex at origin is 0",
      assertEqual "pixelIndex 0,0" 0 (pixelIndex 8 0 0)
    ),
    ( "pixelIndex at (1,0) is 4",
      assertEqual "pixelIndex 1,0" 4 (pixelIndex 8 1 0)
    ),
    ( "pixelIndex at (0,1) for width 8 is 32",
      assertEqual "pixelIndex 0,1" 32 (pixelIndex 8 0 1)
    ),
    ( "hLine draws horizontal",
      let c = hLine (newCanvas 8 8 transparent) 0 7 4 red
       in assertTrue "hLine" (getPixel c 0 4 == red && getPixel c 7 4 == red)
    ),
    ( "hLine reversed args",
      let c = hLine (newCanvas 8 8 transparent) 7 0 4 red
       in assertTrue "hLine rev" (getPixel c 0 4 == red && getPixel c 7 4 == red)
    ),
    ( "floodFill fills connected region",
      let c = fillRect (newCanvas 8 8 transparent) 0 0 8 8 red
          filled = floodFill c 4 4 blue
       in assertEqual "flood center" blue (getPixel filled 4 4)
    ),
    ( "floodFill doesn't cross boundary",
      let base = newCanvas 8 8 transparent
          withBorder = fillRect base 0 0 8 1 red
          withRegion = fillRect withBorder 0 2 8 6 green
          filled = floodFill withRegion 4 4 blue
       in assertEqual "flood above border" red (getPixel filled 4 0)
    ),
    ( "floodFill no-op on same color",
      let c = newCanvas 4 4 red
       in assertEqual "flood same" c (floodFill c 2 2 red)
    ),
    ( "floodFill out of bounds is no-op",
      let c = newCanvas 4 4 red
       in assertEqual "flood oob" c (floodFill c (-1) (-1) blue)
    ),
    ( "getPixel bottom-right corner",
      let c = setPixel (newCanvas 4 4 transparent) 3 3 red
       in assertEqual "BR corner" red (getPixel c 3 3)
    ),
    ( "getPixel just OOB right",
      assertEqual "oob right" transparent (getPixel (newCanvas 4 4 red) 4 0)
    ),
    ( "getPixel just OOB bottom",
      assertEqual "oob bottom" transparent (getPixel (newCanvas 4 4 red) 0 4)
    ),
    ( "inBounds (w-1, h-1) is True",
      assertTrue "in bounds corner" (inBounds (newCanvas 4 4 transparent) 3 3)
    ),
    ( "inBounds negative x",
      assertTrue "oob neg x" (not (inBounds (newCanvas 4 4 transparent) (-1) 0))
    ),
    ( "inBounds negative y",
      assertTrue "oob neg y" (not (inBounds (newCanvas 4 4 transparent) 0 (-1)))
    ),
    ( "drawLine vertical",
      let c = drawLine (newCanvas 8 8 transparent) 4 0 4 7 red
       in assertTrue "vline" (getPixel c 4 0 == red && getPixel c 4 7 == red)
    ),
    ( "drawLine diagonal",
      let c = drawLine (newCanvas 8 8 transparent) 0 0 7 7 red
       in assertTrue "diag line" (getPixel c 0 0 == red && getPixel c 7 7 == red)
    ),
    ( "drawRect zero width is no-op",
      let c = newCanvas 4 4 transparent
       in assertEqual "rect zero w" c (drawRect c 0 0 0 4 red)
    ),
    ( "fillRect zero height is no-op",
      let c = newCanvas 4 4 transparent
       in assertEqual "fill zero h" c (fillRect c 0 0 4 0 red)
    ),
    ( "fillCircle radius 0 sets single pixel",
      let c = fillCircle (newCanvas 8 8 transparent) 4 4 0 red
       in assertEqual "fill circle r0" red (getPixel c 4 4)
    ),
    ( "drawCircle radius 0 sets single pixel",
      let c = drawCircle (newCanvas 8 8 transparent) 4 4 0 red
       in assertEqual "draw circle r0" red (getPixel c 4 4)
    ),
    ( "Canvas Show works",
      assertTrue "canvas show" (not (null (show (newCanvas 1 1 red))))
    ),
    ( "Canvas Eq works",
      assertTrue "canvas eq" (newCanvas 2 2 red == newCanvas 2 2 red)
    ),
    ( "Canvas Eq different",
      assertTrue "canvas neq" (newCanvas 2 2 red /= newCanvas 2 2 blue)
    )
  ]

-- ---------------------------------------------------------------------------
-- Transform tests
-- ---------------------------------------------------------------------------

testTransform :: [(String, TestResult)]
testTransform =
  [ ( "flipH twice = identity",
      let c = setPixel (newCanvas 8 8 transparent) 0 0 red
       in assertEqual "flipH identity" (getPixel c 0 0) (getPixel (flipH (flipH c)) 0 0)
    ),
    ( "flipH moves pixel to opposite side",
      let c = setPixel (newCanvas 8 8 transparent) 0 0 red
          flipped = flipH c
       in assertEqual "flipH position" red (getPixel flipped 7 0)
    ),
    ( "flipV twice = identity",
      let c = setPixel (newCanvas 8 8 transparent) 0 0 red
       in assertEqual "flipV identity" (getPixel c 0 0) (getPixel (flipV (flipV c)) 0 0)
    ),
    ( "flipV moves pixel to bottom",
      let c = setPixel (newCanvas 8 8 transparent) 0 0 red
          flipped = flipV c
       in assertEqual "flipV position" red (getPixel flipped 0 7)
    ),
    ( "rotate180 twice = identity",
      let c = setPixel (newCanvas 8 8 transparent) 0 0 red
          rotated = rotate180 (rotate180 c)
       in assertEqual "rotate180 identity" red (getPixel rotated 0 0)
    ),
    ( "rotate90 swaps dimensions",
      let c = newCanvas 4 8 transparent
          rotated = rotate90 c
       in assertEqual "rotate90 dims" (8, 4) (cWidth rotated, cHeight rotated)
    ),
    ( "rotate270 is inverse of rotate90",
      let c = setPixel (newCanvas 8 8 transparent) 1 0 red
          roundtrip = rotate270 (rotate90 c)
       in assertEqual "rotate round trip" red (getPixel roundtrip 1 0)
    ),
    ( "scaleNearest doubles dimensions",
      let c = newCanvas 4 4 transparent
          scaled = scaleNearest 2 c
       in assertEqual "scaled dims" (8, 8) (cWidth scaled, cHeight scaled)
    ),
    ( "scaleNearest preserves corner pixel",
      let c = setPixel (newCanvas 4 4 transparent) 0 0 red
          scaled = scaleNearest 2 c
       in assertEqual "scaled corner" red (getPixel scaled 0 0)
    )
  ]

testTransformEffects :: [(String, TestResult)]
testTransformEffects =
  [ ( "rotate90 moves TL to TR",
      let c = setPixel (newCanvas 4 4 transparent) 0 0 red
          rotated = rotate90 c
       in assertEqual "rot90 TL" red (getPixel rotated 3 0)
    ),
    ( "rotate180 moves TL to BR",
      let c = setPixel (newCanvas 4 4 transparent) 0 0 red
          rotated = rotate180 c
       in assertEqual "rot180 TL" red (getPixel rotated 3 3)
    ),
    ( "rotate270 moves TL to BL",
      let c = setPixel (newCanvas 4 4 transparent) 0 0 red
          rotated = rotate270 c
       in assertEqual "rot270 TL" red (getPixel rotated 0 3)
    ),
    ( "four rotate90 = identity",
      let c = setPixel (newCanvas 4 4 transparent) 1 2 red
          rotated = rotate90 (rotate90 (rotate90 (rotate90 c)))
       in assertEqual "4x rot90" red (getPixel rotated 1 2)
    ),
    ( "flipH then flipV = rotate180",
      let c = setPixel (newCanvas 4 4 transparent) 1 0 red
          via = flipV (flipH c)
          rot = rotate180 c
       in assertEqual "flipH+flipV" (getPixel via 2 3) (getPixel rot 2 3)
    ),
    ( "scaleNearest 1 = identity",
      let c = newCanvas 4 4 red
       in assertEqual "scale 1" c (scaleNearest 1 c)
    ),
    ( "scaleNearest 3 triples dims",
      let c = newCanvas 2 2 red
          scaled = scaleNearest 3 c
       in assertEqual "scale 3" (6, 6) (cWidth scaled, cHeight scaled)
    ),
    ( "scaleNearest 2 fills 2x2 block",
      let c = setPixel (newCanvas 2 2 transparent) 0 0 red
          scaled = scaleNearest 2 c
       in assertTrue "scale block" (getPixel scaled 0 0 == red && getPixel scaled 1 1 == red)
    ),
    ( "outline adds border pixels",
      let c = setPixel (newCanvas 8 8 transparent) 4 4 red
          outlined = outline blue c
       in assertTrue "outline exists" (getPixel outlined 3 4 == blue || getPixel outlined 5 4 == blue)
    ),
    ( "outline preserves original pixel",
      let c = setPixel (newCanvas 8 8 transparent) 4 4 red
          outlined = outline blue c
       in assertEqual "outline center" red (getPixel outlined 4 4)
    ),
    ( "outline preserves dimensions",
      let c = newCanvas 8 8 transparent
          outlined = outline red c
       in assertEqual "outline dims" (8, 8) (cWidth outlined, cHeight outlined)
    ),
    ( "dropShadow increases canvas size",
      let c = newCanvas 4 4 red
          shadowed = dropShadow 2 2 (Color 0 0 0 128) c
       in assertTrue "shadow bigger" (cWidth shadowed > 4 && cHeight shadowed > 4)
    ),
    ( "dropShadow offset dimensions",
      let c = newCanvas 4 4 red
          shadowed = dropShadow 2 3 black c
       in assertEqual "shadow dims" (6, 7) (cWidth shadowed, cHeight shadowed)
    ),
    ( "dropShadow negative offset",
      let c = newCanvas 4 4 red
          shadowed = dropShadow (-2) (-2) black c
       in assertEqual "shadow neg" (6, 6) (cWidth shadowed, cHeight shadowed)
    )
  ]

-- ---------------------------------------------------------------------------
-- Compose tests
-- ---------------------------------------------------------------------------

testCompose :: [(String, TestResult)]
testCompose =
  [ ( "stamp copies non-transparent pixels",
      let dst = newCanvas 8 8 blue
          src = setPixel (newCanvas 4 4 transparent) 0 0 red
          result = stamp dst 2 2 src
       in assertEqual "stamp pixel" red (getPixel result 2 2)
    ),
    ( "stamp preserves transparent pixels",
      let dst = newCanvas 8 8 blue
          src = newCanvas 4 4 transparent
          result = stamp dst 0 0 src
       in assertEqual "stamp transparent" blue (getPixel result 0 0)
    ),
    ( "overlay last drawn on top",
      let bottom = newCanvas 8 8 blue
          top = newCanvas 8 8 red
          result = overlay bottom top
       in assertEqual "overlay top" red (getPixel result 0 0)
    )
  ]

testComposeAdvanced :: [(String, TestResult)]
testComposeAdvanced =
  [ ( "stampAlpha blends semi-transparent",
      let dst = newCanvas 8 8 blue
          src = newCanvas 4 4 (withAlpha 128 red)
          result = stampAlpha dst 0 0 src
       in assertTrue "stamp alpha blend" (colorR (getPixel result 0 0) > 0 && colorB (getPixel result 0 0) > 0)
    ),
    ( "stampAlpha preserves outside region",
      let dst = newCanvas 8 8 blue
          src = newCanvas 4 4 red
          result = stampAlpha dst 0 0 src
       in assertEqual "stamp alpha outside" blue (getPixel result 7 7)
    ),
    ( "overlayAt offset works",
      let dst = newCanvas 8 8 blue
          src = newCanvas 4 4 red
          result = overlayAt dst 2 2 src
       in assertEqual "overlayAt offset" red (getPixel result 2 2)
    ),
    ( "overlayAt preserves outside",
      let dst = newCanvas 8 8 blue
          src = newCanvas 2 2 red
          result = overlayAt dst 6 6 src
       in assertEqual "overlayAt outside" blue (getPixel result 0 0)
    ),
    ( "stamp at negative offset partial",
      let dst = newCanvas 8 8 blue
          src = newCanvas 4 4 red
          result = stamp dst (-2) (-2) src
       in assertEqual "stamp neg offset" red (getPixel result 0 0)
    ),
    ( "stamp at far offset no effect",
      let dst = newCanvas 4 4 blue
          src = newCanvas 2 2 red
          result = stamp dst 10 10 src
       in assertEqual "stamp far" blue (getPixel result 0 0)
    ),
    ( "overlay transparent on top preserves bottom",
      let bottom = newCanvas 4 4 red
          top = newCanvas 4 4 transparent
          result = overlay bottom top
       in assertEqual "overlay transparent" red (getPixel result 0 0)
    )
  ]

-- ---------------------------------------------------------------------------
-- Animation tests
-- ---------------------------------------------------------------------------

testAnimation :: [(String, TestResult)]
testAnimation =
  [ ( "loop animation wraps around",
      let anim = loopAnimation 1 4
       in assertEqual "loop wrap" 0 (animationFrame anim 4)
    ),
    ( "once animation clamps to last",
      let anim = onceAnimation 1 4
       in assertEqual "once clamp" 3 (animationFrame anim 100)
    ),
    ( "ping-pong bounces",
      let anim = pingPongAnimation 1 4
       in -- frames: 0,1,2,3,2,1,0,1,2,3,...
          -- cycle len = 6 (4*2-2)
          assertEqual "ping-pong frame 4" 2 (animationFrame anim 4)
    ),
    ( "animationDone for once",
      assertTrue "done" (animationDone (onceAnimation 1 4) 3)
    ),
    ( "animationDone false for loop",
      assertTrue "not done" (not (animationDone (loopAnimation 1 4) 100))
    )
  ]

testAnimationAdvanced :: [(String, TestResult)]
testAnimationAdvanced =
  [ ( "animation constructor",
      let anim = animation 2 8 Loop
       in assertEqual "anim delay" 2 (animFrameDelay anim)
    ),
    ( "animation frame count field",
      let anim = animation 1 5 Once
       in assertEqual "anim count" 5 (animFrameCount anim)
    ),
    ( "animation loop mode field",
      let anim = animation 1 3 PingPong
       in assertEqual "anim mode" PingPong (animLoopMode anim)
    ),
    ( "loop delay=2 at tick=3",
      let anim = loopAnimation 2 4
       in assertEqual "loop delay" 1 (animationFrame anim 3)
    ),
    ( "once at tick=0",
      assertEqual "once t0" 0 (animationFrame (onceAnimation 1 4) 0)
    ),
    ( "pingpong at tick=5",
      -- cycle=6, pos=5, count=4: 6-5=1
      assertEqual "pp t5" 1 (animationFrame (pingPongAnimation 1 4) 5)
    ),
    ( "animationDone once not done at tick=0",
      assertTrue "not done t0" (not (animationDone (onceAnimation 1 4) 0))
    ),
    ( "animationDone ping-pong always false",
      assertTrue "pp not done" (not (animationDone (pingPongAnimation 1 4) 100))
    ),
    ( "zero frame count returns 0",
      assertEqual "zero frames" 0 (animationFrame (loopAnimation 1 0) 5)
    ),
    ( "zero delay returns 0",
      assertEqual "zero delay" 0 (animationFrame (loopAnimation 0 4) 5)
    ),
    ( "Animation Show works",
      assertTrue "anim show" (not (null (show (loopAnimation 1 4))))
    ),
    ( "Animation Eq works",
      assertTrue "anim eq" (loopAnimation 1 4 == loopAnimation 1 4)
    ),
    ( "LoopMode Show works",
      assertTrue "loop show" (not (null (show Loop)))
    ),
    ( "LoopMode Eq works",
      assertTrue "loop eq" (Loop == Loop && Loop /= Once)
    )
  ]

-- ---------------------------------------------------------------------------
-- Palette tests
-- ---------------------------------------------------------------------------

testPalette :: [(String, TestResult)]
testPalette =
  [ ( "paletteColor returns correct color",
      let pal = fromColors [red, green, blue]
       in assertEqual "pal index 1" green (paletteColor pal 1)
    ),
    ( "paletteColor clamps high",
      let pal = fromColors [red, green, blue]
       in assertEqual "pal clamp high" blue (paletteColor pal 100)
    ),
    ( "paletteSwap replaces matching color",
      let src = fromColors [red, green]
          dst = fromColors [blue, white]
       in assertEqual "swap red->blue" blue (paletteSwap src dst red)
    ),
    ( "paletteSwap preserves non-matching",
      let src = fromColors [red]
          dst = fromColors [blue]
       in assertEqual "no swap" green (paletteSwap src dst green)
    ),
    ( "gameboy palette has 4 colors",
      assertEqual "gameboy count" 4 (length (paletteColors gameboy))
    )
  ]

testPaletteBuiltins :: [(String, TestResult)]
testPaletteBuiltins =
  [ ( "grayscale4 has 4 colors",
      assertEqual "gs4 count" 4 (length (paletteColors grayscale4))
    ),
    ( "grayscale4 colors are true grayscale",
      assertTrue
        "gs4 grayscale"
        (all (\(Color r g b _) -> r == g && g == b) (paletteColors grayscale4))
    ),
    ( "gameboy colors are green-tinted",
      assertTrue
        "gameboy green"
        (all (\(Color r g _ _) -> g >= r) (paletteColors gameboy))
    ),
    ( "grayscale8 has 8 colors",
      assertEqual "gs8 count" 8 (length (paletteColors grayscale8))
    ),
    ( "nes has 16 colors",
      assertEqual "nes count" 16 (length (paletteColors nes))
    ),
    ( "paletteColor index 0",
      let pal = fromColors [red, green, blue]
       in assertEqual "pal idx 0" red (paletteColor pal 0)
    ),
    ( "paletteColor negative index",
      let pal = fromColors [red, green, blue]
       in assertEqual "pal neg" red (paletteColor pal (-1))
    ),
    ( "paletteColor empty palette",
      assertEqual "pal empty" transparent (paletteColor (fromColors []) 0)
    ),
    ( "paletteSwap second element",
      let src = fromColors [red, green]
          dst = fromColors [blue, white]
       in assertEqual "swap green->white" white (paletteSwap src dst green)
    ),
    ( "paletteSwap empty src",
      let src = fromColors []
          dst = fromColors [blue]
       in assertEqual "swap empty" red (paletteSwap src dst red)
    ),
    ( "Palette Show works",
      assertTrue "palette show" (not (null (show gameboy)))
    ),
    ( "Palette Eq works",
      assertTrue "palette eq" (gameboy == gameboy)
    ),
    ( "all gameboy colors are opaque",
      assertTrue "gb opaque" (all (\c -> colorA c == 255) (paletteColors gameboy))
    ),
    ( "all grayscale4 colors are opaque",
      assertTrue "gs4 opaque" (all (\c -> colorA c == 255) (paletteColors grayscale4))
    ),
    ( "all grayscale8 colors are opaque",
      assertTrue "gs8 opaque" (all (\c -> colorA c == 255) (paletteColors grayscale8))
    ),
    ( "grayscale8 colors are true grayscale",
      assertTrue
        "gs8 grayscale"
        (all (\(Color r g b _) -> r == g && g == b) (paletteColors grayscale8))
    ),
    ( "all nes colors are opaque",
      assertTrue "nes opaque" (all (\c -> colorA c == 255) (paletteColors nes))
    ),
    ( "nes first color is black",
      assertEqual "nes black" (Color 0 0 0 255) (paletteColor nes 0)
    ),
    ( "nes second color is white",
      assertEqual "nes white" (Color 252 252 252 255) (paletteColor nes 1)
    ),
    ( "grayscale8 endpoints are black and white",
      assertTrue
        "gs8 endpoints"
        (paletteColor grayscale8 0 == Color 0 0 0 255 && paletteColor grayscale8 7 == Color 255 255 255 255)
    )
  ]

-- ---------------------------------------------------------------------------
-- Sprite tests
-- ---------------------------------------------------------------------------

testSprite :: [(String, TestResult)]
testSprite =
  [ ( "singleFrame creates 1-frame sprite",
      let s = singleFrame "test" (newCanvas 16 16 red)
       in assertEqual "frame count" 1 (frameCount s)
    ),
    ( "spriteWidth matches canvas",
      let s = singleFrame "test" (newCanvas 32 24 red)
       in assertEqual "sprite width" 32 (spriteWidth s)
    ),
    ( "spriteHeight matches canvas",
      let s = singleFrame "test" (newCanvas 32 24 red)
       in assertEqual "sprite height" 24 (spriteHeight s)
    ),
    ( "getFrame returns correct frame",
      let canvas = newCanvas 8 8 red
          s = singleFrame "test" canvas
       in case getFrame s 0 of
            Just f -> assertEqual "frame match" canvas f
            Nothing -> Left "getFrame returned Nothing"
    ),
    ( "getFrame out of range returns Nothing",
      let s = singleFrame "test" (newCanvas 8 8 red)
       in case getFrame s 5 of
            Just _ -> Left "expected Nothing"
            Nothing -> Right ()
    )
  ]

testSpriteAdvanced :: [(String, TestResult)]
testSpriteAdvanced =
  [ ( "multiFrame creates multi-frame sprite",
      let frames = [newCanvas 8 8 red, newCanvas 8 8 green, newCanvas 8 8 blue]
          s = multiFrame "multi" 2 3 frames
       in assertEqual "multi frame count" 3 (frameCount s)
    ),
    ( "multiFrame origin",
      let s = multiFrame "test" 5 10 [newCanvas 8 8 red]
       in assertTrue "origin" (spriteOriginX s == 5 && spriteOriginY s == 10)
    ),
    ( "multiFrame getFrame 1",
      let greenCanvas = newCanvas 8 8 green
          frames = [newCanvas 8 8 red, greenCanvas]
          s = multiFrame "test" 0 0 frames
       in case getFrame s 1 of
            Just f -> assertEqual "frame 1" greenCanvas f
            Nothing -> Left "expected frame 1"
    ),
    ( "getFrame negative index returns Nothing",
      let s = singleFrame "test" (newCanvas 8 8 red)
       in case getFrame s (-1) of
            Just _ -> Left "expected Nothing"
            Nothing -> Right ()
    ),
    ( "singleFrame name",
      assertEqual "name" "test" (spriteName (singleFrame "test" (newCanvas 1 1 red)))
    ),
    ( "singleFrame origin is (0,0)",
      let s = singleFrame "test" (newCanvas 1 1 red)
       in assertTrue "origin zero" (spriteOriginX s == 0 && spriteOriginY s == 0)
    ),
    ( "singleFrame no bounds",
      assertEqual "no bounds" Nothing (spriteBounds (singleFrame "test" (newCanvas 1 1 red)))
    ),
    ( "multiFrame empty frames",
      let s = multiFrame "empty" 0 0 []
       in assertEqual "empty frames" 0 (frameCount s)
    ),
    ( "spriteWidth empty is 0",
      assertEqual "empty width" 0 (spriteWidth (multiFrame "e" 0 0 []))
    ),
    ( "spriteHeight empty is 0",
      assertEqual "empty height" 0 (spriteHeight (multiFrame "e" 0 0 []))
    ),
    ( "BoundingBox Show works",
      assertTrue "bb show" (not (null (show (BoundingBox 0 0 8 8))))
    ),
    ( "BoundingBox Eq works",
      assertTrue "bb eq" (BoundingBox 0 0 8 8 == BoundingBox 0 0 8 8)
    ),
    ( "Sprite Show works",
      assertTrue "sprite show" (not (null (show (singleFrame "s" (newCanvas 1 1 red)))))
    ),
    ( "Sprite Eq works",
      let s = singleFrame "s" (newCanvas 1 1 red)
       in assertTrue "sprite eq" (s == s)
    )
  ]

-- ---------------------------------------------------------------------------
-- Sheet tests
-- ---------------------------------------------------------------------------

testSheet :: [(String, TestResult)]
testSheet =
  [ ( "packSheet fits all entries",
      let items = [("a", newCanvas 8 8 red), ("b", newCanvas 8 8 green), ("c", newCanvas 8 8 blue)]
          sheet = packSheet 0 items
       in assertEqual "entry count" 3 (length (sheetEntries sheet))
    ),
    ( "packSheet no overlap",
      let items = [("a", newCanvas 8 8 red), ("b", newCanvas 8 8 green)]
          sheet = packSheet 0 items
          entries = sheetEntries sheet
          noOverlap = case entries of
            [e1, e2] ->
              entryX e1 + entryWidth e1 <= entryX e2
                || entryX e2 + entryWidth e2 <= entryX e1
                || entryY e1 + entryHeight e1 <= entryY e2
                || entryY e2 + entryHeight e2 <= entryY e1
            _ -> True
       in assertTrue "no overlap" noOverlap
    ),
    ( "packSheet atlas dimensions positive",
      let items = [("a", newCanvas 8 8 red)]
          sheet = packSheet 0 items
       in assertTrue "positive dims" (cWidth (sheetCanvas sheet) > 0 && cHeight (sheetCanvas sheet) > 0)
    )
  ]

testSheetAdvanced :: [(String, TestResult)]
testSheetAdvanced =
  [ ( "packSheet preserves pixel data",
      let items = [("a", newCanvas 4 4 red)]
          sheet = packSheet 0 items
          entry = case sheetEntries sheet of
            (e : _) -> e
            [] -> SheetEntry "" 0 0 0 0
       in assertEqual "sheet pixel" red (getPixel (sheetCanvas sheet) (entryX entry) (entryY entry))
    ),
    ( "packSheet with padding",
      let items = [("a", newCanvas 4 4 red), ("b", newCanvas 4 4 green)]
          sheet = packSheet 2 items
       in assertTrue "padded positive" (cWidth (sheetCanvas sheet) > 0 && cHeight (sheetCanvas sheet) > 0)
    ),
    ( "packSheet single item",
      let sheet = packSheet 0 [("single", newCanvas 8 8 red)]
       in assertEqual "single entry" 1 (length (sheetEntries sheet))
    ),
    ( "packSheet empty list",
      let sheet = packSheet 0 []
       in assertEqual "empty entries" 0 (length (sheetEntries sheet))
    ),
    ( "packSheet entry names preserved",
      let items = [("alpha", newCanvas 4 4 red), ("beta", newCanvas 4 4 green)]
          sheet = packSheet 0 items
          names = map entryName (sheetEntries sheet)
       in assertTrue "names" ("alpha" `elem` names && "beta" `elem` names)
    ),
    ( "packSheet entry dimensions correct",
      let items = [("a", newCanvas 5 7 red)]
          sheet = packSheet 0 items
       in case sheetEntries sheet of
            (e : _) -> assertTrue "entry dims" (entryWidth e == 5 && entryHeight e == 7)
            [] -> Left "no entries"
    ),
    ( "SheetEntry Show works",
      assertTrue "entry show" (not (null (show (SheetEntry "x" 0 0 1 1))))
    ),
    ( "SpriteSheet Eq works",
      let s = packSheet 0 [("a", newCanvas 4 4 red)]
       in assertTrue "sheet eq" (s == s)
    ),
    ( "packSheet mixed sizes",
      let items = [("small", newCanvas 4 4 red), ("big", newCanvas 16 16 green), ("med", newCanvas 8 8 blue)]
          sheet = packSheet 0 items
       in assertEqual "mixed count" 3 (length (sheetEntries sheet))
    )
  ]

-- ---------------------------------------------------------------------------
-- Text tests
-- ---------------------------------------------------------------------------

testText :: [(String, TestResult)]
testText =
  [ ( "textWidth matches character count",
      assertEqual "text width" 40 (textWidth defaultFont "Hello")
    ),
    ( "renderText produces correct width canvas",
      let canvas = renderText defaultFont white "Hi"
       in assertEqual "rendered width" 16 (cWidth canvas)
    ),
    ( "renderText produces correct height canvas",
      let canvas = renderText defaultFont white "Hi"
       in assertEqual "rendered height" 8 (cHeight canvas)
    )
  ]

testTextAdvanced :: [(String, TestResult)]
testTextAdvanced =
  [ ( "renderChar dimensions",
      let c = renderChar defaultFont white 'A'
       in assertEqual "char dims" (8, 8) (cWidth c, cHeight c)
    ),
    ( "renderChar space is mostly empty",
      let c = renderChar defaultFont white ' '
          anyPixel = any (\(x, y) -> getPixel c x y /= transparent) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "space empty" (not anyPixel)
    ),
    ( "renderChar 'A' has pixels",
      let c = renderChar defaultFont white 'A'
          anyPixel = any (\(x, y) -> getPixel c x y == white) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "A has pixels" anyPixel
    ),
    ( "renderChar unknown fills border",
      let c = renderChar defaultFont white '\128'
          tl = getPixel c 0 0
       in assertEqual "unknown TL" white tl
    ),
    ( "textWidth empty string",
      assertEqual "empty width" 0 (textWidth defaultFont "")
    ),
    ( "textWidth single char",
      assertEqual "single width" 8 (textWidth defaultFont "X")
    ),
    ( "textHeight",
      assertEqual "text height" 8 (textHeight defaultFont)
    ),
    ( "renderText empty string",
      let c = renderText defaultFont white ""
       in assertEqual "empty text width" 0 (cWidth c)
    ),
    ( "renderText single char matches renderChar",
      let textC = renderText defaultFont white "A"
          charC = renderChar defaultFont white 'A'
       in assertEqual "single char text" (getPixel charC 3 3) (getPixel textC 3 3)
    ),
    ( "defaultFont width is 8",
      assertEqual "font width" 8 (fontWidth defaultFont)
    ),
    ( "defaultFont height is 8",
      assertEqual "font height" 8 (fontHeight defaultFont)
    ),
    ( "Font Show works",
      assertTrue "font show" (not (null (show defaultFont)))
    ),
    ( "Font Eq works",
      assertTrue "font eq" (defaultFont == defaultFont)
    ),
    ( "renderChar different chars differ",
      let ca = renderChar defaultFont white 'A'
          cb = renderChar defaultFont white 'B'
       in assertTrue "diff chars" (ca /= cb)
    ),
    ( "renderText all printable ASCII",
      let text = [' ' .. '~']
          c = renderText defaultFont white text
       in assertEqual "printable width" (95 * 8) (cWidth c)
    )
  ]

-- ---------------------------------------------------------------------------
-- VFX tests
-- ---------------------------------------------------------------------------

testVFX :: [(String, TestResult)]
testVFX =
  [ ( "explosion frame count matches config",
      let config = ExplosionConfig 32 8 10 red 42
       in assertEqual "explosion frames" 8 (length (explosionFrames config))
    ),
    ( "explosion frame dimensions match config",
      let config = ExplosionConfig 32 8 10 red 42
          frames = explosionFrames config
       in assertTrue "explosion dims" (all (\c -> cWidth c == 32 && cHeight c == 32) frames)
    ),
    ( "ring frame count matches config",
      let config = RingConfig 32 6 red 2
       in assertEqual "ring frames" 6 (length (ringExpandFrames config))
    ),
    ( "flash frames count correct",
      assertEqual "flash count" 5 (length (flashFrames 5 white))
    ),
    ( "flash frames have expected dimensions",
      assertTrue "flash dims" (all (\c -> cWidth c == 32 && cHeight c == 32) (flashFrames 3 red))
    )
  ]

testVFXGlow :: [(String, TestResult)]
testVFXGlow =
  [ ( "glow frame count",
      let config = GlowConfig 32 8 yellow
       in assertEqual "glow count" 8 (length (glowPulseFrames config))
    ),
    ( "glow frame dimensions",
      let config = GlowConfig 32 8 yellow
          frames = glowPulseFrames config
       in assertTrue "glow dims" (all (\c -> cWidth c == 32 && cHeight c == 32) frames)
    ),
    ( "glow different configs produce different output",
      let config1 = GlowConfig 32 8 red
          config2 = GlowConfig 32 8 blue
       in assertTrue "glow diff" (glowPulseFrames config1 /= glowPulseFrames config2)
    ),
    ( "glow single frame",
      let config = GlowConfig 16 1 green
       in assertEqual "glow 1" 1 (length (glowPulseFrames config))
    ),
    ( "GlowConfig Show works",
      assertTrue "glow show" (not (null (show (GlowConfig 16 4 red))))
    ),
    ( "GlowConfig Eq works",
      assertTrue "glow eq" (GlowConfig 16 4 red == GlowConfig 16 4 red)
    )
  ]

testVFXTrail :: [(String, TestResult)]
testVFXTrail =
  [ ( "trail frame count",
      let config = TrailConfig 64 16 8 green 2
       in assertEqual "trail count" 8 (length (trailFrames config))
    ),
    ( "trail frame dimensions",
      let config = TrailConfig 64 16 8 green 2
          frames = trailFrames config
       in assertTrue "trail dims" (all (\c -> cWidth c == 64 && cHeight c == 16) frames)
    ),
    ( "trail single frame",
      let config = TrailConfig 32 16 1 red 1
       in assertEqual "trail 1" 1 (length (trailFrames config))
    ),
    ( "TrailConfig Show works",
      assertTrue "trail show" (not (null (show (TrailConfig 32 16 4 red 2))))
    ),
    ( "TrailConfig Eq works",
      assertTrue "trail eq" (TrailConfig 32 16 4 red 2 == TrailConfig 32 16 4 red 2)
    )
  ]

testVFXSparks :: [(String, TestResult)]
testVFXSparks =
  [ ( "sparks frame count",
      let config = SparksConfig 32 6 8 yellow 99
       in assertEqual "sparks count" 6 (length (sparksFrames config))
    ),
    ( "sparks frame dimensions",
      let config = SparksConfig 32 6 8 yellow 99
          frames = sparksFrames config
       in assertTrue "sparks dims" (all (\c -> cWidth c == 32 && cHeight c == 32) frames)
    ),
    ( "sparks different seeds differ",
      let config1 = SparksConfig 32 4 5 red 1
          config2 = SparksConfig 32 4 5 red 2
       in assertTrue "sparks seeds" (sparksFrames config1 /= sparksFrames config2)
    ),
    ( "SparksConfig Show works",
      assertTrue "sparks show" (not (null (show (SparksConfig 16 4 3 red 0))))
    ),
    ( "SparksConfig Eq works",
      assertTrue "sparks eq" (SparksConfig 16 4 3 red 0 == SparksConfig 16 4 3 red 0)
    ),
    ( "explosion different seeds differ",
      let config1 = ExplosionConfig 32 4 5 red 1
          config2 = ExplosionConfig 32 4 5 red 2
       in assertTrue "exp seeds" (explosionFrames config1 /= explosionFrames config2)
    ),
    ( "explosion deterministic",
      let config = ExplosionConfig 32 4 5 red 42
       in assertEqual "exp determ" (explosionFrames config) (explosionFrames config)
    ),
    ( "ring frame dimensions",
      let config = RingConfig 32 6 red 2
          frames = ringExpandFrames config
       in assertTrue "ring dims" (all (\c -> cWidth c == 32 && cHeight c == 32) frames)
    ),
    ( "ring deterministic",
      let config = RingConfig 16 4 blue 1
       in assertEqual "ring determ" (ringExpandFrames config) (ringExpandFrames config)
    ),
    ( "flash 1 frame",
      assertEqual "flash 1" 1 (length (flashFrames 1 red))
    ),
    ( "ExplosionConfig Show works",
      assertTrue "exp show" (not (null (show (ExplosionConfig 16 4 3 red 0))))
    ),
    ( "RingConfig Show works",
      assertTrue "ring show" (not (null (show (RingConfig 16 4 red 1))))
    ),
    ( "RingConfig Eq works",
      assertTrue "ring eq" (RingConfig 16 4 red 1 == RingConfig 16 4 red 1)
    ),
    ( "ExplosionConfig Eq works",
      assertTrue "exp eq" (ExplosionConfig 16 4 3 red 0 == ExplosionConfig 16 4 3 red 0)
    )
  ]

-- ---------------------------------------------------------------------------
-- BMP tests
-- ---------------------------------------------------------------------------

testBmpRoundtrip :: IO [(String, TestResult)]
testBmpRoundtrip = do
  let canvas = fillRect (newCanvas 4 4 transparent) 0 0 4 4 red
  (path, tmpHandle) <- openTempFile "." "gb-sprite-test.bmp"
  hClose tmpHandle
  writeBmp path canvas
  raw <- BS.readFile path
  removeFile path
  let bytes = BS.unpack raw
      safeIdx xs idx
        | idx < length xs = xs `seq` (xs !! idx) -- guarded by length check
        | otherwise = 0
  return
    [ ( "BMP starts with BM magic",
        assertEqual "BM magic" [0x42, 0x4D] (take 2 bytes)
      ),
      ( "BMP file size matches",
        let fileSize = fromLE32 (take 4 (drop 2 bytes))
            expected = 54 + 4 * 4 * 4 -- header + 4x4 pixels x 4 bytes
         in assertEqual "file size" expected fileSize
      ),
      ( "BMP pixel data offset is 54",
        let offset = fromLE32 (take 4 (drop 10 bytes))
         in assertEqual "pixel offset" 54 offset
      ),
      ( "BMP width is 4",
        let w = fromLE32 (take 4 (drop 18 bytes))
         in assertEqual "width" 4 w
      ),
      ( "BMP height is 4",
        let height = fromLE32 (take 4 (drop 22 bytes))
         in assertEqual "height" 4 height
      ),
      ( "BMP bits per pixel is 32",
        let bpp = fromLE16 (take 2 (drop 28 bytes))
         in assertEqual "bpp" 32 bpp
      ),
      ( "BMP encodes pixel data as BGRA",
        -- First pixel (bottom-left in BMP) should be red = BGRA(0,0,255,255)
        let pixelStart = 54
            b = safeIdx bytes pixelStart
            g = safeIdx bytes (pixelStart + 1)
            r = safeIdx bytes (pixelStart + 2)
            a = safeIdx bytes (pixelStart + 3)
         in assertEqual "BGRA pixel" (0, 0, 255, 255) (b, g, r, a)
      )
    ]

testBmpEncode :: [(String, TestResult)]
testBmpEncode =
  [ ( "encodeBmp produces non-empty output",
      let bytes = encodeBmp (newCanvas 2 2 red)
       in assertTrue "bmp non-empty" (BL.length bytes > 0)
    ),
    ( "encodeBmp starts with BM",
      let bytes = BL.unpack (encodeBmp (newCanvas 2 2 red))
       in assertEqual "bmp magic" [0x42, 0x4D] (take 2 bytes)
    ),
    ( "encodeBmp 1x1 canvas",
      let bytes = encodeBmp (newCanvas 1 1 red)
       in assertTrue "bmp 1x1" (BL.length bytes > 54)
    ),
    ( "encodeBmp size consistent with dimensions",
      let canvas = newCanvas 8 4 red
          bytes = encodeBmp canvas
       in -- 54 header + 8*4*4 pixel data = 54 + 128 = 182
          assertEqual "bmp size" (182 :: Int) (fromIntegral (BL.length bytes))
    ),
    ( "encodeBmp different canvases differ",
      let bmp1 = encodeBmp (newCanvas 2 2 red)
          bmp2 = encodeBmp (newCanvas 2 2 blue)
       in assertTrue "bmp differ" (bmp1 /= bmp2)
    )
  ]

-- ---------------------------------------------------------------------------
-- PNG tests
-- ---------------------------------------------------------------------------

-- | PNG 8-byte file signature.
pngMagic :: [Word8]
pngMagic = [137, 80, 78, 71, 13, 10, 26, 10]

testPngRoundtrip :: IO [(String, TestResult)]
testPngRoundtrip = do
  let canvas = fillRect (newCanvas 4 4 transparent) 0 0 4 4 red
  (path, tmpHandle) <- openTempFile "." "gb-sprite-test.png"
  hClose tmpHandle
  writePng path canvas
  raw <- BS.readFile path
  removeFile path
  let bytes = BS.unpack raw
  return
    [ ( "PNG starts with correct signature",
        assertEqual "PNG magic" pngMagic (take 8 bytes)
      ),
      ( "PNG file is non-empty",
        assertTrue "PNG non-empty" (length bytes > 8)
      ),
      ( "PNG contains IHDR chunk",
        -- After 8-byte signature: 4 bytes length + "IHDR"
        let chunkType = take 4 (drop 12 bytes)
         in assertEqual "IHDR" [73, 72, 68, 82] chunkType
      ),
      ( "PNG IHDR width is 4",
        let widthBytes = take 4 (drop 16 bytes)
         in assertEqual "PNG width" 4 (fromBE32 widthBytes)
      ),
      ( "PNG IHDR height is 4",
        let heightBytes = take 4 (drop 20 bytes)
         in assertEqual "PNG height" 4 (fromBE32 heightBytes)
      )
    ]

testPngEncode :: [(String, TestResult)]
testPngEncode =
  [ ( "encodePng produces non-empty output",
      let bytes = encodePng (newCanvas 2 2 red)
       in assertTrue "png non-empty" (BL.length bytes > 0)
    ),
    ( "encodePng starts with PNG signature",
      let bytes = BL.unpack (encodePng (newCanvas 2 2 red))
       in assertEqual "png magic" pngMagic (take 8 bytes)
    ),
    ( "encodePng 1x1 canvas",
      let bytes = encodePng (newCanvas 1 1 red)
       in assertTrue "png 1x1" (BL.length bytes > 8)
    ),
    ( "encodePng different canvases differ",
      let png1 = encodePng (newCanvas 2 2 red)
          png2 = encodePng (newCanvas 2 2 blue)
       in assertTrue "png differ" (png1 /= png2)
    ),
    ( "encodePng ends with IEND chunk",
      let bytes = BL.unpack (encodePng (newCanvas 2 2 red))
          iendTrailer = drop (length bytes - 12) bytes
       in -- IEND: length=0, type=IEND, CRC32 of "IEND"
          assertEqual "IEND length" [0, 0, 0, 0] (take 4 iendTrailer)
    )
  ]

-- | Decode a big-endian 32-bit integer from a byte list.
fromBE32 :: [Word8] -> Int
fromBE32 (a : b : c : d : _) =
  fromIntegral a * 16777216
    + fromIntegral b * 65536
    + fromIntegral c * 256
    + fromIntegral d
fromBE32 _ = 0

-- ---------------------------------------------------------------------------
-- Draw tests
-- ---------------------------------------------------------------------------

testDraw :: [(String, TestResult)]
testDraw =
  [ ( "drawThickLine preserves dimensions",
      let c = drawThickLine (newCanvas drawSize drawSize transparent) 0 0 15 15 3 red
       in assertEqual "thick line dims" (drawSize, drawSize) (cWidth c, cHeight c)
    ),
    ( "drawThickLine draws pixels",
      let c = drawThickLine (newCanvas drawSize drawSize transparent) 8 8 8 8 3 red
       in assertEqual "thick line center" red (getPixel c 8 8)
    ),
    ( "drawPolygon preserves dimensions",
      let c = drawPolygon (newCanvas drawSize drawSize transparent) [(2, 2), (14, 2), (8, 14)] red
       in assertEqual "polygon dims" (drawSize, drawSize) (cWidth c, cHeight c)
    ),
    ( "drawPolygon draws vertex",
      let c = drawPolygon (newCanvas drawSize drawSize transparent) [(2, 2), (14, 2), (8, 14)] red
       in assertEqual "polygon vertex" red (getPixel c 2 2)
    ),
    ( "fillPolygon fills interior",
      let c = fillPolygon (newCanvas drawSize drawSize transparent) [(0, 0), (15, 0), (15, 15), (0, 15)] red
       in assertEqual "filled polygon center" red (getPixel c 8 8)
    ),
    ( "drawEllipse preserves dimensions",
      let c = drawEllipse (newCanvas drawSize drawSize transparent) 8 8 6 4 red
       in assertEqual "ellipse dims" (drawSize, drawSize) (cWidth c, cHeight c)
    ),
    ( "drawEllipse draws on perimeter",
      let c = drawEllipse (newCanvas drawSize drawSize transparent) 8 8 6 4 red
       in -- Right edge of ellipse should have a pixel at (14, 8)
          assertEqual "ellipse right" red (getPixel c 14 8)
    ),
    ( "fillEllipse fills center",
      let c = fillEllipse (newCanvas drawSize drawSize transparent) 8 8 6 4 red
       in assertEqual "filled ellipse center" red (getPixel c 8 8)
    ),
    ( "drawBezier preserves dimensions",
      let c = drawBezier (newCanvas drawSize drawSize transparent) (0, 0) (8, 15) (15, 0) red
       in assertEqual "bezier dims" (drawSize, drawSize) (cWidth c, cHeight c)
    ),
    ( "drawBezier draws start point",
      let c = drawBezier (newCanvas drawSize drawSize transparent) (0, 0) (8, 15) (15, 0) red
       in assertEqual "bezier start" red (getPixel c 0 0)
    ),
    ( "drawRoundRect preserves dimensions",
      let c = drawRoundRect (newCanvas drawSize drawSize transparent) 1 1 14 14 3 red
       in assertEqual "round rect dims" (drawSize, drawSize) (cWidth c, cHeight c)
    ),
    ( "fillRoundRect fills center",
      let c = fillRoundRect (newCanvas drawSize drawSize transparent) 1 1 14 14 3 red
       in assertEqual "filled round rect center" red (getPixel c 8 8)
    )
  ]
  where
    drawSize :: Int
    drawSize = 16

testDrawAdvanced :: [(String, TestResult)]
testDrawAdvanced =
  [ ( "drawPolygon empty list is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "polygon empty" c (drawPolygon c [] red)
    ),
    ( "drawPolygon single point is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "polygon single" c (drawPolygon c [(4, 4)] red)
    ),
    ( "fillPolygon empty list is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "fill poly empty" c (fillPolygon c [] red)
    ),
    ( "fillPolygon single point is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "fill poly single" c (fillPolygon c [(4, 4)] red)
    ),
    ( "drawThickLine thickness 1",
      let c = drawThickLine (newCanvas 8 8 transparent) 0 4 7 4 1 red
       in assertTrue "thin thick" (getPixel c 0 4 == red && getPixel c 7 4 == red)
    ),
    ( "drawBezier draws end point",
      let c = drawBezier (newCanvas 16 16 transparent) (0, 0) (8, 15) (15, 0) red
       in assertEqual "bezier end" red (getPixel c 15 0)
    ),
    ( "drawArc preserves dimensions",
      let c = drawArc (newCanvas 32 32 transparent) 16 16 8 8 0.0 360.0 red
       in assertEqual "arc dims" (32, 32) (cWidth c, cHeight c)
    ),
    ( "drawArc draws pixels",
      let c = drawArc (newCanvas 32 32 transparent) 16 16 8 8 0.0 90.0 red
       in assertTrue "arc pixels" (getPixel c 24 16 == red)
    ),
    ( "drawRoundRect zero dims is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "rrect zero" c (drawRoundRect c 0 0 0 8 2 red)
    ),
    ( "fillRoundRect zero dims is no-op",
      let c = newCanvas 8 8 transparent
       in assertEqual "frrect zero" c (fillRoundRect c 0 0 0 8 2 red)
    ),
    ( "fillRoundRect fills corners",
      let c = fillRoundRect (newCanvas 16 16 transparent) 0 0 16 16 2 red
       in assertTrue "rrect corner" (getPixel c 1 1 == red)
    ),
    ( "fillEllipse degenerate rx=0",
      let c = fillEllipse (newCanvas 8 8 transparent) 4 4 0 3 red
       in assertEqual "fill ellipse rx0" red (getPixel c 4 4)
    ),
    ( "drawEllipse degenerate ry=0",
      let c = drawEllipse (newCanvas 8 8 transparent) 4 4 3 0 red
       in assertEqual "draw ellipse ry0" red (getPixel c 4 4)
    ),
    ( "fillPolygon triangle fills",
      let c = fillPolygon (newCanvas 20 20 transparent) [(5, 5), (15, 5), (10, 15)] red
       in assertEqual "triangle fill" red (getPixel c 10 10)
    )
  ]

testEllipse :: [(String, TestResult)]
testEllipse =
  [ ( "drawEllipse circle (rx=ry) top",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 6 6 red
       in assertEqual "circle top" red (getPixel c 16 10)
    ),
    ( "drawEllipse circle (rx=ry) bottom",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 6 6 red
       in assertEqual "circle bottom" red (getPixel c 16 22)
    ),
    ( "drawEllipse circle (rx=ry) left",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 6 6 red
       in assertEqual "circle left" red (getPixel c 10 16)
    ),
    ( "drawEllipse circle (rx=ry) right",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 6 6 red
       in assertEqual "circle right" red (getPixel c 22 16)
    ),
    ( "drawEllipse wide: left edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 10 4 red
       in assertEqual "wide left" red (getPixel c 6 16)
    ),
    ( "drawEllipse wide: right edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 10 4 red
       in assertEqual "wide right" red (getPixel c 26 16)
    ),
    ( "drawEllipse wide: top edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 10 4 red
       in assertEqual "wide top" red (getPixel c 16 12)
    ),
    ( "drawEllipse wide: bottom edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 10 4 red
       in assertEqual "wide bottom" red (getPixel c 16 20)
    ),
    ( "drawEllipse tall: top edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 4 10 red
       in assertEqual "tall top" red (getPixel c 16 6)
    ),
    ( "drawEllipse tall: bottom edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 4 10 red
       in assertEqual "tall bottom" red (getPixel c 16 26)
    ),
    ( "drawEllipse tall: left edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 4 10 red
       in assertEqual "tall left" red (getPixel c 12 16)
    ),
    ( "drawEllipse tall: right edge",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 4 10 red
       in assertEqual "tall right" red (getPixel c 20 16)
    ),
    ( "drawEllipse center should be empty",
      let c = drawEllipse (newCanvas 32 32 transparent) 16 16 8 6 red
       in assertEqual "ellipse center empty" transparent (getPixel c 16 16)
    ),
    ( "drawEllipse rx=1 ry=1",
      let c = drawEllipse (newCanvas 8 8 transparent) 4 4 1 1 red
       in assertTrue "small ellipse" (getPixel c 5 4 == red)
    ),
    ( "fillEllipse fills entire area",
      let c = fillEllipse (newCanvas 32 32 transparent) 16 16 8 4 red
       in assertTrue "fill ellipse area" (getPixel c 16 16 == red && getPixel c 16 12 == red)
    ),
    ( "fillEllipse symmetric",
      let c = fillEllipse (newCanvas 32 32 transparent) 16 16 6 6 red
       in assertTrue
            "fill ellipse sym"
            (getPixel c 10 16 == getPixel c 22 16 && getPixel c 16 10 == getPixel c 16 22)
    )
  ]

-- ---------------------------------------------------------------------------
-- Noise tests
-- ---------------------------------------------------------------------------

testNoise :: [(String, TestResult)]
testNoise =
  [ ( "valueNoise dimensions correct",
      let c = valueNoise noiseSize noiseSize noiseSeed noiseScale
       in assertEqual "noise dims" (noiseSize, noiseSize) (cWidth c, cHeight c)
    ),
    ( "valueNoise deterministic (same seed same output)",
      let c1 = valueNoise noiseSize noiseSize noiseSeed noiseScale
          c2 = valueNoise noiseSize noiseSize noiseSeed noiseScale
       in assertEqual "noise determinism" c1 c2
    ),
    ( "valueNoise different seed different output",
      let c1 = valueNoise noiseSize noiseSize noiseSeed noiseScale
          c2 = valueNoise noiseSize noiseSize (noiseSeed + 1) noiseScale
       in assertTrue "noise different" (c1 /= c2)
    ),
    ( "valueNoise values are valid colors",
      let c = valueNoise noiseSize noiseSize noiseSeed noiseScale
       in assertTrue "noise valid" (colorA (getPixel c 0 0) == 255)
    ),
    ( "valueNoiseColor dimensions correct",
      let c = valueNoiseColor noiseSize noiseSize noiseSeed noiseScale red blue
       in assertEqual "color noise dims" (noiseSize, noiseSize) (cWidth c, cHeight c)
    ),
    ( "fbm dimensions correct",
      let c = fbm noiseSize noiseSize noiseSeed fbmOctaves noiseScale
       in assertEqual "fbm dims" (noiseSize, noiseSize) (cWidth c, cHeight c)
    ),
    ( "fbm deterministic",
      let c1 = fbm noiseSize noiseSize noiseSeed fbmOctaves noiseScale
          c2 = fbm noiseSize noiseSize noiseSeed fbmOctaves noiseScale
       in assertEqual "fbm determinism" c1 c2
    )
  ]
  where
    noiseSize :: Int
    noiseSize = 16

    noiseSeed :: Int
    noiseSeed = 42

    noiseScale :: Double
    noiseScale = 4.0

    fbmOctaves :: Int
    fbmOctaves = 3

testNoiseAdvanced :: [(String, TestResult)]
testNoiseAdvanced =
  [ ( "valueNoise all pixels opaque",
      let c = valueNoise 8 8 42 4.0
          allOpaque = all (\(x, y) -> colorA (getPixel c x y) == 255) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "noise opaque" allOpaque
    ),
    ( "valueNoise grayscale (R=G=B)",
      let c = valueNoise 8 8 42 4.0
          pixel = getPixel c 3 3
       in assertTrue "noise gray" (colorR pixel == colorG pixel && colorG pixel == colorB pixel)
    ),
    ( "valueNoiseColor between colors",
      let c = valueNoiseColor 8 8 42 4.0 red blue
          pixel = getPixel c 3 3
       in assertTrue "noise color range" (colorA pixel == 255)
    ),
    ( "valueNoiseColor different seeds differ",
      let c1 = valueNoiseColor 8 8 1 4.0 red blue
          c2 = valueNoiseColor 8 8 2 4.0 red blue
       in assertTrue "color noise seeds" (c1 /= c2)
    ),
    ( "fbm different seeds differ",
      let c1 = fbm 8 8 1 3 4.0
          c2 = fbm 8 8 2 3 4.0
       in assertTrue "fbm seeds" (c1 /= c2)
    ),
    ( "fbm all pixels opaque",
      let c = fbm 8 8 42 3 4.0
          allOpaque = all (\(x, y) -> colorA (getPixel c x y) == 255) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "fbm opaque" allOpaque
    ),
    ( "fbm octaves=1 differs from octaves=4",
      let c1 = fbm 8 8 42 1 4.0
          c2 = fbm 8 8 42 4 4.0
       in assertTrue "fbm octaves" (c1 /= c2)
    ),
    ( "valueNoise scale affects output",
      let c1 = valueNoise 8 8 42 1.0
          c2 = valueNoise 8 8 42 8.0
       in assertTrue "noise scale" (c1 /= c2)
    )
  ]

-- ---------------------------------------------------------------------------
-- Gradient tests
-- ---------------------------------------------------------------------------

testGradient :: [(String, TestResult)]
testGradient =
  [ ( "linearGradient horizontal dimensions",
      let c = linearGradient gradSize gradSize red blue True
       in assertEqual "hgrad dims" (gradSize, gradSize) (cWidth c, cHeight c)
    ),
    ( "linearGradient horizontal left is start color",
      let c = linearGradient gradSize gradSize red blue True
       in assertEqual "hgrad left" red (getPixel c 0 0)
    ),
    ( "linearGradient horizontal right is end color",
      let c = linearGradient gradSize gradSize red blue True
       in assertEqual "hgrad right" blue (getPixel c (gradSize - 1) 0)
    ),
    ( "linearGradient vertical top is start color",
      let c = linearGradient gradSize gradSize red blue False
       in assertEqual "vgrad top" red (getPixel c 0 0)
    ),
    ( "linearGradient vertical bottom is end color",
      let c = linearGradient gradSize gradSize red blue False
       in assertEqual "vgrad bottom" blue (getPixel c 0 (gradSize - 1))
    ),
    ( "radialGradient dimensions correct",
      let c = radialGradient gradSize gradSize (gradSize `div` 2) (gradSize `div` 2) (gradSize `div` 2) white black
       in assertEqual "radial dims" (gradSize, gradSize) (cWidth c, cHeight c)
    ),
    ( "radialGradient center is inner color",
      let cx = gradSize `div` 2
          cy = gradSize `div` 2
          c = radialGradient gradSize gradSize cx cy (gradSize `div` 2) white black
       in assertEqual "radial center" white (getPixel c cx cy)
    ),
    ( "diagonalGradient dimensions correct",
      let c = diagonalGradient gradSize gradSize red blue
       in assertEqual "diag dims" (gradSize, gradSize) (cWidth c, cHeight c)
    ),
    ( "diagonalGradient top-left is start color",
      let c = diagonalGradient gradSize gradSize red blue
       in assertEqual "diag topleft" red (getPixel c 0 0)
    ),
    ( "diagonalGradient bottom-right is end color",
      let c = diagonalGradient gradSize gradSize red blue
       in assertEqual "diag botright" blue (getPixel c (gradSize - 1) (gradSize - 1))
    )
  ]
  where
    gradSize :: Int
    gradSize = 32

testGradientAdvanced :: [(String, TestResult)]
testGradientAdvanced =
  [ ( "linearGradient horizontal midpoint is mix",
      let c = linearGradient 32 32 black white True
          mid = getPixel c 16 0
       in assertTrue "hgrad mid" (colorR mid > 100 && colorR mid < 200)
    ),
    ( "linearGradient vertical constant across row",
      let c = linearGradient 8 8 red blue False
       in assertEqual "vgrad row" (getPixel c 0 3) (getPixel c 7 3)
    ),
    ( "linearGradient horizontal constant down column",
      let c = linearGradient 8 8 red blue True
       in assertEqual "hgrad col" (getPixel c 3 0) (getPixel c 3 7)
    ),
    ( "radialGradient far corner is outer color",
      let c = radialGradient 32 32 16 16 8 white black
       in assertEqual "radial corner" black (getPixel c 0 0)
    ),
    ( "radialGradient all pixels opaque",
      let c = radialGradient 8 8 4 4 4 white black
          allOpaque = all (\(x, y) -> colorA (getPixel c x y) == 255) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "radial opaque" allOpaque
    ),
    ( "diagonalGradient midpoint is mix",
      let c = diagonalGradient 32 32 black white
          mid = getPixel c 8 8
       in assertTrue "diag mid" (colorR mid > 0 && colorR mid < 255)
    ),
    ( "linearGradient 1x1 canvas",
      let c = linearGradient 1 1 red blue True
       in assertEqual "1x1 grad" (1, 1) (cWidth c, cHeight c)
    )
  ]

-- ---------------------------------------------------------------------------
-- NineSlice tests
-- ---------------------------------------------------------------------------

testNineSlice :: [(String, TestResult)]
testNineSlice =
  [ ( "renderNineSlice output dimensions match target",
      let src = newCanvas sliceSrcSize sliceSrcSize red
          ns = nineSlice src sliceBorder sliceBorder sliceBorder sliceBorder
          result = renderNineSlice ns sliceTargetW sliceTargetH
       in assertEqual "nine slice dims" (sliceTargetW, sliceTargetH) (cWidth result, cHeight result)
    ),
    ( "renderNineSlice corners preserved",
      let src = setPixel (newCanvas sliceSrcSize sliceSrcSize blue) 0 0 red
          ns = nineSlice src sliceBorder sliceBorder sliceBorder sliceBorder
          result = renderNineSlice ns sliceTargetW sliceTargetH
       in assertEqual "nine slice corner" red (getPixel result 0 0)
    ),
    ( "renderNineSlice center fills",
      let src = fillRect (newCanvas sliceSrcSize sliceSrcSize red) sliceBorder sliceBorder (sliceSrcSize - 2 * sliceBorder) (sliceSrcSize - 2 * sliceBorder) green
          ns = nineSlice src sliceBorder sliceBorder sliceBorder sliceBorder
          result = renderNineSlice ns sliceTargetW sliceTargetH
          midX = sliceTargetW `div` 2
          midY = sliceTargetH `div` 2
       in assertEqual "nine slice center" green (getPixel result midX midY)
    ),
    ( "renderNineSlice smaller than source",
      let src = newCanvas sliceSrcSize sliceSrcSize red
          ns = nineSlice src 1 1 1 1
          result = renderNineSlice ns 4 4
       in assertEqual "nine slice small dims" (4, 4) (cWidth result, cHeight result)
    )
  ]
  where
    sliceSrcSize :: Int
    sliceSrcSize = 12

    sliceBorder :: Int
    sliceBorder = 3

    sliceTargetW :: Int
    sliceTargetW = 32

    sliceTargetH :: Int
    sliceTargetH = 24

testNineSliceAdvanced :: [(String, TestResult)]
testNineSliceAdvanced =
  [ ( "nineSlice clamps negative borders",
      let ns = nineSlice (newCanvas 8 8 red) (-1) (-1) (-1) (-1)
       in assertTrue "ns clamp neg" (nsLeft ns == 0 && nsRight ns == 0 && nsTop ns == 0 && nsBottom ns == 0)
    ),
    ( "nineSlice preserves source",
      let src = newCanvas 8 8 red
          ns = nineSlice src 2 2 2 2
       in assertEqual "ns source" src (nsCanvas ns)
    ),
    ( "renderNineSlice same size as source",
      let src = newCanvas 8 8 red
          ns = nineSlice src 2 2 2 2
          result = renderNineSlice ns 8 8
       in assertEqual "ns same size" (8, 8) (cWidth result, cHeight result)
    ),
    ( "renderNineSlice large target",
      let src = newCanvas 8 8 red
          ns = nineSlice src 2 2 2 2
          result = renderNineSlice ns 64 64
       in assertEqual "ns large" (64, 64) (cWidth result, cHeight result)
    ),
    ( "renderNineSlice bottom-right corner",
      let src = setPixel (newCanvas 8 8 blue) 7 7 red
          ns = nineSlice src 2 2 2 2
          result = renderNineSlice ns 16 16
       in assertEqual "ns BR corner" red (getPixel result 15 15)
    ),
    ( "NineSlice Show works",
      assertTrue "ns show" (not (null (show (nineSlice (newCanvas 4 4 red) 1 1 1 1))))
    ),
    ( "NineSlice Eq works",
      let ns = nineSlice (newCanvas 4 4 red) 1 1 1 1
       in assertTrue "ns eq" (ns == ns)
    )
  ]

-- ---------------------------------------------------------------------------
-- Dither tests
-- ---------------------------------------------------------------------------

testDither :: [(String, TestResult)]
testDither =
  [ ( "orderedDither output dimensions preserved",
      let c = newCanvas ditherSize ditherSize red
          result = orderedDither Bayer4 testPal c
       in assertEqual "dither dims" (ditherSize, ditherSize) (cWidth result, cHeight result)
    ),
    ( "orderedDither output contains only palette colors",
      let c = linearGradient ditherSize ditherSize black white True
          result = orderedDither Bayer4 bwPal c
          allPalette = all (\(x, y) -> getPixel result x y `elem` [black, white]) coords
       in assertTrue "palette only" allPalette
    ),
    ( "orderedDither Bayer2 works",
      let c = newCanvas ditherSize ditherSize (Color 128 128 128 255)
          result = orderedDither Bayer2 bwPal c
       in assertEqual "bayer2 dims" (ditherSize, ditherSize) (cWidth result, cHeight result)
    ),
    ( "orderedDither Bayer8 works",
      let c = newCanvas ditherSize ditherSize (Color 128 128 128 255)
          result = orderedDither Bayer8 bwPal c
       in assertEqual "bayer8 dims" (ditherSize, ditherSize) (cWidth result, cHeight result)
    ),
    ( "orderedDither empty palette is identity",
      let c = newCanvas ditherSize ditherSize red
          result = orderedDither Bayer4 (Palette []) c
       in assertEqual "empty palette" c result
    )
  ]
  where
    ditherSize :: Int
    ditherSize = 8

    bwPal :: Palette
    bwPal = fromColors [black, white]

    testPal :: Palette
    testPal = fromColors [red, green, blue, yellow]

    coords :: [(Int, Int)]
    coords = [(x, y) | x <- [0 .. ditherSize - 1], y <- [0 .. ditherSize - 1]]

testDitherAdvanced :: [(String, TestResult)]
testDitherAdvanced =
  [ ( "orderedDither solid red stays red",
      let c = newCanvas 8 8 red
          pal = fromColors [red, green, blue]
          result = orderedDither Bayer4 pal c
          center = getPixel result 4 4
       in assertEqual "dither solid" red center
    ),
    ( "orderedDither Bayer2 has dithering pattern",
      let c = linearGradient 8 8 black white True
          result = orderedDither Bayer2 (fromColors [black, white]) c
          hasBoth =
            any (\(x, y) -> getPixel result x y == black) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
              && any (\(x, y) -> getPixel result x y == white) [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]
       in assertTrue "bayer2 pattern" hasBoth
    ),
    ( "orderedDither preserves alpha channel",
      let c = newCanvas 4 4 red
          result = orderedDither Bayer4 (fromColors [red]) c
       in assertEqual "dither alpha" 255 (colorA (getPixel result 0 0))
    ),
    ( "DitherMatrix Show works",
      assertTrue "dither show" (not (null (show Bayer4)))
    ),
    ( "DitherMatrix Eq works",
      assertTrue "dither eq" (Bayer4 == Bayer4 && Bayer4 /= Bayer2)
    ),
    ( "orderedDither gameboy palette",
      let c = linearGradient 8 8 black white True
          result = orderedDither Bayer4 gameboy c
       in assertEqual "gb dither dims" (8, 8) (cWidth result, cHeight result)
    )
  ]

-- ---------------------------------------------------------------------------
-- Tilemap tests
-- ---------------------------------------------------------------------------

testTilemap :: [(String, TestResult)]
testTilemap =
  [ ( "renderTilemap dimensions match grid",
      let sheet = packSheet 0 [("tile", newCanvas 8 8 red)]
          config =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 4,
                tmGridHeight = 3,
                tmTiles = replicate 12 0
              }
          result = renderTilemap sheet config
       in assertEqual "tilemap dims" (32, 24) (cWidth result, cHeight result)
    ),
    ( "renderTilemap fills tiles",
      let sheet = packSheet 0 [("tile", newCanvas 8 8 red)]
          config =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 2,
                tmGridHeight = 2,
                tmTiles = [0, 0, 0, 0]
              }
          result = renderTilemap sheet config
       in assertEqual "tilemap pixel" red (getPixel result 4 4)
    ),
    ( "renderTilemap negative index is empty",
      let sheet = packSheet 0 [("tile", newCanvas 8 8 red)]
          config =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 2,
                tmGridHeight = 1,
                tmTiles = [-1, 0]
              }
          result = renderTilemap sheet config
       in assertEqual "tilemap neg" transparent (getPixel result 4 4)
    ),
    ( "renderTilemap OOB index is empty",
      let sheet = packSheet 0 [("tile", newCanvas 8 8 red)]
          config =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 1,
                tmGridHeight = 1,
                tmTiles = [99]
              }
          result = renderTilemap sheet config
       in assertEqual "tilemap oob" transparent (getPixel result 4 4)
    ),
    ( "renderTilemap two different tiles",
      let sheet = packSheet 0 [("a", newCanvas 8 8 red), ("b", newCanvas 8 8 green)]
          config =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 2,
                tmGridHeight = 1,
                tmTiles = [0, 1]
              }
          result = renderTilemap sheet config
       in assertTrue "tilemap 2 tiles" (getPixel result 4 4 == red || getPixel result 12 4 == green)
    ),
    ( "TilemapConfig Show works",
      assertTrue
        "tm show"
        ( not
            ( null
                ( show
                    ( TilemapConfig
                        { tmTileWidth = 8,
                          tmTileHeight = 8,
                          tmGridWidth = 1,
                          tmGridHeight = 1,
                          tmTiles = [0]
                        }
                    )
                )
            )
        )
    ),
    ( "TilemapConfig Eq works",
      let tc =
            TilemapConfig
              { tmTileWidth = 8,
                tmTileHeight = 8,
                tmGridWidth = 1,
                tmGridHeight = 1,
                tmTiles = [0]
              }
       in assertTrue "tm eq" (tc == tc)
    )
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Decode a little-endian 16-bit unsigned value.
fromLE16 :: [Word8] -> Int
fromLE16 (lo : hi : _) = fromIntegral lo + fromIntegral hi * 256
fromLE16 _ = 0

-- | Decode a little-endian 32-bit unsigned value.
fromLE32 :: [Word8] -> Int
fromLE32 (b0 : b1 : b2 : b3 : _) =
  fromIntegral b0
    + fromIntegral b1 * 256
    + fromIntegral b2 * 65536
    + fromIntegral b3 * 16777216
fromLE32 _ = 0
