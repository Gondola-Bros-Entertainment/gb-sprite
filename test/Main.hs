-- | gb-sprite test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gbnet-hs.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VS
import Data.Word (Word8)
import GBSprite.Animation (animationDone, animationFrame, loopAnimation, onceAnimation, pingPongAnimation)
import GBSprite.BMP (writeBmp)
import GBSprite.Canvas (Canvas (..), drawCircle, drawLine, drawRect, fillCircle, fillRect, getPixel, inBounds, newCanvas, setPixel)
import GBSprite.Color (Color (..), alphaBlend, black, blue, green, lerp, multiply, red, scaleAlpha, transparent, white, withAlpha)
import GBSprite.Compose (overlay, stamp)
import GBSprite.Palette (Palette (..), fromColors, gameboy, paletteColor, paletteSwap)
import GBSprite.Sheet (SheetEntry (..), SpriteSheet (..), packSheet)
import GBSprite.Sprite (frameCount, getFrame, singleFrame, spriteHeight, spriteWidth)
import GBSprite.Text (defaultFont, renderText, textWidth)
import GBSprite.Transform (flipH, flipV, rotate180, rotate270, rotate90, scaleNearest)
import GBSprite.VFX (ExplosionConfig (..), RingConfig (..), explosionFrames, flashFrames, ringExpandFrames)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

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
  runTests
    ( testColor
        ++ testCanvas
        ++ testTransform
        ++ testCompose
        ++ testAnimation
        ++ testPalette
        ++ testSprite
        ++ testSheet
        ++ testText
        ++ testVFX
        ++ bmpTests
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
    ( "multiply white identity",
      assertEqual "mul white" red (multiply white red)
    ),
    ( "multiply black zero",
      assertEqual "mul black" (Color 0 0 0 255) (multiply black red)
    ),
    ( "alphaBlend opaque over = top",
      assertEqual "opaque over" red (alphaBlend red blue)
    ),
    ( "alphaBlend transparent over = bottom",
      assertEqual "transparent over" blue (alphaBlend transparent blue)
    ),
    ( "withAlpha preserves RGB",
      let halfAlpha = withAlpha 128 red
       in assertTrue "RGB preserved" (colorR halfAlpha == 255 && colorG halfAlpha == 0 && colorB halfAlpha == 0)
    ),
    ( "scaleAlpha 0 gives 0 alpha",
      assertEqual "zero alpha" 0 (colorA (scaleAlpha 0.0 red))
    ),
    ( "scaleAlpha 1 preserves alpha",
      assertEqual "full alpha" 255 (colorA (scaleAlpha 1.0 red))
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
       in assertEqual "pixel bytes" (canvasSize * canvasSize * 4) (VS.length (cPixels c))
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

-- ---------------------------------------------------------------------------
-- BMP tests
-- ---------------------------------------------------------------------------

testBmpRoundtrip :: IO [(String, TestResult)]
testBmpRoundtrip = do
  let canvas = fillRect (newCanvas 4 4 transparent) 0 0 4 4 red
      path = "/tmp/gb-sprite-test.bmp"
  writeBmp path canvas
  raw <- BS.readFile path
  let bytes = BS.unpack raw
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
        let h = fromLE32 (take 4 (drop 22 bytes))
         in assertEqual "height" 4 h
      ),
      ( "BMP bits per pixel is 32",
        let bpp = fromLE16 (take 2 (drop 28 bytes))
         in assertEqual "bpp" 32 bpp
      ),
      ( "BMP encodes pixel data as BGRA",
        -- First pixel (bottom-left in BMP) should be red = BGRA(0,0,255,255)
        let pixelStart = 54
            b = bytes !! pixelStart
            g = bytes !! (pixelStart + 1)
            r = bytes !! (pixelStart + 2)
            a = bytes !! (pixelStart + 3)
         in assertEqual "BGRA pixel" (0, 0, 255, 255) (b, g, r, a)
      )
    ]

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
