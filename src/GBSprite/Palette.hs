-- | Color palettes and palette swapping.
--
-- Palettes are indexed collections of colors used for retro-style
-- sprite coloring and palette-swap effects.
module GBSprite.Palette
  ( -- * Types
    Palette (..),

    -- * Built-in palettes
    grayscale4,
    grayscale8,
    gameboy,
    nes,

    -- * Operations
    paletteColor,
    paletteSwap,
    fromColors,
    paletteSize,
    paletteLerp,
    extractPalette,
    quantizeColor,
  )
where

import Data.List (foldl', sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word8)
import GBSprite.Color (Color (..), lerp)

-- | An ordered collection of colors.
newtype Palette = Palette
  { paletteColors :: [Color]
  }
  deriving (Show, Eq)

-- | Build a palette from a list of colors.
fromColors :: [Color] -> Palette
fromColors = Palette

-- | Look up a color by index. Returns the last color for out-of-range,
-- or transparent for empty palettes.
paletteColor :: Palette -> Int -> Color
paletteColor (Palette []) _ = Color 0 0 0 0
paletteColor (Palette colors@(first : _)) idx
  | idx < 0 = first
  | otherwise = go 0 colors
  where
    go _ [c] = c
    go i (c : cs)
      | i == idx = c
      | otherwise = go (i + 1) cs
    go _ [] = first

-- | Swap colors: replace every occurrence of a source palette color
-- with the corresponding destination palette color.
paletteSwap :: Palette -> Palette -> Color -> Color
paletteSwap (Palette src) (Palette dst) color =
  fromMaybe color (findSwap color src dst)
  where
    findSwap :: Color -> [Color] -> [Color] -> Maybe Color
    findSwap _ [] _ = Nothing
    findSwap _ _ [] = Nothing
    findSwap target (s : ss) (d : ds)
      | target == s = Just d
      | otherwise = findSwap target ss ds

-- | Number of colors in the palette.
paletteSize :: Palette -> Int
paletteSize (Palette cs) = length cs

-- | Interpolate between two palettes element-wise at position @t@ in @[0, 1]@.
-- If palettes differ in length, the shorter one is padded with transparent.
paletteLerp :: Double -> Palette -> Palette -> Palette
paletteLerp t (Palette as) (Palette bs) =
  Palette (zipWithPad (lerp t) as bs)
  where
    zipWithPad _ [] [] = []
    zipWithPad f (x : xs) [] = f x transparentColor : zipWithPad f xs []
    zipWithPad f [] (y : ys) = f transparentColor y : zipWithPad f [] ys
    zipWithPad f (x : xs) (y : ys) = f x y : zipWithPad f xs ys

-- | Extract a palette of up to @n@ representative colors from a list
-- of pixel colors using median-cut quantization.
extractPalette :: Int -> [Color] -> Palette
extractPalette targetCount colors
  | targetCount <= 0 = Palette []
  | otherwise =
      let opaque = filter (\(Color _ _ _ a) -> a >= minOpaqueAlpha) colors
          initial = case opaque of
            [] -> []
            _ -> [opaque]
          buckets = medianCut targetCount initial
          averaged = map bucketAverage buckets
       in Palette averaged

-- | Map a color to the nearest palette color (by squared RGB distance).
-- Useful with 'GBSprite.Canvas.mapPixels' for full-canvas quantization.
quantizeColor :: Palette -> Color -> Color
quantizeColor (Palette []) c = c
quantizeColor pal c = findNearest pal c

-- ---------------------------------------------------------------------------
-- Built-in palettes
-- ---------------------------------------------------------------------------

-- | 4-shade grayscale ramp (evenly spaced).
grayscale4 :: Palette
grayscale4 =
  Palette
    [ Color 0 0 0 maxA,
      Color 85 85 85 maxA,
      Color 170 170 170 maxA,
      Color 255 255 255 maxA
    ]
  where
    maxA = 255

-- | 8-shade grayscale ramp.
grayscale8 :: Palette
grayscale8 =
  Palette
    [ Color 0 0 0 maxA,
      Color 36 36 36 maxA,
      Color 73 73 73 maxA,
      Color 109 109 109 maxA,
      Color 146 146 146 maxA,
      Color 182 182 182 maxA,
      Color 219 219 219 maxA,
      Color 255 255 255 maxA
    ]
  where
    maxA = 255

-- | Game Boy green palette (authentic DMG colors).
gameboy :: Palette
gameboy =
  Palette
    [ Color 15 56 15 maxA, -- darkest
      Color 48 98 48 maxA,
      Color 139 172 15 maxA,
      Color 155 188 15 maxA -- lightest
    ]
  where
    maxA = 255

-- | A small NES-inspired palette (16 commonly used colors).
nes :: Palette
nes =
  Palette
    [ Color 0 0 0 maxA, -- black
      Color 252 252 252 maxA, -- white
      Color 188 0 0 maxA, -- red
      Color 0 120 0 maxA, -- green
      Color 0 0 168 maxA, -- blue
      Color 248 184 0 maxA, -- yellow
      Color 0 168 168 maxA, -- cyan
      Color 148 0 132 maxA, -- magenta
      Color 228 92 16 maxA, -- orange
      Color 124 8 76 maxA, -- purple
      Color 252 160 68 maxA, -- peach
      Color 168 204 0 maxA, -- lime
      Color 88 216 84 maxA, -- bright green
      Color 104 136 252 maxA, -- light blue
      Color 164 164 164 maxA, -- gray
      Color 216 120 168 maxA -- pink
    ]
  where
    maxA = 255

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

transparentColor :: Color
transparentColor = Color 0 0 0 0

minOpaqueAlpha :: Word8
minOpaqueAlpha = 1

channelMax :: Int
channelMax = 255

medianDivisor :: Int
medianDivisor = 2

medianCut :: Int -> [[Color]] -> [[Color]]
medianCut targetCount buckets
  | length buckets >= targetCount = buckets
  | otherwise =
      case findWidestBucket buckets of
        Nothing -> buckets
        Just (chosen, rest) ->
          let (left, right) = splitBucket chosen
           in case (left, right) of
                ([], []) -> buckets
                ([], _) -> medianCut targetCount (right : rest)
                (_, []) -> medianCut targetCount (left : rest)
                _ -> medianCut targetCount (left : right : rest)

findWidestBucket :: [[Color]] -> Maybe ([Color], [[Color]])
findWidestBucket [] = Nothing
findWidestBucket [b] = Just (b, [])
findWidestBucket buckets =
  let scored = map (\b -> (bucketRange b, b)) buckets
      sorted = sortBy (comparing (Down . fst)) scored
   in case sorted of
        ((_, best) : restBuckets) -> Just (best, map snd restBuckets)
        [] -> Nothing

bucketRange :: [Color] -> Int
bucketRange [] = 0
bucketRange colors =
  let rs = map (\(Color r _ _ _) -> fromIntegral r :: Int) colors
      gs = map (\(Color _ g _ _) -> fromIntegral g :: Int) colors
      bs = map (\(Color _ _ b _) -> fromIntegral b :: Int) colors
      rangeR = maxList rs - minList rs
      rangeG = maxList gs - minList gs
      rangeB = maxList bs - minList bs
   in max rangeR (max rangeG rangeB)

splitBucket :: [Color] -> ([Color], [Color])
splitBucket [] = ([], [])
splitBucket colors =
  let rs = map (\(Color r _ _ _) -> fromIntegral r :: Int) colors
      gs = map (\(Color _ g _ _) -> fromIntegral g :: Int) colors
      bs = map (\(Color _ _ b _) -> fromIntegral b :: Int) colors
      rangeR = maxList rs - minList rs
      rangeG = maxList gs - minList gs
      rangeB = maxList bs - minList bs
      sorted
        | rangeR >= rangeG && rangeR >= rangeB =
            sortBy (comparing (\(Color r _ _ _) -> r)) colors
        | rangeG >= rangeB =
            sortBy (comparing (\(Color _ g _ _) -> g)) colors
        | otherwise =
            sortBy (comparing (\(Color _ _ b _) -> b)) colors
      mid = length sorted `div` medianDivisor
   in splitAt mid sorted

maxList :: [Int] -> Int
maxList [] = 0
maxList (x : xs) = foldl' max x xs

minList :: [Int] -> Int
minList [] = 0
minList (x : xs) = foldl' min x xs

bucketAverage :: [Color] -> Color
bucketAverage [] = transparentColor
bucketAverage colors =
  let n = length colors
      (totalR, totalG, totalB) =
        foldl'
          (\(!accR, !accG, !accB) (Color r g b _) -> (accR + fromIntegral r, accG + fromIntegral g, accB + fromIntegral b))
          (0 :: Int, 0 :: Int, 0 :: Int)
          colors
   in Color
        (fromIntegral (totalR `div` n))
        (fromIntegral (totalG `div` n))
        (fromIntegral (totalB `div` n))
        (fromIntegral channelMax)

findNearest :: Palette -> Color -> Color
findNearest (Palette []) c = c
findNearest (Palette (first : rest)) target =
  foldl'
    ( \best candidate ->
        if colorDist target candidate < colorDist target best
          then candidate
          else best
    )
    first
    rest

colorDist :: Color -> Color -> Int
colorDist (Color r1 g1 b1 _) (Color r2 g2 b2 _) =
  let dr = fromIntegral r1 - fromIntegral r2 :: Int
      dg = fromIntegral g1 - fromIntegral g2 :: Int
      db = fromIntegral b1 - fromIntegral b2 :: Int
   in dr * dr + dg * dg + db * db
