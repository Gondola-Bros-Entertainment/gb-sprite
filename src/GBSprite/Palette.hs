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
  )
where

import Data.Maybe (fromMaybe)
import GBSprite.Color (Color (..))

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
paletteColor (Palette colors) idx
  | idx < 0 = head colors
  | idx >= length colors = last colors
  | otherwise = colors !! idx

-- | Swap colors: replace every occurrence of a source palette color
-- with the corresponding destination palette color.
paletteSwap :: Palette -> Palette -> Color -> Color
paletteSwap (Palette src) (Palette dst) color =
  fromMaybe color (findIndex color src dst)
  where
    findIndex :: Color -> [Color] -> [Color] -> Maybe Color
    findIndex _ [] _ = Nothing
    findIndex _ _ [] = Nothing
    findIndex target (s : ss) (d : ds)
      | target == s = Just d
      | otherwise = findIndex target ss ds

-- ---------------------------------------------------------------------------
-- Built-in palettes
-- ---------------------------------------------------------------------------

-- | Classic 4-shade grayscale (Game Boy style).
grayscale4 :: Palette
grayscale4 =
  Palette
    [ Color 15 56 15 maxA,
      Color 48 98 48 maxA,
      Color 139 172 15 maxA,
      Color 155 188 15 maxA
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
