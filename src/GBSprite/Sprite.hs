-- | Sprite type: a named multi-frame image with origin and bounding box.
module GBSprite.Sprite
  ( -- * Types
    Sprite (..),
    BoundingBox (..),

    -- * Construction
    singleFrame,
    multiFrame,

    -- * Queries
    spriteWidth,
    spriteHeight,
    frameCount,
    getFrame,
  )
where

import GBSprite.Canvas (Canvas (..))

-- | A bounding box for hit detection.
data BoundingBox = BoundingBox
  { bbX :: !Int,
    bbY :: !Int,
    bbWidth :: !Int,
    bbHeight :: !Int
  }
  deriving (Show, Eq)

-- | A named sprite with one or more frames and an origin point.
data Sprite = Sprite
  { -- | Human-readable name
    spriteName :: !String,
    -- | Origin X offset (for positioning)
    spriteOriginX :: !Int,
    -- | Origin Y offset (for positioning)
    spriteOriginY :: !Int,
    -- | Animation frames (at least one)
    spriteFrames :: ![Canvas],
    -- | Optional hit region
    spriteBounds :: !(Maybe BoundingBox)
  }
  deriving (Show, Eq)

-- | Create a single-frame sprite centered at (0, 0).
singleFrame :: String -> Canvas -> Sprite
singleFrame name canvas =
  Sprite
    { spriteName = name,
      spriteOriginX = 0,
      spriteOriginY = 0,
      spriteFrames = [canvas],
      spriteBounds = Nothing
    }

-- | Create a multi-frame sprite.
multiFrame :: String -> Int -> Int -> [Canvas] -> Sprite
multiFrame name ox oy frames =
  Sprite
    { spriteName = name,
      spriteOriginX = ox,
      spriteOriginY = oy,
      spriteFrames = frames,
      spriteBounds = Nothing
    }

-- | Width of the first frame (all frames should match).
spriteWidth :: Sprite -> Int
spriteWidth s = case spriteFrames s of
  [] -> 0
  (f : _) -> cWidth f

-- | Height of the first frame.
spriteHeight :: Sprite -> Int
spriteHeight s = case spriteFrames s of
  [] -> 0
  (f : _) -> cHeight f

-- | Number of animation frames.
frameCount :: Sprite -> Int
frameCount = length . spriteFrames

-- | Get a specific frame by index (clamped to valid range).
getFrame :: Sprite -> Int -> Maybe Canvas
getFrame s idx
  | idx < 0 || idx >= frameCount s = Nothing
  | otherwise = safeIndex idx (spriteFrames s)

-- | Safe list indexing without '!!'.
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (x : _) = Just x
safeIndex i (_ : xs) = safeIndex (i - 1) xs
