-- | Sprite sheet packing: combine multiple canvases into a single atlas.
--
-- Uses a simple shelf-packing algorithm for bin packing.
module GBSprite.Sheet
  ( -- * Types
    SheetEntry (..),
    SpriteSheet (..),

    -- * Packing
    packSheet,
  )
where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import GBSprite.Canvas (Canvas (..), newCanvas)
import GBSprite.Color (transparent)
import GBSprite.Compose (stamp)

-- | Metadata for a sprite placed in the sheet.
data SheetEntry = SheetEntry
  { -- | Original sprite name
    entryName :: !String,
    -- | X position in the atlas
    entryX :: !Int,
    -- | Y position in the atlas
    entryY :: !Int,
    -- | Width
    entryWidth :: !Int,
    -- | Height
    entryHeight :: !Int
  }
  deriving (Show, Eq)

-- | A packed sprite sheet: atlas image + metadata.
data SpriteSheet = SpriteSheet
  { -- | The combined atlas canvas
    sheetCanvas :: !Canvas,
    -- | Placement metadata for each sprite
    sheetEntries :: ![SheetEntry]
  }
  deriving (Show, Eq)

-- | Pack named canvases into a sprite sheet atlas.
--
-- Uses shelf packing: sort by height descending, place left-to-right
-- on shelves. The @padding@ parameter adds space between sprites.
packSheet :: Int -> [(String, Canvas)] -> SpriteSheet
packSheet padding items =
  let sorted = sortBy (comparing (Down . cHeight . snd)) items
      (entries, totalW, totalH) = shelfPack padding sorted
      atlas = newCanvas totalW totalH transparent
      drawn = foldl drawEntry atlas entries
   in SpriteSheet drawn entries
  where
    drawEntry canvas entry =
      let (_, src) = findByName (entryName entry) items
       in stamp canvas (entryX entry) (entryY entry) src

    findByName name = head . filter (\(n, _) -> n == name)

-- | Shelf-pack algorithm: returns entries + total dimensions.
shelfPack :: Int -> [(String, Canvas)] -> ([SheetEntry], Int, Int)
shelfPack padding items = go items 0 0 0 0 []
  where
    maxShelfWidth :: Int
    maxShelfWidth = computeMaxWidth padding items

    go [] _ _ totalW totalH acc = (reverse acc, max totalW 1, max totalH 1)
    go ((name, canvas) : rest) shelfX shelfY totalW totalH acc =
      let w = cWidth canvas
          h = cHeight canvas
       in if shelfX + w > maxShelfWidth && shelfX > 0
            then
              -- Start new shelf
              go ((name, canvas) : rest) 0 totalH totalW totalH acc
            else
              let entry =
                    SheetEntry
                      { entryName = name,
                        entryX = shelfX,
                        entryY = shelfY,
                        entryWidth = w,
                        entryHeight = h
                      }
                  nextX = shelfX + w + padding
                  nextTotalW = max totalW (shelfX + w)
                  nextTotalH = max totalH (shelfY + h)
               in go rest nextX shelfY nextTotalW nextTotalH (entry : acc)

-- | Compute a reasonable atlas width based on total area.
computeMaxWidth :: Int -> [(String, Canvas)] -> Int
computeMaxWidth _padding items =
  let totalArea = sum [cWidth c * cHeight c | (_, c) <- items]
      side = ceiling (sqrt (fromIntegral totalArea :: Double)) :: Int
   in max side (maximum (map (cWidth . snd) items))
