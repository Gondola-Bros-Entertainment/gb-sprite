-- | Tile-based map rendering from a sprite atlas.
--
-- Given a tile atlas (sprite sheet) and a 2D grid of tile indices,
-- renders the full tilemap as a single canvas.
module GBSprite.Tilemap
  ( -- * Types
    TilemapConfig (..),

    -- * Rendering
    renderTilemap,
  )
where

import GBSprite.Canvas (Canvas (..), getPixel, newCanvas, setPixel)
import GBSprite.Color (Color (colorA), transparent)
import GBSprite.Sheet (SheetEntry (..), SpriteSheet (..))

-- | Configuration for tilemap rendering.
data TilemapConfig = TilemapConfig
  { -- | Width of each tile in pixels
    tmTileWidth :: !Int,
    -- | Height of each tile in pixels
    tmTileHeight :: !Int,
    -- | Grid width in tiles
    tmGridWidth :: !Int,
    -- | Grid height in tiles
    tmGridHeight :: !Int,
    -- | Tile indices (row-major, index into sheet entries)
    tmTiles :: ![Int]
  }
  deriving (Show, Eq)

-- | Render a tilemap from a sprite sheet and config.
renderTilemap :: SpriteSheet -> TilemapConfig -> Canvas
renderTilemap sheet config =
  let tw = tmTileWidth config
      th = tmTileHeight config
      gw = tmGridWidth config
      gh = tmGridHeight config
      totalW = gw * tw
      totalH = gh * th
      atlas = sheetCanvas sheet
      entries = sheetEntries sheet
      blank = newCanvas totalW totalH transparent
   in foldl (drawTile atlas entries tw th gw) blank (zip [0 ..] (tmTiles config))
  where
    drawTile atlas entries tw th gw canvas (gridIdx, tileIdx)
      | tileIdx < 0 || tileIdx >= length entries = canvas
      | otherwise =
          let entry = entries !! tileIdx
              gridX = (gridIdx `mod` gw) * tw
              gridY = (gridIdx `div` gw) * th
           in stampTile canvas atlas entry gridX gridY tw th

    stampTile canvas atlas entry destX destY tw th =
      foldlCoords
        canvas
        tw
        th
        ( \c tx ty ->
            let pixel = getPixel atlas (entryX entry + tx) (entryY entry + ty)
             in if colorA pixel > 0
                  then setPixel c (destX + tx) (destY + ty) pixel
                  else c
        )

-- | Fold over tile coordinates.
foldlCoords :: Canvas -> Int -> Int -> (Canvas -> Int -> Int -> Canvas) -> Canvas
foldlCoords canvas w h f = go canvas 0 0
  where
    go c x y
      | y >= h = c
      | x >= w = go c 0 (y + 1)
      | otherwise = go (f c x y) (x + 1) y
