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

import Data.List (foldl')
import GBSprite.Canvas (Canvas (..), bulkSetPixels, getPixel, newCanvas)
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
   in foldl' (drawTile atlas entries tw th gw) blank (zip [0 ..] (tmTiles config))
  where
    drawTile atlas entries tw th gw canvas (gridIdx, tileIdx)
      | tileIdx < 0 = canvas
      | otherwise = case drop tileIdx entries of
          (entry : _) ->
            let gridX = (gridIdx `mod` gw) * tw
                gridY = (gridIdx `div` gw) * th
             in stampTile canvas atlas entry gridX gridY tw th
          [] -> canvas

    stampTile canvas atlas entry destX destY tw th =
      let pixels =
            [ (destX + tx, destY + ty, pixel)
            | ty <- [0 .. th - 1],
              tx <- [0 .. tw - 1],
              let pixel = getPixel atlas (entryX entry + tx) (entryY entry + ty),
              colorA pixel > 0
            ]
       in bulkSetPixels canvas pixels
