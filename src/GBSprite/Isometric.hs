-- | Isometric projection utilities: coordinate conversion, hit testing,
-- depth sorting, diamond drawing, and tilemap rendering.
--
-- All functions are pure. Coordinates use @(Int, Int)@ with screen origin
-- at top-left, x increasing right, y increasing down. World coordinates
-- are grid positions with @(0, 0)@ at the back corner of the map.
module GBSprite.Isometric
  ( -- * Configuration
    IsoConfig (..),
    defaultIsoConfig,

    -- * Coordinate conversion
    worldToScreen,
    screenToWorld,

    -- * Hit testing
    pointInDiamond,

    -- * Depth sorting
    isoDepthCompare,

    -- * Drawing
    drawDiamond,
    fillDiamond,

    -- * Map rendering
    renderIsoMap,
  )
where

import Data.List (foldl', sortBy)
import GBSprite.Canvas (Canvas (..), drawLine, hLine, newCanvas)
import GBSprite.Color (Color, transparent)
import GBSprite.Compose (stamp)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Isometric tile dimensions in pixels.
--
-- Standard 2:1 isometric uses a width-to-height ratio of 2:1
-- (e.g. 64 wide by 32 tall).
data IsoConfig = IsoConfig
  { -- | Full tile width in pixels (diamond tip to tip, horizontally)
    isoTileWidth :: !Int,
    -- | Full tile height in pixels (diamond tip to tip, vertically)
    isoTileHeight :: !Int
  }
  deriving (Show, Eq)

-- | Standard 2:1 isometric: 64 pixels wide, 32 pixels tall.
defaultIsoConfig :: IsoConfig
defaultIsoConfig =
  IsoConfig
    { isoTileWidth = defaultTileWidth,
      isoTileHeight = defaultTileHeight
    }

-- | Default tile width for standard 2:1 isometric projection.
defaultTileWidth :: Int
defaultTileWidth = 64

-- | Default tile height for standard 2:1 isometric projection.
defaultTileHeight :: Int
defaultTileHeight = 32

-- ---------------------------------------------------------------------------
-- Coordinate conversion
-- ---------------------------------------------------------------------------

-- | Convert world grid coordinates to screen pixel position.
--
-- Uses the standard isometric transform:
--
-- @
-- screenX = (wx - wy) * (tileWidth \/ 2)
-- screenY = (wx + wy) * (tileHeight \/ 2)
-- @
worldToScreen :: IsoConfig -> Int -> Int -> (Int, Int)
worldToScreen config wx wy =
  let halfW = isoTileWidth config `div` tileHalvingFactor
      halfH = isoTileHeight config `div` tileHalvingFactor
      screenX = (wx - wy) * halfW
      screenY = (wx + wy) * halfH
   in (screenX, screenY)

-- | Convert screen pixel position back to world grid coordinates.
--
-- Uses the inverse isometric transform with rounding for integer
-- conversion:
--
-- @
-- worldX = (screenX \/ halfW + screenY \/ halfH) \/ 2
-- worldY = (screenY \/ halfH - screenX \/ halfW) \/ 2
-- @
screenToWorld :: IsoConfig -> Int -> Int -> (Int, Int)
screenToWorld config screenX screenY =
  let halfW = isoTileWidth config `div` tileHalvingFactor
      halfH = isoTileHeight config `div` tileHalvingFactor
      ratioX = fromIntegral screenX / fromIntegral (max 1 halfW) :: Double
      ratioY = fromIntegral screenY / fromIntegral (max 1 halfH) :: Double
      worldX = round ((ratioX + ratioY) / inverseScaleFactor)
      worldY = round ((ratioY - ratioX) / inverseScaleFactor)
   in (worldX, worldY)

-- | Factor for computing half-tile dimensions.
tileHalvingFactor :: Int
tileHalvingFactor = 2

-- | Divisor used in the inverse isometric transform.
inverseScaleFactor :: Double
inverseScaleFactor = 2.0

-- ---------------------------------------------------------------------------
-- Hit testing
-- ---------------------------------------------------------------------------

-- | Test whether a screen point lies inside an isometric diamond.
--
-- The diamond is positioned at screen coordinates @(dx, dy)@ with its
-- top-left bounding box corner there. The test uses the diamond
-- inequality: a point is inside if
--
-- @
-- abs (px - cx) \/ halfW + abs (py - cy) \/ halfH <= 1
-- @
--
-- where @(cx, cy)@ is the diamond centre.
pointInDiamond ::
  IsoConfig ->
  -- | Screen point x
  Int ->
  -- | Screen point y
  Int ->
  -- | Diamond position x (top-left of bounding box)
  Int ->
  -- | Diamond position y (top-left of bounding box)
  Int ->
  Bool
pointInDiamond config px py dx dy =
  let halfW = isoTileWidth config `div` tileHalvingFactor
      halfH = isoTileHeight config `div` tileHalvingFactor
      cx = dx + halfW
      cy = dy + halfH
      distX = abs (px - cx)
      distY = abs (py - cy)
   in distX * halfH + distY * halfW <= halfW * halfH

-- ---------------------------------------------------------------------------
-- Depth sorting
-- ---------------------------------------------------------------------------

-- | Compare two world positions for isometric depth sorting.
--
-- Tiles with a larger sum of @(wx + wy)@ are drawn later (in front).
-- When sums are equal, the tile with the larger @wy@ is in front.
isoDepthCompare :: (Int, Int) -> (Int, Int) -> Ordering
isoDepthCompare (wx1, wy1) (wx2, wy2) =
  let sum1 = wx1 + wy1
      sum2 = wx2 + wy2
   in case compare sum1 sum2 of
        EQ -> compare wy1 wy2
        other -> other

-- ---------------------------------------------------------------------------
-- Drawing
-- ---------------------------------------------------------------------------

-- | Draw an isometric diamond outline at a screen position.
--
-- The diamond is drawn inside the bounding box with its top-left corner
-- at @(sx, sy)@. Four lines connect the cardinal points:
-- top, right, bottom, left.
drawDiamond :: Canvas -> IsoConfig -> Int -> Int -> Color -> Canvas
drawDiamond canvas config sx sy color =
  let tw = isoTileWidth config
      th = isoTileHeight config
      halfW = tw `div` tileHalvingFactor
      halfH = th `div` tileHalvingFactor
      topX = sx + halfW
      topY = sy
      rightX = sx + tw
      rightY = sy + halfH
      bottomX = sx + halfW
      bottomY = sy + th
      leftX = sx
      leftY = sy + halfH
      withTopRight = drawLine canvas topX topY rightX rightY color
      withRightBottom = drawLine withTopRight rightX rightY bottomX bottomY color
      withBottomLeft = drawLine withRightBottom bottomX bottomY leftX leftY color
      withLeftTop = drawLine withBottomLeft leftX leftY topX topY color
   in withLeftTop

-- | Fill an isometric diamond at a screen position using scanline fill.
--
-- For each scanline row from the diamond top to bottom, computes the
-- left and right edges and draws a horizontal line between them.
fillDiamond :: Canvas -> IsoConfig -> Int -> Int -> Color -> Canvas
fillDiamond canvas config sx sy color =
  let tw = isoTileWidth config
      th = isoTileHeight config
      halfW = tw `div` tileHalvingFactor
      halfH = th `div` tileHalvingFactor
   in foldl' (fillScanline halfW halfH) canvas [0 .. th]
  where
    fillScanline hw hh c row =
      let diamondCenterX = sx + hw
          edgeSpan =
            if row <= hh
              then -- Upper half: expanding from top
                hw * row `div` max 1 hh
              else -- Lower half: contracting toward bottom
                let rowsFromBottom = isoTileHeight config - row
                 in hw * rowsFromBottom `div` max 1 hh
          edgeLeft = diamondCenterX - edgeSpan
          edgeRight = diamondCenterX + edgeSpan
       in hLine c edgeLeft edgeRight (sy + row) color

-- ---------------------------------------------------------------------------
-- Map rendering
-- ---------------------------------------------------------------------------

-- | Render an isometric tilemap.
--
-- Takes an 'IsoConfig', a list of indexed tile canvases
-- @[(tileIndex, Canvas)]@, and a 2D grid of tile indices (row-major,
-- outer list is rows). Tiles are rendered back-to-front according to
-- isometric depth order. The output canvas is sized to fit all tiles.
--
-- Grid indices that do not appear in the tile list are skipped.
-- An empty grid produces a 1x1 transparent canvas.
renderIsoMap ::
  IsoConfig ->
  -- | Indexed tile canvases
  [(Int, Canvas)] ->
  -- | 2D grid of tile indices (row-major)
  [[Int]] ->
  Canvas
renderIsoMap config tiles grid =
  let gridRows = length grid
      gridCols = maxRowLength grid
   in if gridRows == 0 || gridCols == 0
        then newCanvas 1 1 transparent
        else
          let tw = isoTileWidth config
              th = isoTileHeight config
              halfW = tw `div` tileHalvingFactor
              halfH = th `div` tileHalvingFactor
              -- The total canvas width spans from the leftmost diamond
              -- to the rightmost diamond tip.
              totalWidth = (gridRows + gridCols) * halfW
              -- The total canvas height spans from the topmost diamond
              -- to the bottommost diamond tip.
              totalHeight = (gridRows + gridCols) * halfH
              -- Offset so that world (0,0) maps to the top-centre
              originOffsetX = (gridRows - 1) * halfW
              originOffsetY = 0
              blank = newCanvas totalWidth totalHeight transparent
              -- Build a sorted list of (worldX, worldY, tileIndex)
              -- for back-to-front rendering
              cells = sortBy depthOrder (gridCells grid)
           in foldl' (placeTile tiles config originOffsetX originOffsetY) blank cells
  where
    depthOrder (wx1, wy1, _) (wx2, wy2, _) = isoDepthCompare (wx1, wy1) (wx2, wy2)

-- | Extract all grid cells as @(worldX, worldY, tileIndex)@ triples.
gridCells :: [[Int]] -> [(Int, Int, Int)]
gridCells rows = concatMap expandRow (zip [0 ..] rows)
  where
    expandRow (wy, cols) = zipWith (\wx tileIdx -> (wx, wy, tileIdx)) [0 ..] cols

-- | Place a single tile on the output canvas.
placeTile ::
  [(Int, Canvas)] ->
  IsoConfig ->
  Int ->
  Int ->
  Canvas ->
  (Int, Int, Int) ->
  Canvas
placeTile tiles config originX originY canvas (wx, wy, tileIdx) =
  case lookup tileIdx tiles of
    Nothing -> canvas
    Just tileCanvas ->
      let (rawScreenX, rawScreenY) = worldToScreen config wx wy
          screenX = rawScreenX + originX
          screenY = rawScreenY + originY
       in stamp canvas screenX screenY tileCanvas

-- | Compute the maximum row length from a 2D grid, returning 0 for empty.
maxRowLength :: [[a]] -> Int
maxRowLength = foldl' (\acc row -> max acc (length row)) 0
