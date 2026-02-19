-- | Higher-level drawing primitives.
--
-- Builds on 'GBSprite.Canvas' with thick lines, polygons,
-- ellipses, arcs, Bezier curves, and rounded rectangles.
module GBSprite.Draw
  ( -- * Thick lines
    drawThickLine,

    -- * Polygons
    drawPolygon,
    fillPolygon,

    -- * Ellipses and arcs
    drawEllipse,
    fillEllipse,
    drawArc,

    -- * Curves
    drawBezier,

    -- * Rounded rectangles
    drawRoundRect,
    fillRoundRect,
  )
where

import GBSprite.Canvas (Canvas (..), drawLine, fillCircle, fillRect, hLine, setPixel)
import GBSprite.Color (Color)

-- | Draw a thick line by drawing filled circles at each Bresenham point.
drawThickLine :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
drawThickLine canvas x0 y0 x1 y1 thickness color =
  let radius = thickness `div` 2
      points = bresenhamPoints x0 y0 x1 y1
   in foldl (\c (px, py) -> fillCircle c px py radius color) canvas points

-- | Draw a polygon outline connecting the given vertices.
drawPolygon :: Canvas -> [(Int, Int)] -> Color -> Canvas
drawPolygon canvas vertices color = case vertices of
  [] -> canvas
  [_] -> canvas
  (v : vs) ->
    let edges = zip vertices (vs ++ [v])
     in foldl (\c ((ax, ay), (bx, by)) -> drawLine c ax ay bx by color) canvas edges

-- | Fill a polygon using scanline rasterization.
fillPolygon :: Canvas -> [(Int, Int)] -> Color -> Canvas
fillPolygon canvas vertices color = case vertices of
  [] -> canvas
  [_] -> canvas
  (v : vs) ->
    let ys = map snd vertices
        minY = max 0 (minimum ys)
        maxY = min (cHeight canvas - 1) (maximum ys)
        edges = zip vertices (vs ++ [v])
     in foldl (fillScanline edges) canvas [minY .. maxY]
  where
    fillScanline edges c scanY =
      let intersections = concatMap (edgeIntersection scanY) edges
          sorted = insertionSort intersections
          pairs = takePairs sorted
       in foldl (\acc (startX, endX) -> hLine acc startX endX scanY color) c pairs

    edgeIntersection :: Int -> ((Int, Int), (Int, Int)) -> [Int]
    edgeIntersection scanY ((ax, ay), (bx, by))
      | ay == by = []
      | scanY < min ay by || scanY >= max ay by = []
      | otherwise =
          [ax + (scanY - ay) * (bx - ax) `div` (by - ay)]

    takePairs :: [Int] -> [(Int, Int)]
    takePairs (a : b : rest) = (a, b) : takePairs rest
    takePairs _ = []

    insertionSort :: [Int] -> [Int]
    insertionSort = foldl insertSorted []

    insertSorted :: [Int] -> Int -> [Int]
    insertSorted [] x = [x]
    insertSorted (y : ys) x
      | x <= y = x : y : ys
      | otherwise = y : insertSorted ys x

-- | Draw an ellipse outline using the midpoint ellipse algorithm.
drawEllipse :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawEllipse canvas cx cy rx ry color
  | rx <= 0 || ry <= 0 = setPixel canvas cx cy color
  | otherwise =
      let region1 = ellipseRegion1 canvas cx cy rx ry color 0 ry initD1
       in ellipseRegion2 region1 cx cy rx ry color rx 0 initD2
  where
    initD1 = ry * ry - rx * rx * ry + rx * rx `div` 4
    initD2 = ry * ry * rx * rx - ry * ry * rx + rx * rx `div` 4

-- | Fill an ellipse.
fillEllipse :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillEllipse canvas cx cy rx ry color
  | rx <= 0 || ry <= 0 = setPixel canvas cx cy color
  | otherwise =
      foldl (\c dy -> hLine c (cx - xWidth dy) (cx + xWidth dy) (cy + dy) color) canvas [negate ry .. ry]
  where
    rxF = fromIntegral rx :: Double
    ryF = fromIntegral ry :: Double
    rySq = ryF * ryF

    xWidth :: Int -> Int
    xWidth dy =
      let dyF = fromIntegral dy :: Double
       in round (rxF * sqrt (max 0.0 (1.0 - dyF * dyF / rySq)))

-- | Draw an arc (portion of an ellipse) between angles in degrees.
drawArc :: Canvas -> Int -> Int -> Int -> Int -> Double -> Double -> Color -> Canvas
drawArc canvas cx cy rx ry startDeg endDeg color =
  let steps = max arcMinSteps (max rx ry * arcStepsPerRadius)
      startRad = startDeg * degToRad
      endRad = endDeg * degToRad
      angleStep = (endRad - startRad) / fromIntegral steps
      points =
        [ ( cx + round (fromIntegral rx * cos (startRad + fromIntegral i * angleStep)),
            cy + round (fromIntegral ry * sin (startRad + fromIntegral i * angleStep))
          )
        | i <- [0 .. steps]
        ]
      edges = case points of
        [] -> []
        (_ : ps) -> zip points ps
   in foldl (\c ((ax, ay), (bx, by)) -> drawLine c ax ay bx by color) canvas edges
  where
    degToRad :: Double
    degToRad = pi / 180.0

    arcMinSteps :: Int
    arcMinSteps = 16

    arcStepsPerRadius :: Int
    arcStepsPerRadius = 4

-- | Draw a quadratic Bezier curve from @p0@ through @ctrl@ to @p1@.
drawBezier :: Canvas -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Color -> Canvas
drawBezier canvas (x0, y0) (ctrlX, ctrlY) (x1, y1) color =
  let points =
        [ let t = fromIntegral i / fromIntegral bezierSteps :: Double
              invT = 1.0 - t
              px = round (invT * invT * fromIntegral x0 + 2.0 * invT * t * fromIntegral ctrlX + t * t * fromIntegral x1)
              py = round (invT * invT * fromIntegral y0 + 2.0 * invT * t * fromIntegral ctrlY + t * t * fromIntegral y1)
           in (px :: Int, py :: Int)
        | i <- [0 .. bezierSteps]
        ]
      edges = case points of
        [] -> []
        (_ : ps) -> zip points ps
   in foldl (\c ((ax, ay), (bx, by)) -> drawLine c ax ay bx by color) canvas edges
  where
    bezierSteps :: Int
    bezierSteps = 32

-- | Draw a rounded rectangle outline.
drawRoundRect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
drawRoundRect canvas x y w h radius color
  | w <= 0 || h <= 0 = canvas
  | otherwise =
      let r = min radius (min (w `div` 2) (h `div` 2))
          x2 = x + w - 1
          y2 = y + h - 1
          top = drawLine canvas (x + r) y (x2 - r) y color
          bottom = drawLine top (x + r) y2 (x2 - r) y2 color
          left = drawLine bottom x (y + r) x (y2 - r) color
          right_ = drawLine left x2 (y + r) x2 (y2 - r) color
          tl = drawArc right_ (x + r) (y + r) r r 180.0 270.0 color
          tr = drawArc tl (x2 - r) (y + r) r r 270.0 360.0 color
          bl = drawArc tr (x + r) (y2 - r) r r 90.0 180.0 color
          br = drawArc bl (x2 - r) (y2 - r) r r 0.0 90.0 color
       in br

-- | Draw a filled rounded rectangle.
fillRoundRect :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
fillRoundRect canvas x y w h radius color
  | w <= 0 || h <= 0 = canvas
  | otherwise =
      let r = min radius (min (w `div` 2) (h `div` 2))
          center = fillRect canvas (x + r) y (w - 2 * r) h color
          leftStrip = fillRect center x (y + r) r (h - 2 * r) color
          rightStrip = fillRect leftStrip (x + w - r) (y + r) r (h - 2 * r) color
          tl = fillCircle rightStrip (x + r) (y + r) r color
          tr = fillCircle tl (x + w - r - 1) (y + r) r color
          bl = fillCircle tr (x + r) (y + h - r - 1) r color
          br = fillCircle bl (x + w - r - 1) (y + h - r - 1) r color
       in br

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Generate Bresenham line points.
bresenhamPoints :: Int -> Int -> Int -> Int -> [(Int, Int)]
bresenhamPoints x0 y0 x1 y1 =
  let dx = abs (x1 - x0)
      dy = negate (abs (y1 - y0))
      sx = if x0 < x1 then 1 else -1
      sy = if y0 < y1 then 1 else -1
   in go x0 y0 (dx + dy) dx dy sx sy
  where
    go cx cy err dx dy sx sy
      | cx == x1 && cy == y1 = [(cx, cy)]
      | otherwise =
          let e2 = err * 2
              (nextErr1, nextX) =
                if e2 >= dy then (err + dy, cx + sx) else (err, cx)
              (nextErr2, nextY) =
                if e2 <= dx then (nextErr1 + dx, cy + sy) else (nextErr1, cy)
           in (cx, cy) : go nextX nextY nextErr2 dx dy sx sy

-- | Ellipse region 1 (where dy/dx > -1).
ellipseRegion1 :: Canvas -> Int -> Int -> Int -> Int -> Color -> Int -> Int -> Int -> Canvas
ellipseRegion1 canvas cx cy rx ry color x y d
  | rySq * (2 * x + 1) >= rxSq * (2 * y) = canvas
  | otherwise =
      let drawn = plotEllipsePoints canvas cx cy x y color
          nextX = x + 1
          (nextD, nextY) =
            if d < 0
              then (d + rySq * (2 * nextX + 1), y)
              else (d + rySq * (2 * nextX + 1) - rxSq * (2 * y - 2), y - 1)
       in ellipseRegion1 drawn cx cy rx ry color nextX nextY nextD
  where
    rxSq = rx * rx
    rySq = ry * ry

-- | Ellipse region 2 (where dy/dx < -1).
ellipseRegion2 :: Canvas -> Int -> Int -> Int -> Int -> Color -> Int -> Int -> Int -> Canvas
ellipseRegion2 canvas cx cy _rx _ry color x y d
  | y < 0 = canvas
  | otherwise =
      let drawn = plotEllipsePoints canvas cx cy x y color
          nextY = y - 1
          (nextD, nextX) =
            if d > 0
              then (d - rxSq * (2 * nextY + 1), x)
              else (d + rySq * (2 * x + 2) - rxSq * (2 * nextY + 1), x + 1)
       in ellipseRegion2 drawn cx cy _rx _ry color nextX nextY nextD
  where
    rxSq = _rx * _rx
    rySq = _ry * _ry

-- | Plot 4 symmetric ellipse points.
plotEllipsePoints :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
plotEllipsePoints c cx cy x y color =
  setPixel
    ( setPixel
        ( setPixel
            (setPixel c (cx + x) (cy + y) color)
            (cx - x)
            (cy + y)
            color
        )
        (cx + x)
        (cy - y)
        color
    )
    (cx - x)
    (cy - y)
    color
