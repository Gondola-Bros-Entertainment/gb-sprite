-- | Higher-level drawing primitives.
--
-- Builds on "GBSprite.Canvas" with thick lines, polygons,
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
    drawCubicBezier,
    drawCatmullRom,

    -- * Anti-aliased lines
    drawAALine,

    -- * Pattern fill
    patternFill,

    -- * Rounded rectangles
    drawRoundRect,
    fillRoundRect,
  )
where

import Data.List (foldl')
import Data.Word (Word8)
import GBSprite.Canvas
  ( Canvas (..),
    bulkBlendPixels,
    bulkHSpans,
    bulkSetPixels,
    drawLine,
    fillCircle,
    fillRect,
    getPixel,
    setPixel,
  )
import GBSprite.Color (Color (..), alphaBlend, withAlpha)

-- | Draw a thick line by drawing filled circles at each Bresenham point.
drawThickLine :: Canvas -> Int -> Int -> Int -> Int -> Int -> Color -> Canvas
drawThickLine canvas x0 y0 x1 y1 thickness color =
  let radius = thickness `div` 2
      points = bresenhamPoints x0 y0 x1 y1
   in foldl' (\c (px, py) -> fillCircle c px py radius color) canvas points

-- | Draw a polygon outline connecting the given vertices.
drawPolygon :: Canvas -> [(Int, Int)] -> Color -> Canvas
drawPolygon canvas vertices color = case vertices of
  [] -> canvas
  [_] -> canvas
  (v : vs) ->
    let edges = zip vertices (vs ++ [v])
        allPixels =
          concatMap
            ( \((ax, ay), (bx, by)) ->
                map (\(px, py) -> (px, py, color)) (bresenhamPoints ax ay bx by)
            )
            edges
     in bulkSetPixels canvas allPixels

-- | Fill a polygon using scanline rasterization.
fillPolygon :: Canvas -> [(Int, Int)] -> Color -> Canvas
fillPolygon canvas vertices color = case vertices of
  [] -> canvas
  [_] -> canvas
  (v : vs) ->
    let ys = map snd vertices
        minY = max 0 (foldl1' min ys)
        maxY = min (cHeight canvas - 1) (foldl1' max ys)
        edges = zip vertices (vs ++ [v])
        allSpans = concatMap (computeScanlineSpans edges) [minY .. maxY]
     in bulkHSpans canvas color allSpans
  where
    foldl1' f (z : zs) = foldl' f z zs
    foldl1' _ [] = 0

    computeScanlineSpans edges scanY =
      let intersections = concatMap (edgeIntersection scanY) edges
          sorted = insertionSort intersections
          pairs = takePairs sorted
       in map (\(startX, endX) -> (startX, endX, scanY)) pairs

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
    insertionSort = foldl' insertSorted []

    insertSorted :: [Int] -> Int -> [Int]
    insertSorted [] x = [x]
    insertSorted (y : ys) x
      | x <= y = x : y : ys
      | otherwise = y : insertSorted ys x

-- | Draw an ellipse outline using the midpoint ellipse algorithm.
drawEllipse :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawEllipse canvas cx cy rx ry color
  | rx <= 0 || ry <= 0 = setPixel canvas cx cy color
  | otherwise = bulkSetPixels canvas allPixels
  where
    rxSq = rx * rx
    rySq = ry * ry
    initD1 = rySq - rxSq * ry + rxSq `div` 4
    (r1Pixels, endX, endY) = collectRegion1 rySq rxSq cx cy 0 ry initD1 color
    d2 = rySq * (endX * endX + endX) + rxSq * (endY - 1) * (endY - 1) - rxSq * rySq + rySq `div` 4
    r2Pixels = collectRegion2 rxSq rySq cx cy endX endY d2 color
    allPixels = r1Pixels ++ r2Pixels

-- | Fill an ellipse.
fillEllipse :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
fillEllipse canvas cx cy rx ry color
  | rx <= 0 || ry <= 0 = setPixel canvas cx cy color
  | otherwise = bulkHSpans canvas color spans
  where
    rxF = fromIntegral rx :: Double
    ryF = fromIntegral ry :: Double
    rySq = ryF * ryF

    xWidth :: Int -> Int
    xWidth dy =
      let dyF = fromIntegral dy :: Double
       in round (rxF * sqrt (max 0.0 (1.0 - dyF * dyF / rySq)))

    spans = [(cx - xWidth dy, cx + xWidth dy, cy + dy) | dy <- [negate ry .. ry]]

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
      allPixels =
        concatMap
          ( \((ax, ay), (bx, by)) ->
              map (\(px, py) -> (px, py, color)) (bresenhamPoints ax ay bx by)
          )
          edges
   in bulkSetPixels canvas allPixels
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
      allPixels =
        concatMap
          ( \((ax, ay), (bx, by)) ->
              map (\(px, py) -> (px, py, color)) (bresenhamPoints ax ay bx by)
          )
          edges
   in bulkSetPixels canvas allPixels
  where
    bezierSteps :: Int
    bezierSteps = 32

-- | Draw a cubic Bezier curve from @p0@ through controls @c1@ and @c2@ to @p1@.
drawCubicBezier :: Canvas -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Color -> Canvas
drawCubicBezier canvas (x0, y0) (cx1, cy1) (cx2, cy2) (x1, y1) color =
  let points =
        [ let t = fromIntegral i / fromIntegral cubicBezierSteps :: Double
              invT = 1.0 - t
              invT2 = invT * invT
              invT3 = invT2 * invT
              t2 = t * t
              t3 = t2 * t
              px = round (invT3 * fromIntegral x0 + 3.0 * invT2 * t * fromIntegral cx1 + 3.0 * invT * t2 * fromIntegral cx2 + t3 * fromIntegral x1)
              py = round (invT3 * fromIntegral y0 + 3.0 * invT2 * t * fromIntegral cy1 + 3.0 * invT * t2 * fromIntegral cy2 + t3 * fromIntegral y1)
           in (px :: Int, py :: Int)
        | i <- [0 .. cubicBezierSteps]
        ]
      edges = case points of
        [] -> []
        (_ : ps) -> zip points ps
      allPixels =
        concatMap
          ( \((ax, ay), (bx, by)) ->
              map (\(px, py) -> (px, py, color)) (bresenhamPoints ax ay bx by)
          )
          edges
   in bulkSetPixels canvas allPixels

-- | Draw a Catmull-Rom spline through the given control points.
-- Requires at least 4 points; with fewer, draws straight lines.
drawCatmullRom :: Canvas -> [(Int, Int)] -> Color -> Canvas
drawCatmullRom canvas points color = case points of
  [] -> canvas
  [_] -> canvas
  [(ax, ay), (bx, by)] -> drawLine canvas ax ay bx by color
  [(ax, ay), (bx, by), (cx_, cy_)] ->
    drawLine (drawLine canvas ax ay bx by color) bx by cx_ cy_ color
  _ ->
    let segments = catmullRomSegments points
        allPoints = concatMap (catmullRomEvaluate catmullRomSteps) segments
        edges = case allPoints of
          [] -> []
          (_ : ps) -> zip allPoints ps
        allPixels =
          concatMap
            ( \((ax, ay), (bx, by)) ->
                map (\(px, py) -> (px, py, color)) (bresenhamPoints ax ay bx by)
            )
            edges
     in bulkSetPixels canvas allPixels

-- | Draw an anti-aliased line using Wu's algorithm.
drawAALine :: Canvas -> Int -> Int -> Int -> Int -> Color -> Canvas
drawAALine canvas x0 y0 x1 y1 color =
  let steep = abs (y1 - y0) > abs (x1 - x0)
      (ax, ay, bx, by) =
        if steep
          then
            let (sx, sy, ex, ey) = if y0 > y1 then (x1, y1, x0, y0) else (x0, y0, x1, y1)
             in (sy, sx, ey, ex)
          else
            let (sx, sy, ex, ey) = if x0 > x1 then (x1, y1, x0, y0) else (x0, y0, x1, y1)
             in (sx, sy, ex, ey)
      dx = bx - ax
      dy = by - ay
      gradient = if dx == 0 then 1.0 else fromIntegral dy / fromIntegral dx :: Double
      pixels = collectAAPixels canvas steep color ax bx (fromIntegral ay) gradient
   in bulkBlendPixels canvas pixels

-- | Fill a rectangular area with a repeating pattern from a source canvas.
patternFill :: Canvas -> Int -> Int -> Int -> Int -> Canvas -> Canvas
patternFill canvas rx ry rw rh patCanvas
  | rw <= 0 || rh <= 0 = canvas
  | otherwise =
      let patW = max 1 (cWidth patCanvas)
          patH = max 1 (cHeight patCanvas)
          pixels =
            [ (rx + col, ry + row, pixel)
            | row <- [0 .. rh - 1],
              col <- [0 .. rw - 1],
              let px = col `mod` patW
                  py = row `mod` patH
                  pixel = getPixel patCanvas px py,
              colorA pixel > 0
            ]
       in bulkSetPixels canvas pixels

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

cubicBezierSteps :: Int
cubicBezierSteps = 48

catmullRomSteps :: Int
catmullRomSteps = 16

catmullRomTension :: Double
catmullRomTension = 0.5

catmullRomSegments :: [(Int, Int)] -> [((Int, Int), (Int, Int), (Int, Int), (Int, Int))]
catmullRomSegments pts = case pts of
  (a : b : c : d : rest) -> (a, b, c, d) : catmullRomSegments (b : c : d : rest)
  _ -> []

catmullRomEvaluate :: Int -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> [(Int, Int)]
catmullRomEvaluate steps ((x0, y0), (x1, y1), (x2, y2), (x3, y3)) =
  [ let t = fromIntegral i / fromIntegral steps :: Double
        t2 = t * t
        t3 = t2 * t
        px =
          round
            ( catmullRomTension
                * ( 2.0 * fromIntegral x1
                      + (fromIntegral x2 - fromIntegral x0) * t
                      + (2.0 * fromIntegral x0 - 5.0 * fromIntegral x1 + 4.0 * fromIntegral x2 - fromIntegral x3) * t2
                      + (negate (fromIntegral x0) + 3.0 * fromIntegral x1 - 3.0 * fromIntegral x2 + fromIntegral x3) * t3
                  )
            )
        py =
          round
            ( catmullRomTension
                * ( 2.0 * fromIntegral y1
                      + (fromIntegral y2 - fromIntegral y0) * t
                      + (2.0 * fromIntegral y0 - 5.0 * fromIntegral y1 + 4.0 * fromIntegral y2 - fromIntegral y3) * t2
                      + (negate (fromIntegral y0) + 3.0 * fromIntegral y1 - 3.0 * fromIntegral y2 + fromIntegral y3) * t3
                  )
            )
     in (px :: Int, py :: Int)
  | i <- [0 .. steps]
  ]

-- | Collect anti-aliased line pixels (Wu's algorithm).
collectAAPixels :: Canvas -> Bool -> Color -> Int -> Int -> Double -> Double -> [(Int, Int, Color)]
collectAAPixels canvas steep lineColor startX endX yInter gradient =
  go startX yInter
  where
    go x intery
      | x > endX = []
      | otherwise =
          let iy = floor intery :: Int
              frac = intery - fromIntegral iy
              invFrac = 1.0 - frac
              p1 = mkAAPixel canvas steep x iy lineColor invFrac
              p2 = mkAAPixel canvas steep x (iy + 1) lineColor frac
           in p1 ++ p2 ++ go (x + 1) (intery + gradient)

mkAAPixel :: Canvas -> Bool -> Int -> Int -> Color -> Double -> [(Int, Int, Color)]
mkAAPixel canvas steep x y lineColor intensity
  | intensity <= 0 = []
  | otherwise =
      let (px, py) = if steep then (y, x) else (x, y)
          bg = getPixel canvas px py
          fg = withAlpha (clampByte (round (fromIntegral (colorA lineColor) * intensity))) lineColor
          blended = alphaBlend fg bg
       in [(px, py, blended)]

clampByte :: Int -> Word8
clampByte n = fromIntegral (max 0 (min aaMaxChannel n))

aaMaxChannel :: Int
aaMaxChannel = 255

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

-- | Collect ellipse region 1 outline pixels (where dy/dx > -1).
collectRegion1 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Color -> ([(Int, Int, Color)], Int, Int)
collectRegion1 rySq rxSq cx cy x y d color
  | rySq * (2 * x + 1) >= rxSq * (2 * y) =
      (ellipseQuadPixels cx cy x y color, x, y)
  | otherwise =
      let pixels = ellipseQuadPixels cx cy x y color
          nextX = x + 1
          (nextD, nextY) =
            if d < 0
              then (d + rySq * (2 * nextX + 1), y)
              else (d + rySq * (2 * nextX + 1) - rxSq * (2 * y - 2), y - 1)
          (restPixels, endX, endY) = collectRegion1 rySq rxSq cx cy nextX nextY nextD color
       in (pixels ++ restPixels, endX, endY)

-- | Collect ellipse region 2 outline pixels (where dy/dx < -1).
collectRegion2 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Color -> [(Int, Int, Color)]
collectRegion2 rxSq rySq cx cy x y d color
  | y < 0 = []
  | otherwise =
      let pixels = ellipseQuadPixels cx cy x y color
          nextY = y - 1
          (nextD, nextX) =
            if d > 0
              then (d - rxSq * (2 * nextY + 1), x)
              else (d + rySq * (2 * x + 2) - rxSq * (2 * nextY + 1), x + 1)
       in pixels ++ collectRegion2 rxSq rySq cx cy nextX nextY nextD color

-- | Generate the 4 symmetric ellipse outline points.
ellipseQuadPixels :: Int -> Int -> Int -> Int -> Color -> [(Int, Int, Color)]
ellipseQuadPixels cx cy x y color =
  [ (cx + x, cy + y, color),
    (cx - x, cy + y, color),
    (cx + x, cy - y, color),
    (cx - x, cy - y, color)
  ]
