{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Port-aware layout of untyped string diagrams ('SDiagram').
--
-- Coordinates use chart-svg data space (y increases upward).  Morphisms
-- compose left-to-right ('SThenD'); tensor stacks top-to-bottom
-- ('SBeside').  That matches the left/right port model and the wall-chart
-- schematics — not the wording slip in the requirements bullet that had
-- composition stacking vertically.
module Strings.Svg.Layout
  ( Layout (..),
    layoutSDiagram,
    layoutWidth,
    layoutHeight,
    moveLayout,
    countCharts,
    countLines,
    countRects,
    countPaths,
    unitW,
    boxH,
    composeGap,
    tensorGap,
    portPitch,
  )
where

import Chart
import Circuit.Int.StringDiagram (SDiagram (..))
import Data.Text (pack)
import Optics.Core
import Strings.Svg.Palette
import Prelude

-- | Laid-out diagram with boundary ports and bounding box.
data Layout = Layout
  { picture :: ChartTree,
    leftPorts :: [Point Double],
    rightPorts :: [Point Double],
    bounds :: Rect Double
  }
  deriving (Eq, Show)

-- | Horizontal span of one wire / box cell.
unitW :: Double
unitW = 1.0

-- | Box height (centred on the wire).
boxH :: Double
boxH = 0.6

-- | Gap between sequential (then) cells.
composeGap :: Double
composeGap = 0.12

-- | Gap between tensor (beside) cells.
tensorGap :: Double
tensorGap = 0.35

-- | Vertical pitch between parallel wires.
portPitch :: Double
portPitch = 0.5

layoutWidth :: Layout -> Double
layoutWidth (Layout _ _ _ (Rect x z _ _)) = z - x

layoutHeight :: Layout -> Double
layoutHeight (Layout _ _ _ (Rect _ _ y w)) = w - y

-- | Translate a layout by a vector.
moveLayout :: Point Double -> Layout -> Layout
moveLayout d (Layout pic ls rs b) =
  Layout
    { picture = moveChartTree d pic,
      leftPorts = (d +) <$> ls,
      rightPorts = (d +) <$> rs,
      bounds = addPoint d b
    }

-- | chart-svg has no public moveChartTree reexport path that keeps names;
-- fold charts with moveChart.
moveChartTree :: Point Double -> ChartTree -> ChartTree
moveChartTree d = over charts' (fmap (moveChart d))

-- | Layout an untyped skeleton at the origin (first left port near @x=0@).
layoutSDiagram :: SDiagram -> Layout
layoutSDiagram = go 0
  where
    -- box colour index cycles blue / magenta for multi-box composites
    go :: Int -> SDiagram -> Layout
    go n = \case
      SWire -> wireL
      SBox lbl -> boxL n lbl
      SPrismBox -> boxL n "prism"
      SBeside a b -> besideL (go n a) (go (n + boxCount a) b)
      SThenD a b -> thenL (go n a) (go (n + boxCount a) b)
      SBend -> cupL
      SBend' -> capL
      STurn d -> turnL (go n d)
      SUnitL -> wireL
      SUnitL' -> wireL
      SUnitR -> wireL
      SUnitR' -> wireL
      SAssoc -> multiWireL 3
      SAssoc' -> multiWireL 3
      SSwap -> swapL

boxCount :: SDiagram -> Int
boxCount = \case
  SBox _ -> 1
  SPrismBox -> 1
  SBeside a b -> boxCount a + boxCount b
  SThenD a b -> boxCount a + boxCount b
  STurn d -> boxCount d
  _ -> 0

boxStroke :: Int -> Colour
boxStroke n = case n `mod` 3 of
  0 -> accentBlue
  1 -> accentMagenta
  _ -> accentGreen

--------------------------------------------------------------------------------
-- primitives
--------------------------------------------------------------------------------

wireL :: Layout
wireL =
  Layout
    { picture = unnamed [LineChart (wireStyle wireGrey) [[Point 0 0, Point unitW 0]]],
      leftPorts = [Point 0 0],
      rightPorts = [Point unitW 0],
      bounds = Rect 0 unitW (-0.05) 0.05
    }

-- | @k@ parallel identity wires spanning one unit of width.
multiWireL :: Int -> Layout
multiWireL k =
  Layout
    { picture = unnamed lines',
      leftPorts = ports,
      rightPorts = (Point unitW 0 +) <$> ports0,
      bounds = Rect 0 unitW (ymin - 0.05) (ymax + 0.05)
    }
  where
    ports0 = portYs k
    ports = ports0
    lines' =
      [ LineChart (wireStyle wireGrey) [[p, p + Point unitW 0]]
      | p <- ports0
      ]
    ys = (\(Point _ y) -> y) <$> ports0
    ymin = minimum (0 : ys)
    ymax = maximum (0 : ys)

portYs :: Int -> [Point Double]
portYs k
  | k <= 0 = []
  | k == 1 = [Point 0 0]
  | otherwise =
      let half = fromIntegral (k - 1) * portPitch / 2
       in [Point 0 (half - fromIntegral i * portPitch) | i <- [0 .. k - 1]]

boxL :: Int -> String -> Layout
boxL n lbl =
  Layout
    { picture =
        unnamed
          [ LineChart (wireStyle wireGrey) [[Point 0 0, Point x0 0]],
            LineChart (wireStyle wireGrey) [[Point x1 0, Point unitW 0]],
            RectChart (boxStyle (boxStroke n)) [Rect x0 x1 (-h2) h2],
            TextChart labelStyle [(pack lbl, Point midX 0)]
          ],
      leftPorts = [Point 0 0],
      rightPorts = [Point unitW 0],
      bounds = Rect 0 unitW (-h2 - 0.05) (h2 + 0.05)
    }
  where
    h2 = boxH / 2
    x0 = 0.18
    x1 = unitW - 0.18
    midX = (x0 + x1) / 2

-- | Cup (counit): two inputs, closes on the right.
cupL :: Layout
cupL =
  Layout
    { picture =
        unnamed
          [ PathChart
              (pathStroke accentBlue 0.04)
              [ StartP (Point 0 top),
                CubicP (Point 0.55 top) (Point 0.55 bot) (Point 0 bot)
              ]
          ],
      leftPorts = [Point 0 top, Point 0 bot],
      rightPorts = [],
      bounds = Rect 0 0.6 (bot - 0.05) (top + 0.05)
    }
  where
    top = portPitch / 2
    bot = -(portPitch / 2)

-- | Cap (unit): opens on the left into two outputs.
capL :: Layout
capL =
  Layout
    { picture =
        unnamed
          [ PathChart
              (pathStroke accentBlue 0.04)
              [ StartP (Point unitW top),
                CubicP (Point 0.45 top) (Point 0.45 bot) (Point unitW bot)
              ]
          ],
      leftPorts = [],
      rightPorts = [Point unitW top, Point unitW bot],
      bounds = Rect 0.4 unitW (bot - 0.05) (top + 0.05)
    }
  where
    top = portPitch / 2
    bot = -(portPitch / 2)

-- | Braid: under-strand continuous; over-strand gapped at the crossing.
swapL :: Layout
swapL =
  Layout
    { picture =
        unnamed
          [ -- under (magenta): bottom-left → top-right continuous
            LineChart (wireStyle accentMagenta) [[Point 0 bot, Point unitW top]],
            -- over (blue): top-left → bottom-right with gap
            LineChart (wireStyle accentBlue) [[Point 0 top, Point (cx - gap) (cy + gapY)]],
            LineChart (wireStyle accentBlue) [[Point (cx + gap) (cy - gapY), Point unitW bot]]
          ],
      leftPorts = [Point 0 top, Point 0 bot],
      rightPorts = [Point unitW top, Point unitW bot],
      bounds = Rect 0 unitW (bot - 0.05) (top + 0.05)
    }
  where
    top = portPitch / 2
    bot = -(portPitch / 2)
    cx = unitW / 2
    cy = 0
    gap = 0.08
    gapY = gap * (top - bot) / unitW

--------------------------------------------------------------------------------
-- composition
--------------------------------------------------------------------------------

-- | Sequential composition: @a@ then @b@ (left to right).
thenL :: Layout -> Layout -> Layout
thenL a b =
  Layout
    { picture = group Nothing [picture a, picture b', connectors],
      leftPorts = leftPorts a,
      rightPorts = rightPorts b',
      bounds = bounds a <> bounds b' <> connBounds
    }
  where
    -- align b so its left ports match a's right ports (first port vertical align)
    dy = alignY (rightPorts a) (leftPorts b)
    dx = rx a + composeGap - lx b
    b' = moveLayout (Point dx dy) b
    connectors = connectPorts (rightPorts a) (leftPorts b')
    connBounds =
      case rightPorts a <> leftPorts b' of
        [] -> Rect 0 0 0 0
        (p0 : ps) ->
          foldl
            (<>)
            (let Point x y = p0 in Rect x x y y)
            [let Point x y = p in Rect x x y y | p <- ps]

-- | Tensor: @a@ above @b@ (ports concatenated top-then-bottom).
besideL :: Layout -> Layout -> Layout
besideL a b =
  Layout
    { picture = group Nothing [picture a', picture b'],
      leftPorts = leftPorts a' <> leftPorts b',
      rightPorts = rightPorts a' <> rightPorts b',
      bounds = bounds a' <> bounds b'
    }
  where
    -- place a above b with tensorGap between bounding boxes
    ay = -(midY a) + layoutHeight a / 2 + tensorGap / 2
    by = -(midY b) - layoutHeight b / 2 - tensorGap / 2
    a' = moveLayout (Point (-lx a) ay) a
    b' = moveLayout (Point (-lx b) by) b

turnL :: Layout -> Layout
turnL lay =
  Layout
    { picture = over charts' (fmap (rotateChartData pi c)) (picture lay),
      -- dual: old right becomes left, reversed; old left becomes right, reversed
      leftPorts = reverse (reflect <$> rightPorts lay),
      rightPorts = reverse (reflect <$> leftPorts lay),
      bounds = reflectRect (bounds lay)
    }
  where
    c = centre (bounds lay)
    reflect (Point x y) =
      let Point cx cy = c
       in Point (2 * cx - x) (2 * cy - y)
    reflectRect (Rect x z y w) =
      let Point cx cy = c
       in Rect (2 * cx - z) (2 * cx - x) (2 * cy - w) (2 * cy - y)

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

lx :: Layout -> Double
lx (Layout _ _ _ (Rect x _ _ _)) = x

rx :: Layout -> Double
rx (Layout _ _ _ (Rect _ z _ _)) = z

midY :: Layout -> Double
midY (Layout _ _ _ (Rect _ _ y w)) = (y + w) / 2

centre :: Rect Double -> Point Double
centre (Rect x z y w) = Point ((x + z) / 2) ((y + w) / 2)

alignY :: [Point Double] -> [Point Double] -> Double
alignY (Point _ ya : _) (Point _ yb : _) = ya - yb
alignY _ _ = 0

connectPorts :: [Point Double] -> [Point Double] -> ChartTree
connectPorts as bs =
  unnamed
    [ LineChart (wireStyle wireGrey) [[p, q]]
    | (p, q) <- zip as bs,
      p /= q
    ]

rotateChartData :: Double -> Point Double -> Chart -> Chart
rotateChartData theta c = over #chartData (rotateData theta c)

rotateData :: Double -> Point Double -> ChartData -> ChartData
rotateData theta c = \case
  RectData rs -> RectData (fmap (rotateRect theta c) rs)
  LineData lss -> LineData (fmap (fmap (rotatePoint theta c)) lss)
  GlyphData ps -> GlyphData (fmap (rotatePoint theta c) ps)
  TextData ts -> TextData (fmap (fmap (rotatePoint theta c)) ts)
  PathData ps -> PathData (fmap (rotatePath theta c) ps)
  BlankData rs -> BlankData (fmap (rotateRect theta c) rs)

rotatePoint :: Double -> Point Double -> Point Double -> Point Double
rotatePoint theta (Point cx cy) (Point x y) =
  let dx = x - cx
      dy = y - cy
      ct = cos theta
      st = sin theta
   in Point (cx + ct * dx - st * dy) (cy + st * dx + ct * dy)

rotateRect :: Double -> Point Double -> Rect Double -> Rect Double
rotateRect theta c (Rect x z y w) =
  let corners = [Point x y, Point z y, Point x w, Point z w]
      cs = rotatePoint theta c <$> corners
      xs = (\(Point px _) -> px) <$> cs
      ys = (\(Point _ py) -> py) <$> cs
   in Rect (minimum xs) (maximum xs) (minimum ys) (maximum ys)

rotatePath :: Double -> Point Double -> PathData Double -> PathData Double
rotatePath theta c = \case
  StartP p -> StartP (rotatePoint theta c p)
  LineP p -> LineP (rotatePoint theta c p)
  CubicP a b p -> CubicP (rotatePoint theta c a) (rotatePoint theta c b) (rotatePoint theta c p)
  QuadP a p -> QuadP (rotatePoint theta c a) (rotatePoint theta c p)
  ArcP i p -> ArcP i (rotatePoint theta c p)

--------------------------------------------------------------------------------
-- structural counts
--------------------------------------------------------------------------------

countCharts :: Layout -> Int
countCharts = length . foldOf charts' . picture

countLines :: Layout -> Int
countLines = countPred isLine
  where
    isLine (Chart _ (LineData _)) = True
    isLine _ = False

countRects :: Layout -> Int
countRects = countPred isRect
  where
    isRect (Chart _ (RectData _)) = True
    isRect _ = False

countPaths :: Layout -> Int
countPaths = countPred isPath
  where
    isPath (Chart _ (PathData _)) = True
    isPath _ = False

countPred :: (Chart -> Bool) -> Layout -> Int
countPred p = length . filter p . foldOf charts' . picture
