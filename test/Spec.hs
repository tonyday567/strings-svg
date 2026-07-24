-- | Structural oracles for string-diagram layout.
module Main (main) where

import Circuit.Poly.StringDiagram
  ( SDiagram (..),
    beside,
    boxLabelled,
    skeleton,
    thenD,
    wire,
  )
import Circuit.Poly (lens)
import Strings.Svg.Layout
import System.Exit (exitFailure)
import Prelude

assert :: String -> Bool -> IO ()
assert msg ok =
  if ok
    then putStrLn ("  PASS " ++ msg)
    else do
      putStrLn ("  FAIL " ++ msg)
      exitFailure

approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 0.08

main :: IO ()
main = do
  putStrLn "strings-svg structural oracles"

  let w = layoutSDiagram (skeleton wire)
  assert "skeleton wire is one line" $ countLines w == 1 && countRects w == 0

  let b = layoutSDiagram (skeleton (boxLabelled "f" (lens id (\_ da -> da))))
  assert "skeleton box is one rect + two line segments" $
    countRects b == 1 && countLines b == 2

  let d1 = layoutSDiagram SWire
      d2 = layoutSDiagram SWire
      ten = layoutSDiagram (SBeside SWire SWire)
      seq' = layoutSDiagram (SThenD SWire SWire)
  assert "beside height ≈ h1 + gap + h2" $
    approx (layoutHeight ten) (layoutHeight d1 + tensorGap + layoutHeight d2)
  assert "thenD width ≈ w1 + gap + w2" $
    approx (layoutWidth seq') (layoutWidth d1 + composeGap + layoutWidth d2)

  let cup = layoutSDiagram SBend
      cap = layoutSDiagram SBend'
  assert "bend (cup) is one path" $ countPaths cup == 1
  assert "bend' (cap) is one path" $ countPaths cap == 1

  let sw = layoutSDiagram SSwap
  assert "swap has crossing line segments (≥2)" $ countLines sw >= 2

  let typedBeside = layoutSDiagram (skeleton (beside wire wire))
      typedThen = layoutSDiagram (skeleton (thenD wire wire))
  assert "typed beside skeleton layouts" $ layoutHeight typedBeside > layoutHeight d1
  assert "typed thenD skeleton layouts" $ layoutWidth typedThen > layoutWidth d1

  putStrLn "all structural oracles passed"
