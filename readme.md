# strings-svg

Render compact-closed string diagrams from `circuits-int` as SVG via `chart-svg`.

```haskell
import Circuit.Int.StringDiagram (skeleton, wire, boxLabelled, thenD)
import Strings.Svg (renderSDiagram, writeChartOptions)

writeChartOptions "out.svg" (renderSDiagram (skeleton d))
```

Regenerate the equality gallery:

```bash
cabal run render-examples -- other/
```

Order of record: `coffee/buff/string-diagram-renderer-requirements.md`.
