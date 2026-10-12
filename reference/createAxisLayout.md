# Create axis list for plotly layout

Create axis list for plotly layout

## Usage

``` r
createAxisLayout(
  y1Range,
  y2Range = NULL,
  y1Name = "W",
  y2Name = "Q",
  nTick = 6,
  fontSize = 14,
  fontFamily = "Arial"
)
```

## Arguments

- y1Range, y2Range:

  Ranges of y1, y2 values.

- y1Name, y2Name:

  Names of y1, y2 axes.

- nTick:

  Number of ticks.

- fontSize:

  Font size.

- fontFamily:

  Font family.

## Value

List of axis-parameters for the `plotly::layout` function.
