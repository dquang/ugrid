# Color classification for leaflet map

Color classification for leaflet map

## Usage

``` r
genColorInfo(
  values = NA,
  pol = NULL,
  field = NULL,
  n = 5,
  style = "kmeans",
  valClass = NULL,
  colPal = "seaborn.bright",
  colType = "cat",
  reverse = FALSE,
  opacity = 1
)
```

## Arguments

- values:

  A numeric vector of values to classify

- pol:

  a data.frame.

- field:

  The name of the column to use for classification.

- n:

  Number of classes

- style:

  Style of classification. One of: "fixed", "sd", "equal", "pretty",
  "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih",
  "headtails", "maximum", or "box".

- valClass:

  An object of the class `classIntervals` to set the classification
  manually

- colPal:

  Full name of a color palette.

- colType:

  Type of palette: "cat", "seq", "div" or "cyc".

- reverse:

  Should the palette be reversed?

- opacity:

  Legend opacity
