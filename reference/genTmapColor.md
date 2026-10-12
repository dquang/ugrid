# Color classification for leaflet map

Color classification for leaflet map

## Usage

``` r
genTmapColor(
  values = NA,
  n = 5,
  style = "kmeans",
  colPal = "seaborn.bright",
  colType = "seq",
  reverse = FALSE
)
```

## Arguments

- values:

  A numeric vector of values to classify

- n:

  Number of classes

- style:

  Style of classification. One of: "fixed", "sd", "equal", "pretty",
  "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih",
  "headtails", "maximum", or "box".

- colPal:

  Full name of a color palette.

- colType:

  Type of palette: "cat", "seq", "div" or "cyc".

- reverse:

  Should the palette be reversed?
