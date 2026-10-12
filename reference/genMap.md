# Generate a Maplibre GL Map

Generate a Maplibre GL Map

## Usage

``` r
genMap(
  pol,
  field,
  values = NA,
  n = 5,
  style = "kmeans",
  valClass = NULL,
  colPal = "seaborn.bright",
  colType = "cat",
  mapId = "map1",
  continuous = TRUE,
  reverse = FALSE,
  legendTitle = field,
  legendWidth = "300px",
  legendPos = "top-left",
  addControls = FALSE,
  addLine = FALSE,
  lineColor = "grey",
  lineWidth = 0.1,
  rsf = NULL
)
```

## Arguments

- pol:

  A sf object.

- field:

  The name of the column to use for classification.

- values:

  values of the dataset.

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

- mapId:

  ID of the map.

- continuous:

  TRUE for continuous and FALSE for categorical legend.

- reverse:

  Should the palette be reversed?

- legendTitle:

  Legend title.

- legendWidth:

  Width of the legend.

- legendPos:

  Position of the legend.

- addControls:

  Add some controls to the map?

- addLine:

  Add polygon line?

- lineColor:

  Line color of polygons.

- lineWidth:

  Line width of polygons.

- rsf:

  a list of rescaling functions returned from `rescale` function.

## Value

An HTML widget for a Mapbox map.
