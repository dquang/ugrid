# Generate a Maplibre GL Map

Generate a Maplibre GL Map

## Usage

``` r
genRasterMap(
  pol,
  field,
  values = NA,
  n = 5,
  style = "kmeans",
  valClass = NULL,
  mapId = "map1",
  resolution = NULL,
  legendTitle = field,
  legendWidth = "300px",
  legendPos = "top-left",
  addControls = FALSE,
  opacity = 1,
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

- mapId:

  ID of the map.

- resolution:

  Resolution of the raster.

- legendTitle:

  Legend title.

- legendWidth:

  Width of the legend.

- legendPos:

  Position of the legend.

- addControls:

  Add some controls to the map?

- opacity:

  Opacity of the legend.

- rsf:

  a list of rescaling functions returned from `rescale` function.

## Value

An HTML widget for a Mapbox map.
