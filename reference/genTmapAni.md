# Generate animation GIF of tmap from list of rasters

Generate animation GIF of tmap from list of rasters

## Usage

``` r
genTmapAni(
  rasLst,
  fname = tempfile(fileext = ".gif"),
  rNames = NULL,
  legendTitle = "Value",
  n = 5,
  style = "kmeans",
  colPal = "seaborn.bright",
  colType = "seq",
  reverse = FALSE
)
```

## Arguments

- rasLst:

  List of raster files.

- fname:

  Output file name.

- rNames:

  Names of raster layers.

- legendTitle:

  Title for map legend.

- n:

  Number of color classes.

- style:

  Style for color classification.

- colPal:

  Color palette name.

- colType:

  Color type.

- reverse:

  Should color palette be reversed?
