# Get data for faces

Get data for faces

## Usage

``` r
getMapData(
  mesh,
  variable,
  lyr = 1L,
  tsIdx = 1L,
  agg = "none",
  force = FALSE,
  onlyMain = FALSE,
  dryAsNa = TRUE
)
```

## Arguments

- mesh:

  a Ugrid object or a list of those.

- variable:

  Variable name for the data.

- lyr:

  layer or interface indexes, or "all" for the whole dataset.

- tsIdx:

  Time index.

- agg:

  Option for aggregating the data by rows. The aggregation functions
  come from `matrixStats` package.

- force:

  If TRUE, the data stored in Ugrid object, if any, will be read again.

- onlyMain:

  If TRUE, the cell elements of other domains will be removed.

- dryAsNa:

  If TRUE (default), values for dried (waterlevel - elevation \< 0)
  cells will be assigned NaN.

## Value

A sf object.
