# Get data for faces

Get data for faces

## Usage

``` r
raster4Meshes(
  mesh,
  variable,
  tsIdx = 1L,
  agg = "none",
  lyr = 1L,
  force = FALSE,
  onlyMain = FALSE
)
```

## Arguments

- mesh:

  a Ugrid object or a list of those.

- variable:

  Variable name for the data.

- tsIdx:

  Time index.

- agg:

  Option for aggregating the data by rows. The aggregation functions
  come from `matrixStats` package.

- lyr:

  layer or interface index.

- force:

  If TRUE, the data stored in Ugrid object, if any, will be read again.

- onlyMain:

  If TRUE, the cell elements of other domains will be removed.

## Value

A sf object.
