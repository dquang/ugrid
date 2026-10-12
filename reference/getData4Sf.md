# Get data for faces that are intersected with or crossed by a sf object

Get data for faces that are intersected with or crossed by a sf object

## Usage

``` r
getData4Sf(
  x,
  mesh,
  variable,
  tsIdx = NULL,
  lyr = 1L,
  agg = c("none", "max", "min", "mean"),
  onlyMain = TRUE
)
```

## Arguments

- x:

  an sf object with only one feature

- mesh:

  a Ugrid object or a list of those.

- variable:

  Variable name for the data.

- tsIdx:

  Time index.

- lyr:

  layer or interface indexes, or "all" for the whole dataset.

- agg:

  Option for aggregating the data by rows. The aggregation functions
  come from `matrixStats` package.

- onlyMain:

  If TRUE, the cell elements of other domains will be removed.

## Value

sf object of polygon (faces) with values of the variable and names of
the source nc.
