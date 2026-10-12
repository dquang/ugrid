# Generate a LINESTRING sf object of velocity vectors

Generate a LINESTRING sf object of velocity vectors

## Usage

``` r
genVectorLayer(
  x,
  tsIdx = NULL,
  lyr = 1L,
  onlyMain = FALSE,
  baseLength = NULL,
  aRatio = 0.2,
  aOpen = 30,
  aSide = 2,
  fillValue = NULL
)
```

## Arguments

- x:

  a Ugrid object.

- tsIdx:

  Index on time step.

- lyr:

  layer index.

- onlyMain:

  If TRUE, only elements of main domain will be taken.

- baseLength:

  Base length of the vector.

- aRatio:

  A ratio between vector head and vector length.

- aOpen:

  Open angle in degree of vector head.

- aSide:

  Vector head to both side or only one?

- fillValue:

  The fill value of the variable in the NetCDF.
