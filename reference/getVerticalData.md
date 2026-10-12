# Get data for a vertical section

Get data for a vertical section

## Usage

``` r
getVerticalData(x, variable, mesh, force = FALSE, ...)
```

## Arguments

- x:

  an integer vector of face indexes or an object of class `sf`.

- variable:

  Character. Name of variable

- mesh:

  an object of class `Ugrid`

- force:

  if TRUE, the cached data will be read again.

- ...:

  will be forwarded to `Ugrid$data4Face2D`
