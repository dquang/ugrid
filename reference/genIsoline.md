# Generate isolines from a sf object.

Generate isolines from a sf object.

## Usage

``` r
genIsoline(x, field, resolution = 10, nbin = 10, binWidth = NULL, asSf = TRUE)
```

## Arguments

- x:

  sf object that can form a regular grid.

- field:

  Field for values.

- resolution:

  Numeric vector of length 1 or 2 to set the spatial resolution.

- nbin:

  Number of bins

- binWidth:

  Bin-width, if given, nbin will be ignored.

- asSf:

  If TRUE, the generated raster will be converted to sf.

## Value

a SpatVector or sf LINESTRING vector object.
