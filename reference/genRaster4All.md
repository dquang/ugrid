# Generate value rasters of one variable for all time steps

Generate value rasters of one variable for all time steps

## Usage

``` r
genRaster4All(
  mesh,
  variable = "sea_surface_height",
  lyr = 1L,
  folder = tempdir(),
  resolution = NULL,
  dryAsNa = TRUE
)
```

## Arguments

- mesh:

  List of Ugrid objects

- variable:

  NetCDF variable to get values

- lyr:

  layer index.

- folder:

  Output folder

- resolution:

  Raster resolution

- dryAsNa:

  If TRUE, the dry area of "sea_surface_height" will be assigned to NaN
