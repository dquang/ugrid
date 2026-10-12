# Squash the geometry of an sf object into another bbox

Squash the geometry of an sf object into another bbox

## Usage

``` r
squash2Bbox(geom, toBb = c(-37, -44, -16, -30), rsf = NULL)
```

## Arguments

- geom:

  a sf object.

- toBb:

  destination bbox.

- rsf:

  a list of rescaling functions returned from `rescale` function.
