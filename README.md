
# Package ugrid

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The package `ugrid` is a tool for post-processing model outputs stored
in NetCDF files following the UGRID convention. While it is primarily
designed to handle results from Delft3D-FM, it can be used with any
UGRID-compliant NetCDF dataset.

The data are encapsulated in an R6 object, where the 2D mesh is
represented as a polygon layer, enabling seamless integration with
additional post-processing functions included in the package. For users
who prefer a graphical interface, the package also provides a Shiny
application that brings together the most important functionality. This
allows convenient data exploration and processing without requiring
prior knowledge of R programming. Processed data can also be directly
downloaded from within the app.

## Installation

You can install the development version of ugrid from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dquang/ugrid")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ugrid)
mapFile = system.file("testdata/d3dfm/elbe2d/FlowFM_0000_map.nc", package="ugrid")
hisFile = system.file("testdata/d3dfm/elbe2d/FlowFM_0000_his.nc", package="ugrid")

# create ugrid object
mesh = Ugrid$new(mapFile)
# information about 2D topology
mesh$m2D
# read water level data
mesh$getData4Face2D(variable="water level")
mesh$data2D$face$`water level`

# create his object
his = HisNc$new(hisFile)
# read data at a station
his$getData4Ids("water level", ids="504_7", at="station")
```
