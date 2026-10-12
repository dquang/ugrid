# Package index

## Ugrid object

R6-Ugrid object wrapping NetCDF data that follows Ugrid-Convention.

- [`Ugrid`](https://dquang.github.io/ugrid/reference/Ugrid.md) : R6Class
  to store UGRID-Data

## HisNc object

R6-HisNc object wrapping \_his.nc NetCDF data.

- [`HisNc`](https://dquang.github.io/ugrid/reference/HisNc.md) :

  R6 Class representing a `_his.nc` file

## Reading data

Functions for reading model output.

- [`getAllAtts()`](https://dquang.github.io/ugrid/reference/getAllAtts.md)
  : Get table of attributes for all variables in a nc file
- [`getData4Sf()`](https://dquang.github.io/ugrid/reference/getData4Sf.md)
  : Get data for faces that are intersected with or crossed by a sf
  object
- [`getDimTbl()`](https://dquang.github.io/ugrid/reference/getDimTbl.md)
  : Get table of dimensions
- [`getFaceData4Var()`](https://dquang.github.io/ugrid/reference/getFaceData4Var.md)
  : Get data at faces
- [`getVarTbl()`](https://dquang.github.io/ugrid/reference/getVarTbl.md)
  : Get table of variables
- [`getVerticalData()`](https://dquang.github.io/ugrid/reference/getVerticalData.md)
  : Get data for a vertical section

## Generating geodata

Functions for generating vector / raster for model output.

- [`genColorInfo()`](https://dquang.github.io/ugrid/reference/genColorInfo.md)
  : Color classification for leaflet map
- [`genIsoline()`](https://dquang.github.io/ugrid/reference/genIsoline.md)
  : Generate isolines from a sf object.
- [`genMap()`](https://dquang.github.io/ugrid/reference/genMap.md) :
  Generate a Maplibre GL Map
- [`genMeta()`](https://dquang.github.io/ugrid/reference/genMeta.md) :
  Generate minimal metadata for a dataset
- [`genRaster4All()`](https://dquang.github.io/ugrid/reference/genRaster4All.md)
  : Generate value rasters of one variable for all time steps
- [`genRasterMap()`](https://dquang.github.io/ugrid/reference/genRasterMap.md)
  : Generate a Maplibre GL Map
- [`genTmapAni()`](https://dquang.github.io/ugrid/reference/genTmapAni.md)
  : Generate animation GIF of tmap from list of rasters
- [`genTmapColor()`](https://dquang.github.io/ugrid/reference/genTmapColor.md)
  : Color classification for leaflet map
- [`genVectorLayer()`](https://dquang.github.io/ugrid/reference/genVectorLayer.md)
  : Generate a LINESTRING sf object of velocity vectors

## Read Deltares-format data.

Functions reading some Deltares-format data.

- [`readPli()`](https://dquang.github.io/ugrid/reference/readPli.md) :
  Read .pli data to sf_linestring

## Shiny app

Functions for creating shiny modules / inputs.

- [`ugridApp()`](https://dquang.github.io/ugrid/reference/ugridApp.md) :
  A shiny app for observing ugrid data stored in NetCDF files

## Utilities

Utility functions.

- [`findFaces()`](https://dquang.github.io/ugrid/reference/findFaces.md)
  : Find faces that are crossed by a line or intersected by points.
- [`xy2station()`](https://dquang.github.io/ugrid/reference/xy2station.md)
  : Look up river mileage for a table of points
