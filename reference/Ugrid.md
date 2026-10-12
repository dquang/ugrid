# R6Class to store UGRID-Data

R6Class to store UGRID-Data

## Public fields

- `nc`:

  Nc object of the "UGRID-NetCDF" file

- `vars`:

  data.table of variables

- `dims`:

  data.table of dimensions

- `m1D`:

  list of attributes of 1D-topology

- `m2D`:

  list of attributes of 2D-topology

- `m3D`:

  list of attributes of 3D-topology

- `crs`:

  Coordinate Reference System of the original data

- `newCrs`:

  New Coordinate Reference System

- `path`:

  Path to NetCDF file

- `name`:

  Name of NetCDF file

- `data1D`:

  a named list of matrix stored the data for mesh1D-variables once read

- `data2D`:

  a named list of matrix stored the data for mesh2D-variables once read

- `data3D`:

  a named list of matrix stored the data for mesh3d-variables once read

- `tz`:

  Time zone

- `totalTs`:

  Number of timesteps

- `ts`:

  Vector of time series

- `md`:

  main cell domain number

- `od`:

  other cell domain numbers

- `ignoreCrsInFile`:

  If TRUE, the CRS information in the NetCDF file will be ignored.

- `ret`:

  a field to store return result from a parallel process. So that we can
  get back also the whole R6 object, otherwise the changes to this R6
  object that made during the parallel process will be lost.

## Active bindings

- `tf`:

  active binding to check the transformation ability of CRS

## Methods

### Public methods

- [`Ugrid$new()`](#method-Ugrid-initialize)

- [`Ugrid$getVarName()`](#method-Ugrid-getVarName)

- [`Ugrid$hasLayerData()`](#method-Ugrid-hasLayerData)

- [`Ugrid$getData4Node1D()`](#method-Ugrid-getData4Node1D)

- [`Ugrid$getData4Edge1D()`](#method-Ugrid-getData4Edge1D)

- [`Ugrid$getData4Node2D()`](#method-Ugrid-getData4Node2D)

- [`Ugrid$getData4Edge2D()`](#method-Ugrid-getData4Edge2D)

- [`Ugrid$getData4Face2D()`](#method-Ugrid-getData4Face2D)

- [`Ugrid$getData4Any()`](#method-Ugrid-getData4Any)

- [`Ugrid$getData4Polygon()`](#method-Ugrid-getData4Polygon)

- [`Ugrid$buildFace2DPoly()`](#method-Ugrid-buildFace2DPoly)

- [`Ugrid$buildEdge1DLine()`](#method-Ugrid-buildEdge1DLine)

- [`Ugrid$readDomainInfo()`](#method-Ugrid-readDomainInfo)

- [`Ugrid$clone()`](#method-Ugrid-clone)

------------------------------------------------------------------------

### `Ugrid$new()`

Read basic information of the topologies stored in the NetCDF file

Check if the pointer to NetCDF still alive If not, try to open the
NetCDF file again.

Close the NetCDF connection

Initialization

#### Usage

    Ugrid$new(
      ncFile,
      crs = NULL,
      newCrs = NULL,
      domainInfo = FALSE,
      ignoreCrsInFile = FALSE
    )

#### Arguments

- `ncFile`:

  Path to 'UGRID-NetCDF' file.

- `crs`:

  Coordinate reference system used in the NetCDF. It must be a character
  in format authority:code like EPSG:4326 or an object of class crs. An
  integer is also accepted with an assumption that it is an EPSG-code.

- `newCrs`:

  New coordinate reference system to transform the exported layers to.

- `domainInfo`:

  Read information about domains?

- `ignoreCrsInFile`:

  If TRUE, the CRS information in the NetCDF file will be ignored.

#### Returns

a logical value

------------------------------------------------------------------------

### `Ugrid$getVarName()`

Find NetCDF variable based on standard_name or name (case-insensitive).

#### Usage

    Ugrid$getVarName(
      variable,
      topo = c("m2D", "m1D", "m3D"),
      at = c("face", "node", "edge", "volume", "interface", "layer")
    )

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable.

- `topo`:

  Topology (m1D, m2D, or m3D).

- `at`:

  Type of elements (node, edge, face, interface, layer, or volume).

#### Examples

    # generate Ugrid Object
    library(ugrid)
    mapFile <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    mesh <-  Ugrid$new(mapFile)
    # data.table with all vars in nc
    mesh$vars
    # get Ugrid name of this variable
    mesh$getVarName(variable = "sea_surface_height", topo = "m2D", at = "face")
    mesh$getVarName(variable = "altitude", topo = "m2D", at = "face")
    mesh$getVarName(variable = "altitude", topo = "m2D", at = "node")
    mesh$getVarName(variable = "mesh2d_cftrt", topo = "m2D", at = "edge")

------------------------------------------------------------------------

### `Ugrid$hasLayerData()`

Check if a variable has layered data

#### Usage

    Ugrid$hasLayerData(variable)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable

------------------------------------------------------------------------

### `Ugrid$getData4Node1D()`

Read data at nodes of 1D-Topology

#### Usage

    Ugrid$getData4Node1D(variable, force = FALSE, ...)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

------------------------------------------------------------------------

### `Ugrid$getData4Edge1D()`

Read data at edges of 1D-Topology

#### Usage

    Ugrid$getData4Edge1D(variable, force = FALSE, ...)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

------------------------------------------------------------------------

### `Ugrid$getData4Node2D()`

Read data at nodes of 2D-Topology

#### Usage

    Ugrid$getData4Node2D(variable, lyr = "all", force = FALSE, ...)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable.

- `lyr`:

  layer or interface indexes, or "all" for the whole dataset.

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

------------------------------------------------------------------------

### `Ugrid$getData4Edge2D()`

Read data at edges of 2D-Topology

#### Usage

    Ugrid$getData4Edge2D(variable, lyr = "all", force = FALSE, ...)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable

- `lyr`:

  layer or interface indexes, or "all" for the whole dataset.

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

------------------------------------------------------------------------

### `Ugrid$getData4Face2D()`

Read data at faces of 2D-Topology Wrapper for `Method getData4Any()`

#### Usage

    Ugrid$getData4Face2D(
      variable,
      lyr = "all",
      onlyMain = FALSE,
      force = FALSE,
      ...
    )

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable.

- `lyr`:

  layer or interface indexes, or "all" for the whole dataset.

- `onlyMain`:

  If TRUE, the cell elements of other domains will be removed.

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

#### Examples

    # generate Ugrid Object
    file <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    UgridObj <-  Ugrid$new(file)
    # Wrapper to function getData4Any, writes data to table in UgridObj$data2D$face$sea_surface_height
    UgridObj$getData4Face2D("sea_surface_height")
    dim(UgridObj$data2D$face$sea_surface_height)
    # Get data for main domain
    main <- UgridObj$getData4Face2D("sea_surface_height", onlyMain = TRUE)
    dim(main)

------------------------------------------------------------------------

### `Ugrid$getData4Any()`

Read data for any topology

#### Usage

    Ugrid$getData4Any(variable, topo, at, lyr = "all", force = FALSE, ...)

#### Arguments

- `variable`:

  Character of standard name (UGRID) or name of the variable

- `topo`:

  Topology (m1D, m2D, or m3D)

- `at`:

  Type of elements (node, edge, face, interface, or volume)

- `lyr`:

  layer or interface indexes, or "all" for the whole dataset.

- `force`:

  Logical. Force to read from NetCDF or first get from cache?

- `...`:

  will be forwarded to
  [`RNetCDF::var.get.nc`](https://rdrr.io/pkg/RNetCDF/man/var.get.nc.html)

#### Returns

A matrix or a vector. In case of matrix, the ordering of dimensions are
always: interface/layer (if any) x element x time. Some special
variables like "Mapping from every edge to the two faces that it
separates" are excluded.

------------------------------------------------------------------------

### `Ugrid$getData4Polygon()`

Get data on faces and assign it to the face polygon layer.

#### Usage

    Ugrid$getData4Polygon(
      variable,
      tsIdx = 1L,
      agg = "none",
      lyr = 1L,
      force = FALSE,
      onlyMain = FALSE,
      dryAsNa = TRUE
    )

#### Arguments

- `variable`:

  Variable name for the data

- `tsIdx`:

  Time index

- `agg`:

  Option for aggregating the data by rows. The aggregation functions
  come from `matrixStats` package.

- `lyr`:

  Index of the layer to get data.

- `force`:

  If TRUE, the data stored in Ugrid object, if any, will be read again.

- `onlyMain`:

  If TRUE, the cell elements of other domains will be removed.

- `dryAsNa`:

  If TRUE (default), values for dried (waterlevel - elevation \< 0)
  cells will be assigned NaN.

------------------------------------------------------------------------

### `Ugrid$buildFace2DPoly()`

Build polygon layer for 2D-Topology

#### Usage

    Ugrid$buildFace2DPoly()

#### Returns

sf object of POLYGON geometries with `faceID` field that reflexes the
index of faces.

------------------------------------------------------------------------

### `Ugrid$buildEdge1DLine()`

Build line layer for 1D-Topology

#### Usage

    Ugrid$buildEdge1DLine()

------------------------------------------------------------------------

### `Ugrid$readDomainInfo()`

Read information about domains and identify the main domain

#### Usage

    Ugrid$readDomainInfo()

------------------------------------------------------------------------

### `Ugrid$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Ugrid$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `Ugrid$getVarName()`
## ------------------------------------------------

# generate Ugrid Object
library(ugrid)
mapFile <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
mesh <-  Ugrid$new(mapFile)
# data.table with all vars in nc
mesh$vars
#> Key: <name>
#>                         name    id      type ndims natts  dim1  dim2  dim3
#>                       <char> <num>    <char> <num> <int> <int> <int> <int>
#>  1:             mesh2d_cftrt    24 NC_DOUBLE     2     8     1     5    NA
#>  2:        mesh2d_edge_faces     9    NC_INT     2     4     0     1    NA
#>  3:        mesh2d_edge_nodes     7    NC_INT     2     3     0     1    NA
#>  4:         mesh2d_edge_type    14    NC_INT     1     7     1    NA    NA
#>  5:            mesh2d_edge_x     5 NC_DOUBLE     1     3     1    NA    NA
#>  6:            mesh2d_edge_y     6 NC_DOUBLE     1     3     1    NA    NA
#>  7:        mesh2d_face_nodes     8    NC_INT     2     4     4     3    NA
#>  8:            mesh2d_face_x    10 NC_DOUBLE     1     4     3    NA    NA
#>  9:        mesh2d_face_x_bnd    12 NC_DOUBLE     2     4     4     3    NA
#> 10:            mesh2d_face_y    11 NC_DOUBLE     1     4     3    NA    NA
#> 11:        mesh2d_face_y_bnd    13 NC_DOUBLE     2     4     4     3    NA
#> 12:       mesh2d_flowelem_ba    15 NC_DOUBLE     1     9     3    NA    NA
#> 13:       mesh2d_flowelem_bl    16 NC_DOUBLE     1    10     3    NA    NA
#> 14:   mesh2d_flowelem_domain    17    NC_INT     1    10     3    NA    NA
#> 15: mesh2d_flowelem_globalnr    18    NC_INT     1    10     3    NA    NA
#> 16:            mesh2d_node_x     2 NC_DOUBLE     1     3     2    NA    NA
#> 17:            mesh2d_node_y     3 NC_DOUBLE     1     3     2    NA    NA
#> 18:            mesh2d_node_z     4 NC_DOUBLE     1     8     2    NA    NA
#> 19:                mesh2d_s1    21 NC_DOUBLE     2    10     3     5    NA
#> 20:             mesh2d_ucmag    23 NC_DOUBLE     2    10     3     5    NA
#> 21:        mesh2d_waterdepth    22 NC_DOUBLE     2    10     3     5    NA
#> 22:                     time    19 NC_DOUBLE     1     2     5    NA    NA
#> 23:                 timestep    20 NC_DOUBLE     1     2     5    NA    NA
#>                         name    id      type ndims natts  dim1  dim2  dim3
#>                       <char> <num>    <char> <num> <int> <int> <int> <int>
#>                   dim1Name      dim2Name dim3Name fill_value
#>                     <char>        <char>   <char>      <num>
#>  1:          mesh2d_nEdges          time     <NA>       -999
#>  2:                    Two mesh2d_nEdges     <NA>       -999
#>  3:                    Two mesh2d_nEdges     <NA>         NA
#>  4:          mesh2d_nEdges          <NA>     <NA>       -999
#>  5:          mesh2d_nEdges          <NA>     <NA>         NA
#>  6:          mesh2d_nEdges          <NA>     <NA>         NA
#>  7: mesh2d_nMax_face_nodes mesh2d_nFaces     <NA>       -999
#>  8:          mesh2d_nFaces          <NA>     <NA>         NA
#>  9: mesh2d_nMax_face_nodes mesh2d_nFaces     <NA>       -999
#> 10:          mesh2d_nFaces          <NA>     <NA>         NA
#> 11: mesh2d_nMax_face_nodes mesh2d_nFaces     <NA>       -999
#> 12:          mesh2d_nFaces          <NA>     <NA>       -999
#> 13:          mesh2d_nFaces          <NA>     <NA>       -999
#> 14:          mesh2d_nFaces          <NA>     <NA>       -999
#> 15:          mesh2d_nFaces          <NA>     <NA>       -999
#> 16:          mesh2d_nNodes          <NA>     <NA>         NA
#> 17:          mesh2d_nNodes          <NA>     <NA>         NA
#> 18:          mesh2d_nNodes          <NA>     <NA>       -999
#> 19:          mesh2d_nFaces          time     <NA>       -999
#> 20:          mesh2d_nFaces          time     <NA>       -999
#> 21:          mesh2d_nFaces          time     <NA>       -999
#> 22:                   time          <NA>     <NA>         NA
#> 23:                   time          <NA>     <NA>         NA
#>                   dim1Name      dim2Name dim3Name fill_value
#>                     <char>        <char>   <char>      <num>
#>                                                        long_name
#>                                                           <char>
#>  1:                           Manning roughness from trachytopes
#>  2:                              Neighboring faces of mesh edges
#>  3:                            Start and end nodes of mesh edges
#>  4:          edge type (relation between edge and flow geometry)
#>  5: characteristic x-coordinate of the mesh edge (e.g. midpoint)
#>  6: characteristic y-coordinate of the mesh edge (e.g. midpoint)
#>  7:                Vertex nodes of mesh faces (counterclockwise)
#>  8:                     Characteristic x-coordinate of mesh face
#>  9:  x-coordinate bounds of mesh faces (i.e. corner coordinates)
#> 10:                     Characteristic y-coordinate of mesh face
#> 11:  y-coordinate bounds of mesh faces (i.e. corner coordinates)
#> 12:                                           mesh2d_flowelem_ba
#> 13:                            flow element center bedlevel (bl)
#> 14:                                domain number of flow element
#> 15:                                global flow element numbering
#> 16:                                   x-coordinate of mesh nodes
#> 17:                                   y-coordinate of mesh nodes
#> 18:                                   z-coordinate of mesh nodes
#> 19:                                                  Water level
#> 20:                       Flow element center velocity magnitude
#> 21:                               Water depth at pressure points
#> 22:                                                         time
#> 23:   Latest computational timestep size in each output interval
#>                                                        long_name
#>                                                           <char>
#>                         standard_name                                     unit
#>                                <char>                                   <char>
#>  1:                      mesh2d_cftrt                                     <NA>
#>  2:                 mesh2d_edge_faces                                     <NA>
#>  3:                 mesh2d_edge_nodes                                     <NA>
#>  4:                  mesh2d_edge_type                                     <NA>
#>  5:           projection_x_coordinate                                        m
#>  6:           projection_y_coordinate                                        m
#>  7:                 mesh2d_face_nodes                                     <NA>
#>  8:           projection_x_coordinate                                        m
#>  9:           projection_x_coordinate                                        m
#> 10:           projection_y_coordinate                                        m
#> 11:           projection_y_coordinate                                        m
#> 12:                         cell_area                                       m2
#> 13:                          altitude                                        m
#> 14:                cell_domain_number                                     <NA>
#> 15:                cell_global_number                                     <NA>
#> 16:           projection_x_coordinate                                        m
#> 17:           projection_y_coordinate                                        m
#> 18:                          altitude                                        m
#> 19:                sea_surface_height                                        m
#> 20:                   sea_water_speed                                    m s-1
#> 21: sea_floor_depth_below_sea_surface                                        m
#> 22:                              time seconds since 2013-06-01 00:00:00 +00:00
#> 23:                          timestep                                        s
#>                         standard_name                                     unit
#>                                <char>                                   <char>
#>     hasTime
#>      <lgcl>
#>  1:    TRUE
#>  2:   FALSE
#>  3:   FALSE
#>  4:   FALSE
#>  5:   FALSE
#>  6:   FALSE
#>  7:   FALSE
#>  8:   FALSE
#>  9:   FALSE
#> 10:   FALSE
#> 11:   FALSE
#> 12:   FALSE
#> 13:   FALSE
#> 14:   FALSE
#> 15:   FALSE
#> 16:   FALSE
#> 17:   FALSE
#> 18:   FALSE
#> 19:    TRUE
#> 20:    TRUE
#> 21:    TRUE
#> 22:    TRUE
#> 23:    TRUE
#>     hasTime
#>      <lgcl>
# get Ugrid name of this variable
mesh$getVarName(variable = "sea_surface_height", topo = "m2D", at = "face")
#> [1] "mesh2d_s1"
mesh$getVarName(variable = "altitude", topo = "m2D", at = "face")
#> [1] "mesh2d_flowelem_bl"
mesh$getVarName(variable = "altitude", topo = "m2D", at = "node")
#> [1] "mesh2d_node_z"
mesh$getVarName(variable = "mesh2d_cftrt", topo = "m2D", at = "edge")
#> Variable with name: mesh2d_cftrt does not found or the name is ambigious. Result: 
#> NULL

## ------------------------------------------------
## Method `Ugrid$getData4Face2D()`
## ------------------------------------------------

# generate Ugrid Object
file <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
UgridObj <-  Ugrid$new(file)
# Wrapper to function getData4Any, writes data to table in UgridObj$data2D$face$sea_surface_height
UgridObj$getData4Face2D("sea_surface_height")
dim(UgridObj$data2D$face$sea_surface_height)
#> [1] 3621   15
# Get data for main domain
main <- UgridObj$getData4Face2D("sea_surface_height", onlyMain = TRUE)
dim(main)
#> [1] 2531   15
```
