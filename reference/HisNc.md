# R6 Class representing a `_his.nc` file

This class represents a `_his.nc` file

## Public fields

- `nc`:

  Nc object of the his.nc file

- `path`:

  Path to the his.nc file

- `vars`:

  data.table of variables.

- `dims`:

  data.table of dimensions.

- `data`:

  A list object to cache data read from NetCDF file.

- `sections`:

  List for storing information / data of sections.

- `stations`:

  List for storing information / data of stations.

- `others`:

  List for storing information / data of other types.

- `ts`:

  Time steps.

- `tz`:

  Time zone

- `tsIds`:

  Variables of IDs / Names for time series data.

## Methods

### Public methods

- [`HisNc$new()`](#method-HisNc-initialize)

- [`HisNc$getData()`](#method-HisNc-getData)

- [`HisNc$getData4Ids()`](#method-HisNc-getData4Ids)

- [`HisNc$getVarName()`](#method-HisNc-getVarName)

- [`HisNc$getTsInfo()`](#method-HisNc-getTsInfo)

- [`HisNc$clone()`](#method-HisNc-clone)

------------------------------------------------------------------------

### `HisNc$new()`

Check if the pointer to NetCDF still alive If not, try to open the
NetCDF file again.

Read basic information of the his.nc file

Close the NetCDF connection

Initilisation

#### Usage

    HisNc$new(path)

#### Arguments

- `path`:

  Path to his.nc

------------------------------------------------------------------------

### `HisNc$getData()`

Read data for a variable.

#### Usage

    HisNc$getData(variable, at = "cross_section", force = FALSE)

#### Arguments

- `variable`:

  Name or standard_name / long_name of the variable. If variable is
  given as its name in the NetCDF, the parameter `at` will be ignored.

- `at`:

  Name of the dimension in which the variable is located.

- `force`:

  Logical. If TRUE, data will be read directly from the NetCDF-file and
  cached data will be replaced.

#### Returns

An array.

------------------------------------------------------------------------

### `HisNc$getData4Ids()`

Read time series data for a variable.

#### Usage

    HisNc$getData4Ids(
      variable,
      ids = NULL,
      idNames = ids,
      at = "cross_section",
      aggFun = NULL,
      force = FALSE
    )

#### Arguments

- `variable`:

  Name/ID of the variable

- `ids`:

  Character vector of IDs to get data. It will be ignored if the
  variable is only a single time serie.

- `idNames`:

  Names to assign as column names for the IDs.

- `at`:

  Name of the dimension in which the variable is located.

- `aggFun`:

  A function to apply to all output-columns

- `force`:

  Logical. If TRUE, data will be read directly from the NetCDF-file and
  cached data will be replaced.

#### Returns

a data.table.

------------------------------------------------------------------------

### `HisNc$getVarName()`

Find NetCDF variable based on standard_name or long_name
(case-insensitive).

#### Usage

    HisNc$getVarName(variable, at = "cross_section")

#### Arguments

- `variable`:

  Character of standard / long name the variable.

- `at`:

  Type of elements (node, edge, face, interface, layer, or volume).

- `topo`:

  Topology (m1D, m2D, or m3D).

------------------------------------------------------------------------

### `HisNc$getTsInfo()`

Read information about time series.

#### Usage

    HisNc$getTsInfo()

------------------------------------------------------------------------

### `HisNc$clone()`

The objects of this class are cloneable with this method.

#### Usage

    HisNc$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
