# R6Class for managing simulations

R6Class for managing simulations

## Public fields

- `tbl`:

  A data.table with path to NetCDF files and their hashes

- `cases`:

  Vector of case names.

- `ugrids`:

  reactiveValues holding Ugrid objects. It does not necessarily contain
  all objects of NetCDF files in the tbl.

## Methods

### Public methods

- [`CaseManager$new()`](#method-CaseManager-initialize)

- [`CaseManager$addCases()`](#method-CaseManager-addCases)

- [`CaseManager$add()`](#method-CaseManager-add)

- [`CaseManager$get()`](#method-CaseManager-get)

- [`CaseManager$clone()`](#method-CaseManager-clone)

------------------------------------------------------------------------

### `CaseManager$new()`

Initialize a CaseManager object.

#### Usage

    CaseManager$new(
      caseLst = NULL,
      pattern = getOption("ugrid.pattern"),
      crs = NA_character_,
      newCrs = NA_character_,
      ignoreCrsInFile = FALSE
    )

#### Arguments

- `caseLst`:

  Character vector of folder-paths of cases.

- `pattern`:

  Regex pattern for listing NetCDF files.

- `crs`:

  ID of the coordinate reference system used in the NetCDF. A number
  will be understood as an EPSG code. In case no information about CRS
  found in the NetCDF, this will be assigned as CRS for the dataset. It
  must have the length 1 (will be recycled) or the same length as
  `caseLst`.

- `newCrs`:

  ID of a new coordinate reference system to transform the exported
  layers to.

- `ignoreCrsInFile`:

  If TRUE, the CRS information in the NetCDF file will be ignored. Like
  `crs`, it must the length 1 (will be recycled) or the same length as
  `caseLst` also. A number will be also understood as an EPSG code.

------------------------------------------------------------------------

### `CaseManager$addCases()`

Initialize a ProjectManager object.

#### Usage

    CaseManager$addCases(
      caseLst = NULL,
      pattern = getOption("ugrid.pattern"),
      crs = NA_character_,
      newCrs = NA_character_,
      ignoreCrsInFile = FALSE
    )

#### Arguments

- `caseLst`:

  Character vector of folder-paths of cases.

- `pattern`:

  Regex pattern for listing NetCDF files.

- `crs`:

  ID of the coordinate reference system used in the NetCDF. A number
  will be understood as an EPSG code. In case no information about CRS
  found in the NetCDF, this will be assigned as CRS for the dataset. It
  must have the length 1 (will be recycled) or the same length as
  `caseLst`.

- `newCrs`:

  ID of a new coordinate reference system to transform the exported
  layers to. Like `crs`, it must the length 1 (will be recycled) or the
  same length as `caseLst` also. A number will be also understood as an
  EPSG code.

- `ignoreCrsInFile`:

  If TRUE, the CRS information in the NetCDF file will be ignored.

------------------------------------------------------------------------

### `CaseManager$add()`

add Ugrid object to the manager.

#### Usage

    CaseManager$add(path, overwrite = FALSE, ...)

#### Arguments

- `path`:

  Path to NetCDF file.

- `overwrite`:

  Set TRUE to overwrite the current object, if exists

- `...`:

  Other parameters for initializing the Ugrid object.

------------------------------------------------------------------------

### `CaseManager$get()`

Get Ugrid object from the manager.

#### Usage

    CaseManager$get(id)

#### Arguments

- `id`:

  Path to NetCDF file or its hash.

------------------------------------------------------------------------

### `CaseManager$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CaseManager$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
