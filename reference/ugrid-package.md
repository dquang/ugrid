# ugrid: Postprocessing for NetCDF-Data in UGRID-Format

An R package for post-processing model outputs stored in NetCDF files
following the UGRID convention. While it is primarily designed to handle
results from Delft3D-FM, it can be used with any UGRID-compliant NetCDF
dataset. The data are encapsulated in an R6 object, where the 2D mesh is
represented as a polygon layer, enabling seamless integration with
additional post-processing functions included in the package. For users
who prefer a graphical interface, the package also provides a Shiny
application that brings together the most important functionalities.
This allows convenient data exploration and processing without requiring
prior knowledge of R programming. Processed data can also be directly
downloaded from within the app.

## See also

Useful links:

- <https://dquang.github.io/ugrid/>

## Author

**Maintainer**: Quang Duong <duong@bafg.de>

Authors:

- Quang Duong <duong@bafg.de>

Other contributors:

- Asta Kunkel \[contributor\]
