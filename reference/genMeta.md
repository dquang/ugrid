# Generate minimal metadata for a dataset

Generate minimal metadata for a dataset

## Usage

``` r
genMeta(
  fid,
  pid,
  uri = NULL,
  time = Sys.time(),
  title = "Dataset",
  name = NA,
  org = "BfG",
  position = NA,
  phone = "+4926113060",
  email = "posteingang@bafg.de",
  homepage = "http://www.bafg.de",
  street = "Am Mainzer Tor 1",
  city = "Koblenz",
  postcode = 56068,
  crsCode = "",
  crsCodeSpace = "EPSG",
  abstract = NA,
  purpose = NA,
  credits = NULL,
  categories = "inlandWaters",
  status = "onGoing",
  keywords = c("2D-Modelling"),
  statement = NA,
  spType = "vector",
  bbox = NULL,
  moreInfo = character(0)
)
```

## Arguments

- fid:

  File identifier

- pid:

  Parent identifier (i.e. Project id)

- uri:

  URI to the file

- time:

  A time stamp in POSIXct class

- title:

  Title of the dataset

- name, org, position, email, phone, street, city, postcode, homepage:

  Contact information.

- crsCode:

  CRS code.

- crsCodeSpace:

  CRS Codespace.

- abstract:

  Abstract of the dataset.

- purpose:

  Purpose of the dataset.

- credits:

  Credits for the dataset

- categories:

  Categories of the dataset.

- status:

  Status of the dataset.

- keywords:

  Keywords of the dataset.

- statement:

  Statemet of data quality of the dataset.

- spType:

  Type of spatial data.

- bbox:

  an bbox object.

- moreInfo:

  More information about the dataset.

## Value

an XML character vector encoded by `ISOMetadata` an object.
