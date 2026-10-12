# An extended version of `as.POSIXct` function.

This version tries different date-time formats, included german one.

## Usage

``` r
asPOSIXctManyFormats(x, origin = NULL, tz = getOption("ugrid.timeZone"), ...)
```

## Arguments

- x:

  character vector

- origin:

  Origin of the time series.

- tz:

  Time zone
