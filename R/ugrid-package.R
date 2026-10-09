#' @keywords internal
"_PACKAGE"

#' @importFrom data.table :=
#' @importFrom data.table .SD
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @importFrom data.table .GRP
#' @importFrom data.table %between%
NULL

#' List of coordinate systems for the function `shinyWidgets::virtualSelectInput`.
#'
#' Use this list for the `virtualSelectInput` so that a CRS can be searched and selected.
#'
#' @name crsidChoices
#' @keywords crsidChoices
#' @docType data
#' @format a list that created by function `shinyWidgets::prepare_choices`.
"crsidChoices"
