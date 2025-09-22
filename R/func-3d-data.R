#' Get data for a vertical section
#'
#' @param x an integer vector of face indexes or an object of class `sf`.
#' @param variable Character. Name of variable
#' @param mesh an object of class `Ugrid`
#' @export
verticalData <- function(x, variable, mesh, force = FALSE, ...) {

    chkLayer <- mesh$hasLayerData(variable=variable)
    if (!chkLayer) {
        message("No layer data found for: ", variable)
        return(NULL)
    }
    if (inherits(x, "sf")) {
        faces <- findFaces(x, mesh)
    } else {
        faces <- x # we don't check or do any converting in this case.
    }
    dta <- mesh$data4Face2D(variable = variable, force = force, ...)
    dta <- dta[, faces, ]
    return(dta)
}


