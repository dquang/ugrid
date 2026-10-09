#' Read .pli data to sf_linestring
#'
#' This function can read file in Deltares-polyline format with or without Z values
#' and convert it to a sf LINESTRING.
#'
#' @param path Path to the .pli file (only x, y)
#' @param crs Coordinate system of the polyline, eg. crs = "EPSG:25833"
#' @return sf_linestring
#' @export
#' @examples
#' # read .pli file
#' # with cross sections of Elbe river (x and y)
#' file <- system.file("testdata/Obs_cross_sections01_crs.pli", package = "ugrid")
#' # read without crs, CRS attribute is NA
#' readPli(file)
#' # read with crs, CRS attribute has value
#' readPli(file, crs = "EPSG:25833")
#' # read .pliz file with kribben and kade (x y z)
#' file <- system.file("testdata/Buhnen_Deiche_v02_fxw.pliz", package="ugrid")
#' readPli(file, crs = "EPSG:25833")
#'
readPli <- function(path, crs=sf::NA_crs_) {

    if (!file.exists(path)) {
        warning("File not found: ", path)
        return(NULL)
    }
    tbl <- data.table::fread(file=path, sep="\n", header=FALSE, blank.lines.skip=TRUE)
    tbl <- tbl[!grepl("^\\*", V1)]
    if (nrow(tbl) < 4) {
        warning("Not enough information in the file: ", path)
        return(NULL)
    }
    tbl[, V1 := stringi::stri_trim_both(V1)][, V1 := stringi::stri_replace_all_regex(V1, " +", " ")]
    tbl[, crInfo := data.table::shift(V1, n=-1)]
    tbl[, npts := as.integer(stringi::stri_match_first_regex(crInfo, "(^\\d+) ")[, 2])]
    i <- 1L
    totalRow <- nrow(tbl)
    anyZ <- FALSE
    while (i < totalRow) {
        if (i > totalRow)
            break
        np <- tbl[i, npts]
        nextRow <- i + np + 2
        tbl[i, crName := V1]
        nDim <- tbl[i, strsplit(crInfo, " ")[[1]]] |> as.integer()
        hasZ <- ifelse(nDim[2] > 2, TRUE, FALSE)
        # if there are more than 3 columns, the exceeded columns will be ignored.
        # if there are less than 3 columns, they will be recycled.
        tbl[(i + 2):(nextRow - 1), c("X", "Y", "Z") := data.table::tstrsplit(V1, split=" "), by=V1]
        if (hasZ)
            anyZ <- TRUE
        else
            tbl[(i + 2):(nextRow - 1), Z := NA]
        i <- nextRow
    }
    tbl[, crName := crName[1], by=.(cumsum(!is.na(crName)))]
    tbl[, X := as.double(X)][, Y := as.double(Y)]
    tbl <- tbl[!is.na(X), c("crName", "X", "Y", "Z")]
    if (anyZ) {
        tbl[, Z := as.double(Z)]
        ret <- sfheaders::sf_linestring(tbl, x="X", y="Y", z="Z", linestring_id="crName")
    } else {
        tbl[, Z := NULL]
        ret <- sfheaders::sf_linestring(tbl, x="X", y="Y", linestring_id="crName")
    }
    sf::st_crs(ret) <- crs

    return(ret)
}
