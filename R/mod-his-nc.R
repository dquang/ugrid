#' R6 Class representing a `_his.nc` file
#'
#' This class represents a `_his.nc` file
#' @export
HisNc <- R6::R6Class(
    "HisNc",
    private = list(
        #' @description
        #' Check if the pointer to NetCDF still alive
        #' If not, try to open the NetCDF file again.
        reOpen = function() {
            ptr <- attributes(self$nc)
            if (identical(ptr$handle_ptr, new("externalptr")))
                self$nc <- RNetCDF::open.nc(self$path)
        },
        #' @description
        #' Read basic information of the his.nc file
        readInfo = function() {

            vars <- getVarTbl(nc=self$nc)
            dims <- getDimTbl(nc=self$nc)
            atts <- getAllAtts(nc=self$nc)
            atts2 <- atts[grepl("unit|long_name|standard_name|FillValue", name, ignore.case=TRUE),
                          c("name", "val", "varName")] |>
                data.table::dcast(varName ~ name, value.var="val")
            vCols <- colnames(atts2) |>
                stringi::stri_replace_all_regex("units", "unit", opts_regex=list(case_insensitive=TRUE)) |>
                stringi::stri_replace_all_regex(".*FillValue$", "fill_value", opts_regex=list(case_insensitive=TRUE))
            colnames(atts2) <- vCols
            atts2[, unit := trimws(unit)]
            atts2[, unit := gsub("m^2", "m2", unit)]
            data.table::setnames(atts2, "varName", "name")
            vars <- merge(vars, atts2, by="name", all.x=TRUE)
            vars[is.na(standard_name), standard_name := name]
            vars[is.na(long_name), long_name := standard_name]
            timeDid <- dims[name == "time", as.integer(id)]
            vars[, hasTime := FALSE]
            vars[dim1 == timeDid | dim2 == timeDid | dim3 == timeDid, hasTime := TRUE]
            self$dims <- dims
            self$vars <- vars
            tsIds <- atts[grepl("timeseries_id", val), varName]
            self$tsIds <- tsIds
            locations <- sub("_name$|_id$", "", tsIds)
            crVars <- vars[grepl("cross_section", name)]
            self$sections$geometry <- list(x=crVars[grepl("_x_coor", name), name],
                                           y=crVars[grepl("_y_coor", name), name],
                                           id=crVars[grepl("_id$", name), name],
                                           name=crVars[grepl("_name$", name), name],
                                           count=dims[grepl("^cross_section$", name), length])
            if (!chkChr(self$sections$geometry$id))
                self$sections$geometry$id <- self$stations$geometry$name
            stVars <- vars[grepl("station_", name)]
            self$stations$geometry <- list(
                x=stVars[grepl("_x_coor", name), name],
                y=stVars[grepl("_y_coor", name), name],
                id=stVars[grepl("_id$", name), name],
                name=stVars[grepl("_name$", name), name],
                count=dims[grepl("^station[s]*$", name), length]
            )
            if (!chkChr(self$stations$geometry$id))
                self$stations$geometry$id <- self$stations$geometry$name
            self$others$names <- grep("^station|^cross_section", locations, invert=TRUE, value=TRUE)
            self$others$dims <- dims[grepl(paste0("^", self$others$names, collapse = "|"), name), id]
            t0 <- atts[varName == "time" & name == "units", unlist(val)]
            if (chkChr(t0)) {
                tUnit <- stringi::stri_match_first_regex(
                    t0, "(^[^ ]+) since")[, 2]
                tUnit <- substr(tUnit, 1, 1)
                tUnitCommons <- list(s=1L, m=60L, h=3600L, d=86400L)
                tUnitFactor <- tUnitCommons[[tUnit]]
                # suppose that timezone shift is always by even hour(s)
                tzShift <- stringi::stri_match_first_regex(
                    t0, "since .* ([+-]*\\d+):\\d+")[, 2]
                tzShift <- as.integer(tzShift)
                tzSign <- ifelse(tzShift < 0, "-", "+")
                tz <- paste0("Etc/GMT", tzSign, tzShift)
                t0 <- stringi::stri_match_first_regex(t0,"since (.+)")[, 2]
                t0 <- asPOSIXctManyFormats(t0, tz=tz)
                ts <- RNetCDF::var.get.nc(self$nc, "time")
                if (tUnitFactor != 1)
                    ts <- ts * tUnitFactor
                self$ts <- as.POSIXct(ts, tz=tz, origin=t0)
                self$tz <- tz
            }

        },
        #' @description
        #' Close the NetCDF connection
        finalize = function() {
            RNetCDF::close.nc(self$nc)
        }
    ),
    public = list(
        #' @description
        #' Initilisation
        #' @param path Path to his.nc
        initialize = function(path) {
            stopifnot(file.exists(path))
            self$nc <- RNetCDF::open.nc(path)
            self$path <- path
            private$readInfo()
        },
        #' @description
        #' Read data for a variable and a list of stations
        #' @param var Name/ID of the variable
        #' @param at Name of the dimension in which the variable is located.
        #' @param cache Logical. If TRUE, the whole data array of the variable will be stored in memory.
        #' This parameter behaves as following:
        #' * If TRUE, the cached data will be used, despite of how it was read, so maybe it is not exactly what you want.
        #' * If FALSE, the cached data will be ignored and also erased.
        #' @return An array.
        getData = function(variable, at="cross_section", cache=TRUE) {

            ncVar <- self$getVarName(variable, at=at)
            if (!chkChr(ncVar)) {
                warning("No variable found.")
                return(NULL)
            }
            if (cache) {
                if (!is.null(self$data[[at]][[ncVar]])) {
                    dta <- self$data[[at]][[ncVar]]
                } else {
                    dta <- RNetCDF::var.get.nc(self$nc, variable=ncVar)
                    self$data[[at]][[ncVar]] <- dta
                }
            } else {
                dta <- RNetCDF::var.get.nc(self$nc, variable=ncVar)
                self$data[[at]][[ncVar]] <- NULL
            }
            return(dta)
        },
        #' @description
        #' Read time series data for a variable.
        #' @param var Name/ID of the variable
        #' @param at Name of the dimension in which the variable is located.
        #' @param ids Character vector of IDs to get data. It will be ignored if the variable is only a single time serie.
        #' @param idNames Names to assign as column names for the IDs.
        #' @param aggFun A function to apply to all output-columns
        #' @param cache Logical. If TRUE, the whole data array of the variable will be stored in memory.
        #' This parameter behaves as following:
        #' * If TRUE, the cached data will be used, despite of how it was read, so maybe it is not exactly what you want.
        #' * If FALSE, the cached data will be ignored and also erased.
        #' @return a data.table.
        getData4Ids = function(variable, ids=NULL, idNames=ids, at="cross_section", aggFun=NULL, cache=TRUE) {

            ncVar <- self$getVarName(variable=variable, at=at)
            thisVar <- self$vars[name %in% ncVar]
            if (!isTRUE(thisVar[, hasTime])) {
                message("This function is only for reading time series.")
                return(NULL)
            }
            dta <- self$getData(variable=ncVar, at=at, cache=cache)
            if (thisVar$ndims < 2)
                return(dta)
            else
                dta <- t(dta)
            dimName <- self$dims[id == thisVar$dim1, name]
            idVar <- self$vars[grep(paste0("^", dimName, "[s]*_id$"), name), name]
            if (!chkChr(idVar))
                idVar <- self$vars[grep(paste0("^", dimName, "[s]*_name$"), name), name]
            varIds <- RNetCDF::var.get.nc(self$nc, idVar)
            if (length(ids) < 1)
                ids <- varIds
            idx <- which(varIds %in% ids)
            naIds <- setdiff(ids, varIds[idx])
            newIds <- c(varIds[idx], naIds)
            newIdx <- sapply(newIds, function(x) which(ids %in% x), USE.NAMES=FALSE)
            if (length(naIds) > 0)
                idx <- c(idx, rep(NA, length(naIds)))
            ret <- dta[, idx]
            if (length(idNames) != length(ids))
                idNames <- ids
            newNames <- idNames[newIdx]
            ret <- data.table::data.table(ret)
            colnames(ret) <- newNames
            ret$ts <- self$ts
            setcolorder(ret, c("ts", idNames))
            if (is.function(aggFun)) {
                ret <- ret[, lapply(.SD, FUN=aggFun), .SDcols = -c("ts")]
            }
            return(ret)
        },
        #' @description
        #' Find NetCDF variable based on standard_name or long_name (case-insensitive).
        #' @param variable Character of standard / long name the variable.
        #' @param topo Topology (m1D, m2D, or m3D).
        #' @param at Type of elements (node, edge, face, interface, layer, or volume).
        getVarName = function(variable, at="cross_section") {

            ncVar <- self$vars[grepl(variable, name, ignore.case=TRUE), name]
            if (!chkChr(ncVar)) {
                pat <- paste0("^", at, "[s]*$")
                dimId <- self$dims[grepl(pat, name), id]
                if (length(dimId) != 1) {
                    warning(at, " is not one of the dimension names or ambiguous")
                    return(NULL)
                }
                ncVar <- self$vars[grepl(variable, standard_name, ignore.case=TRUE) &
                                       (dim1 == dimId | dim2 == dimId | dim3 == dimId), name]
                if (!chkChr(ncVar)) {
                    ncVar <- self$vars[grepl(variable, long_name, ignore.case=TRUE)
                                       & (dim1 == dimId | dim2 == dimId | dim3 == dimId), name]
                }
                if (!chkChr(ncVar)) {
                    message("Variable with name: ", variable, " does not found at ", at,
                            ", or the name is ambigious. Result: ", paste(ncVar, collapse=","))
                    ncVar <- NULL
                }
            }

            return(ncVar)
        },
        getTsInfo = function() {
            tsIds <- self$tsIds
            locations <- gsub("_name$|_id$", "", tsIds)
            patts <- paste0("^", locations, "[s]*$") |>
                paste0(collapse="|")
            dimIds <- sapply(locations, function(x) self$dims[grepl(paste0("^", x, "[s]*$"), name), id])
            varTbl <- lapply(seq_along(dimIds), function(i) {
                vNames <- self$vars[(dim1 == dimIds[i] | dim2 == dimIds[i]) & (hasTime == TRUE), name]
                vTbl <- data.table::data.table(varName=vNames)
                vTbl$location <- locations[i]
                vTbl
                }) |> rbindlist()
            idTbl <- lapply(seq_along(tsIds), function(i) {
                vNames <- self$getData(tsIds[i])
                vTbl <- data.table::data.table(id=vNames)
                vTbl$location <- locations[i]
                vTbl
            }) |> rbindlist()
            idTbl[nchar(id) < 1]
            return(list(varTbl=varTbl, idTbl=idTbl))
        },
        #' @field nc Nc object of the his.nc file
        nc = NULL,
        #' @field path Path to the his.nc file
        path = NULL,
        #' @field vars data.table of variables.
        vars = data.table::data.table(),
        #' @field dims data.table of dimensions.
        dims = data.table::data.table(),
        #' @field data A list object to cache data read from NetCDF file.
        data = list(),
        #' @field sections List for storing information / data of sections.
        sections = list(),
        #' @field stations List for storing information / data of stations.
        stations = list(),
        #' @field others List for storing information / data of other types.
        others = list(),
        #' @field ts Time steps.
        ts = vector(),
        #' @field tz Time zone
        tz = character(0),
        #' @field tsIds Variables of IDs / Names for time series data.
        tsIds = vector()
    )
)

getData4Ids <- function(hisLst, variable, ids=NULL, idNames=ids, at="cross_section", aggFun=NULL, cache=TRUE) {
    ret <- list()
}
