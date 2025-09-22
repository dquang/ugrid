#' Get table of variables
#'
#' Get table of variables from NetCDF file
#'
#' @param nc NetCDF object or path to nc file
#'
#' @return a data.table
#' @export
getVarTbl = function(nc = NULL){

    toClose <- FALSE
    if (!inherits(nc, "NetCDF")) {
        nc <- RNetCDF::open.nc(nc)
        toClose <- TRUE
    }
    ncInfo <- RNetCDF::file.inq.nc(nc)
    varTbl <- lapply(0:(ncInfo$nvars - 1), function(v) {RNetCDF::var.inq.nc(nc, variable=v)})
    varTbl <- rbindlist(varTbl, fill = TRUE)
    varTbl <- rbind(varTbl, data.table(id=ncInfo$nvars + 1,
                                       name="NC_GLOBAL",
                                       ndims=0,
                                       natts=ncInfo$ngatts),
                    fill=TRUE)
    varTbl[, dimIdx := paste0("dim", 1:.N), by="name"]
    varTbl <- dcast(varTbl, id + name + type + ndims + natts ~ dimIdx, value.var="dimids")
    # make sure there are three columns of dimensions
    dimCols <- c("dim1", "dim2", "dim3")
    dimCols <- dimCols[! dimCols %in% colnames(varTbl)]
    if (length(dimCols) > 0)
        varTbl[, (dimCols) := as.list(rep(NA, length(dimCols)))]
    if (toClose)
        RNetCDF::close.nc(nc)
    return(varTbl)
}

#' Get table of dimensions
#'
#' Get table of dimensions from NetCDF file
#'
#' @param nc NetCDF object or path to nc file
#'
#' @return a data.table
#' @export
getDimTbl = function(nc = NULL){

    toClose <- FALSE
    if (!inherits(nc, "NetCDF")) {
        nc <- RNetCDF::open.nc(nc)
        toClose <- TRUE
    }
    ncInfo <- RNetCDF::file.inq.nc(nc)
    dimTbl <- lapply(0:(ncInfo$ndims - 1), function(d) RNetCDF::dim.inq.nc(nc, dimension=d))
    dimTbl <- rbindlist(dimTbl, fill=TRUE)
    if (toClose)
        RNetCDF::close.nc(nc)
    return(dimTbl)
}


#' Get table of attributes for all variables in a nc file
#'
#' Get table of attributes from NetCDF file
#'
#' @param nc NetCDF object or path to nc file
#'
#' @return a data.table
#' @export
getAllAtts <- function(nc) {


    toClose <- FALSE
    if (!inherits(nc, "NetCDF")) {
        nc <- RNetCDF::open.nc(nc)
        toClose <- TRUE
    }
    varTbl <- getVarTbl(nc)
    varAtts <- list()
    for (aV in varTbl$name) {
        natt <- varTbl[name==aV, natts[1]]
        if (natt < 1)
            next
        attLst <- sapply(seq.int(0, natt - 1, 1),
                         FUN = function(x) unlist(RNetCDF::att.inq.nc(nc, aV, x))) |>
            t() |> as.data.table()
        attValLst <- sapply(seq.int(0, natt - 1, 1),
                            FUN = function(x) {
                                vi <- RNetCDF::att.get.nc(nc, aV, x)
                                if (length(vi) > 1)
                                    vi <- paste0(vi, collapse = ";")
                                vi
                            }
                                )
        attLst[, val := attValLst]
        attLst[, varName := aV]
        varAtts[[aV]] <- attLst
    }
    varAtts <- rbindlist(varAtts, fill=TRUE)
    if (toClose)
        RNetCDF::close.nc(nc)
    return(varAtts)
}

# Quick check if a NetCDF file follows UGRID convention
# by checking UGRID keyword in global attributes.
isUgridNc <- function(nc) {

    toClose <- FALSE
    if (!inherits(nc, "NetCDF")) {
        nc <- RNetCDF::open.nc(nc)
        toClose <- TRUE
    }
    fInfo <- RNetCDF::file.inq.nc(nc)
    ret <- FALSE
    if (isTRUE(fInfo$ngatts > 0)) {
        globalAtt <- sapply(
            seq.int(0, fInfo$ngatts - 1, 1),
            function(i) RNetCDF::att.get.nc(nc, "NC_GLOBAL", i)
        )
        ret <- any(grepl("UGRID", globalAtt, ignore.case = TRUE))
    }
    if (toClose)
        RNetCDF::close.nc(nc)
    return(ret)
}

# List all Ugrid NetCDF files in a folder
listUgirdNc <- function(path, pattern=getOption("ugrid.pattern")) {

    if (!chkChr(pattern))
        pattern="\\.nc$"
    ncfiles <- list.files(path, pattern=pattern, ignore.case=TRUE, full.names=TRUE)
    if (length(ncfiles) > 0) {
        isUgrid <- sapply(ncfiles, isUgridNc)
        ncfiles <- ncfiles[isUgrid]
    }
    return(ncfiles)
}

#' R6Class for managing simulations
CaseManager <- R6::R6Class(
    "CaseManager",
    public = list(
        #' @description
        #' Initialize a CaseManager object.
        #' @param caseLst Character vector of folder-paths of cases.
        #' @param pattern Regex pattern for listing NetCDF files.
        #' @param crs ID of the coordinate reference system used in the NetCDF. A number will be understood as an EPSG code.
        #' In case no information about CRS found in the NetCDF, this will be assigned as CRS for the dataset.
        #' It must have the length 1 (will be recycled) or the same length as `caseLst`.
        #' @param newCrs ID of a new coordinate reference system to transform the exported layers to.
        #' @param ignoreCrsInFile If TRUE, the CRS information in the NetCDF file will be ignored.
        #' Like `crs`, it must the length 1 (will be recycled) or the same length as `caseLst` also.
        #' A number will be also understood as an EPSG code.
        initialize = function(caseLst=NULL, pattern=getOption("ugrid.pattern"),
                              crs=NA_character_, newCrs=NA_character_,
                              ignoreCrsInFile=FALSE) {

            if (!chkChr(pattern))
                pattern="\\.nc$"
            self$addCases(caseLst=caseLst, pattern=pattern, crs=crs, newCrs=newCrs, ignoreCrsInFile=ignoreCrsInFile)
        },
        #' @description
        #' Initialize a ProjectManager object.
        #' @param caseLst Character vector of folder-paths of cases.
        #' @param pattern Regex pattern for listing NetCDF files.
        #' @param crs ID of the coordinate reference system used in the NetCDF. A number will be understood as an EPSG code.
        #' In case no information about CRS found in the NetCDF, this will be assigned as CRS for the dataset.
        #' It must have the length 1 (will be recycled) or the same length as `caseLst`.
        #' @param newCrs ID of a new coordinate reference system to transform the exported layers to.
        #' Like `crs`, it must the length 1 (will be recycled) or the same length as `caseLst` also.
        #' A number will be also understood as an EPSG code.
        #' @param ignoreCrsInFile If TRUE, the CRS information in the NetCDF file will be ignored.
        addCases = function(caseLst=NULL, pattern=getOption("ugrid.pattern"), crs=NA_character_, newCrs=NA_character_,
                            ignoreCrsInFile=FALSE) {

            if (!chkChr(pattern))
                pattern="\\.nc$"
            ret <- vector(mode = "logical", length = length(caseLst))
            chk0 <- rlang::is_named(caseLst) & (length(caseLst) > 0)
            if (!(chk0)) {
                rlang::warn(paste("caseLst must be a named and has more than one member. Names must be unique and not already in:",
                                   paste(self$cases, collapse=", ")))
                return(ret)
            }
            chk1 <- (length(crs) == 1) || (length(crs) == length(caseLst)) &
                    (length(newCrs) == 1) || (length(newCrs) == length(caseLst)) &
                    (length(ignoreCrsInFile) == 1) || (length(ignoreCrsInFile) == length(caseLst))
            if (!chk1) {
                rlang::warn("crs, newCrs, and ignoreCrsInFile must have length 1 or same length as caseLst!")
                return(ret)
            }
            normalize <- function(x) {
                if (is.na(x)) NA_character_
                else if (!is.na(suppressWarnings(as.integer(x)))) paste0("EPSG:", x)
                else x
            }
            crs <- sapply(crs, normalize, USE.NAMES=FALSE)
            if (length(crs) != length(caseLst))
                crs <- rep(crs, length(caseLst))
            newCrs <- sapply(newCrs, normalize, USE.NAMES=FALSE)
            if (length(newCrs) != length(caseLst))
                newCrs <- rep(newCrs, length(caseLst))
            if (length(ignoreCrsInFile) != length(caseLst))
                ignoreCrsInFile <- rep(ignoreCrsInFile, length(caseLst))
            for (i in seq_along(caseLst)) {
                cName <-  names(caseLst)[i]
                if (cName %in% self$cases)
                    next
                ncFiles <- list.files(path=caseLst[[i]], pattern=pattern, full.names=TRUE, ignore.case=TRUE)
                if (length(ncFiles) < 1) {
                    rlang::warn(paste("No nc files found with pattern:", pattern, "in folder:", caseLst[i]))
                } else {
                    tbl <- data.table::data.table(path=ncFiles)
                    tbl[, caseName := cName]
                    tbl[, ncName := basename(path)]
                    tbl[, hash := sapply(path, digest::sha1)]
                    tbl[, crsid := crs[i]][, newCrsid := newCrs[i]][, ignoreFileCrs := ignoreCrsInFile[i]]
                    tbl <- tbl[!hash %in% self$tbl$hash]
                    if (nrow(tbl) < 1) {
                        rlang::warn(paste("Nothing added. Files from:,", caseLst[i], "are already in the case table."))
                    } else {
                        self$cases <- c(self$cases, cName)
                        self$tbl <- rbind(self$tbl, tbl)
                        ret[i] <- TRUE
                    }
                }
            }
            return(ret)
        },
        #' @description
        #' add Ugrid object to the manager.
        #' @param path Path to NetCDF file.
        #' @param overwrite Set TRUE to overwrite the current object, if exists
        #' @param ... Other parameters for initializing the Ugrid object.
        add = function(path, overwrite=FALSE, ...) {

            if (!rlang::is_scalar_character(path)) {
                message("path must be a single string.")
                return(NULL)
            }
            chk <- isUgridNc(path)
            if (!chk) {
                message("File: ", path, " does not comply with the UGRID convention!")
                return(NULL)
            }
            thisHash <- digest::sha1(path)
            chk <- !(thisHash %in% names(self$ugrids)) | overwrite
            if (chk) {
                thisCrs <- self$tbl[hash == thisHash, crsid]
                thisNewCrs <- self$tbl[hash == thisHash, newCrsid]
                thisIgnoreCrs <- self$tbl[hash == thisHash, ignoreFileCrs]
                self$ugrids[[thisHash]] <- Ugrid$new(ncFile=path, crs=thisCrs, newCrs=thisNewCrs,
                                                     ignoreCrsInFile=thisIgnoreCrs, ...)
                if (!path %in% self$tbl$path)
                    self$tbl <- rbind(self$tbl,
                                      data.table::data.table(path=path, hash=hash, ncName=basename(path)))
            }

            invisible(self$ugrids[[thisHash]])
        },
        #' @description
        #' Get Ugrid object from the manager.
        #' @param id Path to NetCDF file or its hash.
        get = function(id) {

            if (!rlang::is_scalar_character(id)) {
                message("id must be a single string.")
                return(NULL)
            }
            if (id %in% names(self$ugrids)) {
                ret <- self$ugrids[[id]]
            } else if (id %in% self$tbl$path) {
                thisHash <- self$tbl[path == id, hash]
                ret <- self$ugrids[[thisHash]]
            } else {
                message("Object with path or hash: ", id, " not found!")
                ret <- NULL
            }

            return(ret)
        },
        #' @field tbl A data.table with path to NetCDF files and their hashes
        tbl=NULL,
        #' @field cases Vector of case names.
        cases=NULL,
        #' @field ugrids reactiveValues holding Ugrid objects.
        #' It does not necessarily contain all objects of NetCDF files in the tbl.
        ugrids=list()
    )
)

addCases <- function(caseLst, cman, pattern=getOption("ugrid.pattern"),
                     crs=NA_character_, newCrs=NA_character_,
                     ignoreCrsInFile=FALSE, initUgrid=FALSE) {

    if (!chkChr(pattern))
        pattern <- "\\.nc$"
    msg <- rep(FALSE, length(caseLst))
    chk0 <- rlang::is_named(caseLst) & (length(caseLst) > 0)
    if (!(chk0)) {
        shiny::showNotification(
            paste("caseLst must be a named and has more than one member. Names must be unique and not already in:",
                  paste(cman$cases, collapse=", ")))
        return(msg)
    }
    chk1 <- ((length(crs) == 1) || (length(crs) == length(caseLst))) &
        ((length(newCrs) == 1) || (length(newCrs) == length(caseLst))) &
        ((length(ignoreCrsInFile) == 1) || (length(ignoreCrsInFile) == length(caseLst)))
    if (!chk1) {
        shiny::showNotification("crs, newCrs, and ignoreCrsInFile must have length 1 or same length as caseLst!")
        return(msg)
    }
    normalize <- function(x) {
        if (rlang::is_na(x) | rlang::is_null(x)) NA_character_
        else if (!is.na(suppressWarnings(as.integer(x)))) paste0("EPSG:", x)
        else x
    }
    crs <- sapply(crs, normalize, USE.NAMES=FALSE)
    if (length(crs) != length(caseLst))
        crs <- rep(crs, length(caseLst))
    newCrs <- sapply(newCrs, normalize, USE.NAMES=FALSE)
    if (length(newCrs) != length(caseLst))
        newCrs <- rep(newCrs, length(caseLst))
    if (length(ignoreCrsInFile) != length(caseLst))
        ignoreCrsInFile <- rep(ignoreCrsInFile, length(caseLst))
    for (i in seq_along(caseLst)) {
        cName <-  names(caseLst)[i]
        if (cName %in% cman[["cases"]])
            next
        ncFiles <- list.files(path=caseLst[[i]], pattern=pattern, full.names=TRUE, ignore.case=TRUE)
        if (length(ncFiles) < 1) {
            shiny::showNotification(paste("No nc files found with pattern:", pattern, "in folder:", caseLst[i]))
        } else {
            tbl <- data.table::data.table(path=ncFiles)
            tbl[, caseName := cName]
            tbl[, ncName := basename(path)]
            tbl[, hash := sapply(path, digest::sha1)]
            tbl[, crsid := crs[i]][, newCrsid := newCrs[i]][, ignoreFileCrs := ignoreCrsInFile[i]]
            tbl <- tbl[!hash %in% cman$tbl$hash]
            if (nrow(tbl) < 1) {
                shiny::showNotification(paste("Nothing added. Files from:,", caseLst[i], "are already in the case table."))
            } else {
                if (initUgrid) {
                    nCores <- parallel::detectCores()
                    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
                    `%dopar%` <- foreach::`%dopar%`
                    meshes <- foreach::foreach(i=1:length(tbl$path), .combine=c) %dopar% {
                        fpath <- tbl[i, path]
                        thisHash <- digest::sha1(fpath)
                        thisCrs <- tbl[hash == thisHash, crsid]
                        thisNewCrs <- tbl[hash == thisHash, newCrsid]
                        thisIgnoreCrs <- tbl[hash == thisHash, ignoreFileCrs]
                        thisMesh <- Ugrid$new(ncFile=fpath, crs=thisCrs, newCrs=thisNewCrs,
                                              ignoreCrsInFile=thisIgnoreCrs, domainInfo=TRUE)
                        if (is(thisMesh, "Ugrid")) {
                            thisMesh$buildFace2DPoly()
                        }
                        ret <- list(thisMesh)
                        names(ret) <- thisHash
                        return(ret)
                    }
                    if (length(meshes) > 0)
                        cman$ugrids <- c(cman$ugrids, meshes)
                }
                cman$cases <- c(cman$cases, cName)
                cman$tbl <- rbind(cman$tbl, tbl)
                msg[i] <- TRUE
            }
        }
    }
    return(msg)
}

addUgrid <- function(path, cman, overwrite=FALSE) {

    if (!rlang::is_scalar_character(path)) {
        message("path must be a single string.")
        return(NULL)
    }
    chk <- isUgridNc(path)
    if (!chk) {
        message("File: ", path, " does not comply with the UGRID convention!")
        return(NULL)
    }
    thisHash <- digest::sha1(path)
    chk <- !(thisHash %in% names(cman$ugrids)) | overwrite
    if (chk) {
        thisCrs <- cman$tbl[hash == thisHash, crsid]
        thisNewCrs <- cman$tbl[hash == thisHash, newCrsid]
        thisIgnoreCrs <- cman$tbl[hash == thisHash, ignoreFileCrs]
        cman$ugrids[[thisHash]] <- Ugrid$new(ncFile=path, crs=thisCrs, newCrs=thisNewCrs,
                                             ignoreCrsInFile=thisIgnoreCrs, domainInfo=TRUE)
        if (!path %in% cman$tbl$path)
            cman$tbl <- rbind(cman$tbl,
                              data.table::data.table(path=path, hash=hash, ncName=basename(path)))
    }

    invisible(cman$ugrids[[thisHash]])
}

getUgrid <- function(id, cman) {

    if (!rlang::is_scalar_character(id)) {
        message("id must be a single string.")
        return(NULL)
    }
    if (id %in% names(cman$ugrids)) {
        ret <- cman$ugrids[[id]]
    } else if (id %in% cman$tbl$path) {
        thisHash <- cman$tbl[path == id, hash]
        ret <- cman$ugrids[[thisHash]]
    } else {
        message("Object with path or hash: ", id, " not found!")
        ret <- NULL
    }

    return(ret)
}


initCaseManager <- function(
        caseLst=NULL, pattern=getOption("ugrid.pattern"),
        crs=NA_character_, newCrs=NA_character_,
        ignoreCrsInFile=FALSE, initUgrid=FALSE) {

    if (!chkChr(pattern))
        pattern <- "\\.nc$"
    cman <- shiny::reactiveValues(tbl=NULL, ugrids=list(), cases=vector(mode="character"))
    if (length(caseLst) > 0)
        addCases(cman, caseLst=caseLst, pattern=pattern, crs=crs,
                 newCrs=newCrs, ignoreCrsInFile=ignoreCrsInFile, initUgrid=initUgrid)
    return(cman)
}
