#' R6Class to store UGRID-Data
#' @export
Ugrid <- R6::R6Class(
    "Ugrid", # UpperCamelCase is standard for R6 Class-Name
    private = list(
        #' @description
        #' Read basic information of the topologies stored in the NetCDF file
        readInfo = function() {
            vars <- getVarTbl(nc=self$nc)
            dims <- getDimTbl(nc=self$nc)
            atts <- getAllAtts(nc=self$nc)
            vars[!is.na(dim1), dim1Name := dims[.SD, on = .(id = dim1), name]]
            vars[!is.na(dim2), dim2Name := dims[.SD, on = .(id = dim2), name]]
            vars[!is.na(dim3), dim3Name := dims[.SD, on = .(id = dim3), name]]
            vars <- vars[ndims > 0]
            atts2 <- atts[grepl("^standard_name|^long_name|FillValue|^unit", name, ignore.case=TRUE),
                             c("name", "val", "varName")] |>
                unique() |>
                dcast(varName ~ name , value.var = "val")
            vCols <- colnames(atts2) |>
                stringi::stri_replace_all_regex("units", "unit", opts_regex=list(case_insensitive=TRUE)) |>
                stringi::stri_replace_all_regex(".*FillValue$", "fill_value", opts_regex=list(case_insensitive=TRUE))
            colnames(atts2) <- vCols
            atts2[, fill_value := as.numeric(fill_value)]
            atts2[is.na(standard_name) | (nchar(standard_name) < 1), standard_name := varName]
            atts2[is.na(long_name) | (nchar(long_name) < 1), long_name := varName]
            vars <- merge(vars, atts2, by.x="name", by.y="varName", all.x=TRUE)
            vars[, hasTime := mapply(
                function(...) grepl("/time", paste("/", ..., sep="/")),
                dim1Name, dim2Name, dim3Name, USE.NAMES = FALSE)]
            self$vars <- vars
            self$dims <- dims
            # self$atts <- atts
            topoVars <- atts[grepl("^mesh_topology$", val), varName] # there should be max 3 mesh_topologies
            topo1D <- atts[grepl("topology_dimension", name) & val == "1", varName]
            if (length(topo1D) == 1) {
                atts1D <- atts[varName == topo1D]
                m1DTopo <- as.list(atts1D$val)
                names(m1DTopo) <- atts1D$name
                if ("node_coordinates" %in% atts1D$name)
                    m1DTopo$node_coordinates <- unlist(strsplit(m1DTopo$node_coordinates, " ", fixed=TRUE))
                if ("edge_coordinates" %in% atts1D$name)
                    m1DTopo$edge_coordinates <- unlist(strsplit(m1DTopo$edge_coordinates, " ", fixed=TRUE))
                node1DDimId <- dims[name == m1DTopo$node_dimension, id]
                # self$mesh1D <- mesh1D
                node1DVars <- vars[dim1 == node1DDimId | dim2 == node1DDimId]
                # manually duplicated standard_names
                # TODO: find out what meaning of the others
                node1DVars <- node1DVars[!grepl("coordinates forming flow element|flowelem_bl",
                                                long_name) &
                                             !grepl("_coordinate$|_Numlimdt", standard_name)]
                node1DVarStdNames <- as.list(node1DVars$name)
                names(node1DVarStdNames) <- node1DVars$standard_name
                node1DVarStdNames$node_x <- m1DTopo$node_coordinates[1]
                node1DVarStdNames$node_y <- m1DTopo$node_coordinates[2]
                edge1DDimId <- dims[name == m1DTopo$edge_dimension, id]
                edge1DVars <- vars[(dim1 == edge1DDimId | dim2 == edge1DDimId) &
                                       !grepl("_coordinate$|_Numlimdt", standard_name)]
                edge1DVarStdNames <- as.list(edge1DVars$name)
                names(edge1DVarStdNames) <- edge1DVars$standard_name
                edge1DVarStdNames$edge_x <- m1DTopo$edge_coordinates[1]
                edge1DVarStdNames$edge_y <- m1DTopo$edge_coordinates[2]

                # edge-node start index
                ensi <- atts[varName == m1DTopo$edge_node_connectivity &
                                 name == "start_index", as.integer(val)]
                if (!rlang::is_bare_integer(ensi, 1))
                    ensi <- 0
                m1DTopo$ensi <- ensi
                self$m1D <- list(topo=m1DTopo, node=node1DVarStdNames, edge=edge1DVarStdNames)
            }
            topo2D <- atts[grepl("topology_dimension", name) & val == "2", varName]
            if (length(topo2D) == 1) {
                atts2D <- atts[varName == topo2D]
                m2DTopo <- as.list(atts2D$val)
                names(m2DTopo) <- atts2D$name
                # node coordinates must be always there.
                m2DTopo$node_coordinates <- unlist(strsplit(m2DTopo$node_coordinates, " ", fixed=TRUE))
                if ("edge_coordinates" %in% atts2D$name)
                    m2DTopo$edge_coordinates <- unlist(strsplit(m2DTopo$edge_coordinates, " ", fixed=TRUE))
                if ("face_coordinates" %in% atts2D$name)
                    m2DTopo$face_coordinates <- unlist(strsplit(m2DTopo$face_coordinates, " ", fixed=TRUE))
                node2DDimId <- dims[name == m2DTopo$node_dimension, id]
                node2DVars <- vars[(dim1 == node2DDimId | dim2 == node2DDimId | dim3 == node2DDimId) &
                                       !grepl("_coordinate$|_Numlimdt", standard_name)]
                node2DVarStdNames <- as.list(node2DVars$name)
                names(node2DVarStdNames) <- node2DVars$standard_name
                node2DVarStdNames$node_x <- m2DTopo$node_coordinates[1]
                node2DVarStdNames$node_y <- m2DTopo$node_coordinates[2]
                edge2DDimId <- dims[name == m2DTopo$edge_dimension, id]
                edge2DVars <- vars[ndims == 1 &
                                       (dim1 == edge2DDimId | dim2 == edge2DDimId | dim3 == edge2DDimId) &
                                       !grepl("_coordinate$|_Numlimdt", standard_name)]
                edge2DVarStdNames <- as.list(edge2DVars$name)
                names(edge2DVarStdNames) <- edge2DVars$standard_name
                edge2DVarStdNames$edge_x <- m2DTopo$edge_coordinates[1]
                edge2DVarStdNames$edge_y <- m2DTopo$edge_coordinates[2]
                face2DDimId <- dims[name == m2DTopo$face_dimension, id]
                face2DVars <- vars[(dim1 == face2DDimId | dim2 == face2DDimId | dim3 == face2DDimId) &
                                       !grepl("_coordinate$|_Numlimdt", standard_name)]
                faceDupStd <- face2DVars[duplicated(standard_name), standard_name]
                face2DVars[standard_name %in% faceDupStd,
                           standard_name := paste0(standard_name, sub(topo2D, "", name))]
                face2DVarStdNames <- as.list(face2DVars$name)
                names(face2DVarStdNames) <- face2DVars$standard_name
                face2DVarStdNames$face_x <- m2DTopo$face_coordinates[1]
                face2DVarStdNames$face_y <- m2DTopo$face_coordinates[2]
                # for mesh2d with layered 3d
                interfaceDimId <- dims[grepl("interface", name, ignore.case = TRUE), id]
                interfaceStdNames <- NULL
                if (length(interfaceDimId) > 0) {
                    interfaceVars <- vars[(dim1 == interfaceDimId | dim2 == interfaceDimId |
                                               dim3 == interfaceDimId) &
                                              !grepl("_coordinate$|_Numlimdt", standard_name)]
                    interfaceDupStd <- interfaceVars[duplicated(standard_name), standard_name]
                    interfaceVars[standard_name %in% interfaceDupStd,
                               standard_name := paste0(standard_name, sub(topo2D, "", name))]
                    interfaceStdNames <- as.list(interfaceVars$name)
                    names(interfaceStdNames) <- interfaceVars$standard_name
                }
                layerDimId <- dims[grepl("layer", name, ignore.case = TRUE), id]
                layerStdNames <- NULL
                if (length(layerDimId) > 0) {
                    layerVars <- vars[(dim1 == layerDimId | dim2 == layerDimId |
                                               dim3 == layerDimId) &
                                              !grepl("_coordinate$|_Numlimdt", standard_name)]
                    layerDupStd <- layerVars[duplicated(standard_name), standard_name]
                    layerVars[standard_name %in% layerDupStd,
                                  standard_name := paste0(standard_name, sub(topo2D, "", name))]
                    layerStdNames <- as.list(layerVars$name)
                    names(layerStdNames) <- layerVars$standard_name
                }
                # edge-node start index
                if (length(m2DTopo$edge_node_connectivity) == 1) {
                    ensi <- atts[varName == m2DTopo$edge_node_connectivity &
                                     name == "start_index", as.integer(val)]
                    if (!rlang::is_bare_integer(ensi, 1))
                        ensi <- 0
                    m2DTopo$ensi <- ensi
                }
                # face-node start index
                if (length(m2DTopo$face_node_connectivity) == 1) {
                    fnsi <- atts[varName == m2DTopo$face_node_connectivity &
                                     name == "start_index", as.integer(val)]
                    if (!rlang::is_bare_integer(fnsi, 1))
                        fnsi <- 0
                    m2DTopo$fnsi <- fnsi
                }
                # edge-face start index
                if (length(m2DTopo$edge_face_connectivity) == 1) {
                    efsi <- atts[varName == m2DTopo$edge_face_connectivity &
                                     name == "start_index", as.integer(val)]
                    if (!rlang::is_bare_integer(efsi, 1))
                        efsi <- 0
                    m2DTopo$efsi <- efsi
                }
                self$m2D <- list(topo=m2DTopo, node=node2DVarStdNames,
                                 edge=edge2DVarStdNames, face=face2DVarStdNames,
                                 interface=interfaceStdNames, layer=layerStdNames)
            }
            topo3D <- atts[grepl("topology_dimension", name) & val == "3", varName]
            if (length(topo3D) == 1) {
                self$m3D <- atts[varName == topo3d]
                # TODO: deploying 3D variables
            }
            t0 <- atts[varName == "time" & name == "units", unlist(val)]
            # time variable is normally only for output data. Model-network does not have time dimension.
            if (length(t0) == 1) {
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
                self$totalTs <- length(ts)
            }

            if (!self$ignoreCrsInFile) {
                # Try to read coordinate system from attributes
                # The crs that was given by initializing will be replaced.
                crsAtt <- atts[grepl("coordinate_system", varName)]
                projStr <- crsAtt[grepl("proj_string|proj4_params", name), unlist(val)]
                wkt <- crsAtt[grepl("wkt", name), unlist(val)]
                epsg <- crsAtt[grepl("epsg", name, ignore.case=TRUE), unlist(val)][1]
                epsg <- paste0("EPSG:", epsg)
                for (x in list(wkt, epsg, projStr)) {
                    thisCrs <- tryCatch(suppressWarnings(sf::st_crs(x)), error=function(e) NULL)
                    if (!is.null(thisCrs))
                        break
                }
                if (is.null(thisCrs))
                    message("Cannot detect CRS from NetCDF file.")
                else
                    self$crs <- thisCrs
            }
        },
        #' @description
                #' Check if the pointer to NetCDF still alive
                #' If not, try to open the NetCDF file again.
        reOpen = function() {
            ptr <- attributes(self$nc)
            if (identical(ptr$handle_ptr, new("externalptr")))
                self$nc <- RNetCDF::open.nc(self$path)
        },
        #' @description
                #' Close the NetCDF connection
        finalize = function() {
            RNetCDF::close.nc(self$nc)
        }
    ),
    active = list(
        #' @field tf active binding to check the transformation ability of CRS
        #' @return a logical value
        tf = function() {
            sf::st_can_transform(self$crs, self$newCrs)
        }
    ),
    public = list(
        #' @description
        #' Initialization
        #' @param ncFile Path to 'UGRID-NetCDF' file.
        #' @param crs Coordinate reference system used in the NetCDF.
        #' It must be a character in format authority:code like EPSG:4326 or an object of class crs.
        #' An integer is also accepted with an assumption that it is an EPSG-code.
        #' @param newCrs New coordinate reference system to transform the exported layers to.
        #' @param domainInfo Read information about domains?
        #' @param ignoreCrsInFile If TRUE, the CRS information in the NetCDF file will be ignored.
        initialize = function(ncFile, crs=NULL, newCrs=NULL, domainInfo=FALSE, ignoreCrsInFile=FALSE) {

            stopifnot(file.exists(ncFile))
            self$nc <- RNetCDF::open.nc(ncFile)
            self$path <- ncFile
            self$name <- basename(ncFile)
            if (chkDbl(crs) | chkChr(crs)) {
                if (!is.na(suppressWarnings(as.integer(crs))))
                    crs <- paste0("EPSG:", crs)
                crs <- sf::st_crs(crs)
                if (!is.na(crs)) {
                    self$crs <- crs
                } else {
                    warning("crs is not valid. It must be a crs object or text in format authority:code like EPSG:4326.
                        An integer will be also accepted with an assumption that it is an EPSG-code.")
                }
            }
            if (chkDbl(newCrs) | chkChr(newCrs)) {
                if (!is.na(suppressWarnings(as.integer(newCrs))))
                    newCrs <- paste0("EPSG:", newCrs)
                newCrs <- sf::st_crs(newCrs)
                if (!is.na(newCrs)) {
                    self$newCrs <- newCrs
                } else {
                    warning("newCrs is not valid. It must be a crs object or text in format authority:code like EPSG:4326.
                        An integer will be also accepted with an assumption that it is an EPSG-code.")
                }
            }
            self$ignoreCrsInFile <- ignoreCrsInFile
            private$readInfo()
            if (isTRUE(domainInfo))
                self$readDomainInfo()
            invisible(self)
        },
        #' @description
        #' Find NetCDF variable based on standard_name or name (case-insensitive).
        #' @param variable Character of standard name (UGRID) or name of the variable.
        #' @param topo Topology (m1D, m2D, or m3D).
        #' @param at Type of elements (node, edge, face, interface, layer, or volume).
        getVarName = function(variable,
                              topo=c("m2D", "m1D", "m3D"),
                              at=c("face", "node", "edge", "volume", "interface", "layer")) {

            topo <- match.arg(topo)
            at <- match.arg(at)
            if (!chkChr(variable)) {
                ncVar <- NULL
            } else {
                ncVar <- grep(paste0("^", variable, "$"), self[[topo]][[at]], ignore.case=TRUE, value=TRUE)
                if (!chkChr(ncVar)) {
                    vars <- self$vars[name %in% self[[topo]][[at]]]
                    ncVar <- vars[grepl(variable, standard_name, ignore.case=TRUE), name]
                    if (!chkChr(ncVar)) {
                        ncVar <- vars[grepl(variable, long_name, ignore.case=TRUE), name]
                        if (!chkChr(ncVar)) {
                            message("Variable with name: ", variable,
                                    " does not found or the name is ambigious. Result: ",
                                    paste(ncVar, collapse=","))
                            ncVar <- NULL
                        }
                    }
                }
            }
            return(ncVar)
        },
        #' @description
        #' Check if a variable has layered data
        #' @param variable Character of standard name (UGRID) or name of the variable
        hasLayerData = function(variable) {
            ret <- self$vars[
                grepl(variable, name) |
                    grepl(variable, long_name) | grepl(variable, standard_name), length(name) == 1]
            return(ret)
        },
        #' @description
        #' Read data at nodes of 1D-Topology
        #' @param variable Character of standard name (UGRID) or name of the variable
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        getData4Node1D = function(variable, force=FALSE, ...) {

            ret <- self$getData4Any(variable=variable, force=force, topo="m1D", at="node", ...)
            invisible(ret)
        },
        #' @description
        #' Read data at edges of 1D-Topology
        #' @param variable Character of standard name (UGRID) or name of the variable
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        getData4Edge1D = function(variable, force=FALSE, ...) {

            ret <- self$getData4Any(variable=variable, force=force, topo="m1D", at="edge", ...)

            invisible(ret)
        },
        #' @description
        #' Read data at nodes of 2D-Topology
        #' @param variable Character of standard name (UGRID) or name of the variable.
        #' @param lyr layer or interface indexes, or "all" for the whole dataset.
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        getData4Node2D = function(variable, lyr="all", force=FALSE, ...) {

            ret <- self$getData4Any(variable=variable, lyr=lyr, force=force, topo="m2D", at="node", ...)

            invisible(ret)
        },
        #' @description
        #' Read data at edges of 2D-Topology
        #' @param variable Character of standard name (UGRID) or name of the variable
        #' @param lyr layer or interface indexes, or "all" for the whole dataset.
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        getData4Edge2D = function(variable, lyr="all", force=FALSE, ...) {

            ret <- self$getData4Any(variable=variable, lyr=lyr, force=force, topo="m2D", at="edge", ...)

            invisible(ret)
        },
        #' @description
        #' Read data at faces of 2D-Topology
        #' @param variable Character of standard name (UGRID) or name of the variable.
        #' @param lyr layer or interface indexes, or "all" for the whole dataset.
        #' @param onlyMain If TRUE, the cell elements of other domains will be removed.
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        getData4Face2D = function(variable, lyr="all", onlyMain=FALSE, force=FALSE, ...) {

            ret <- self$getData4Any(variable=variable, lyr=lyr, force=force, topo="m2D", at="face", ...)

            if (onlyMain & chkChr(self$m2D$face$cell_domain_number)) {
                self$readDomainInfo()
                nd <- length(dim(ret))
                if (nd > 2)
                    ret <- ret[, self$m2D$fids, ]
                else if (nd == 2)
                    ret <- ret[self$m2D$fids, ]
                else
                    ret <- ret[self$m2D$fids]
            }
            invisible(ret)
        },
        #' @description
        #' Read data for any topology
        #' @param variable Character of standard name (UGRID) or name of the variable
        #' @param topo Topology (m1D, m2D, or m3D)
        #' @param at Type of elements (node, edge, face, interface, or volume)
        #' @param lyr layer or interface indexes, or "all" for the whole dataset.
        #' @param force Logical. Force to read from NetCDF or first get from cache?
        #' @param ... will be forwarded to `RNetCDF::var.get.nc`
        #' @returns A matrix or a vector.
        #' In case of matrix, the ordering of dimensions are always: interface/layer (if any) x element x time.
        #' Some special variables like "Mapping from every edge to the two faces that it separates" are excluded.
        getData4Any = function(variable, topo, at, lyr="all", force=FALSE, ...) {

            private$reOpen()
            ncVar <- self$getVarName(variable, topo=topo, at=at)
            if (!chkChr(ncVar))
                return(NULL)
            dataSet <- sub("m", "data", topo)
            chk <- !is.null(self[[dataSet]][[at]][[variable]]) & !isTRUE(force)
            if (chk) {
                dta <- self[[dataSet]][[at]][[variable]]
            } else {
                dta <- RNetCDF::var.get.nc(self$nc, variable=ncVar, ...)
                varDim <- self$vars[name == ncVar, .SD, .SDcols = data.table::patterns("^dim")]
                varDim <- suppressWarnings(melt(varDim, measure.vars = list(dimId=1:3, dimName=4:6), variable.name = "tmp"))
                varDim[, dimIdx := .I][, tmp := NULL]
                varDim <- varDim[!is.na(dimId)]
                if (nrow(varDim) < 2) {
                    dta <- as.vector(dta)
                } else {
                    verticalDims <- c(self$m2D$topo$layer_dimension, self$m2D$topo$interface_dimension)
                    # time has the biggest index, and is therefore the last dimension
                    varDim[dimName == "time", idx := max(dimIdx)]
                    # element has the second biggest index
                    timeIdx <- varDim[dimName == "time", idx]
                    varDim[grepl("node[s]*$|edge[s]*$|face[s]*$", dimName, ignore.case = TRUE),
                           idx := ifelse(length(timeIdx), timeIdx - 1, dimIdx)]
                    # vertical dimension, layer or interface, if any, has the smallest index
                    varDim[dimName %in% verticalDims,
                           idx := min(dimIdx, na.rm = TRUE)]
                    newOrder <- c(
                        varDim[!grepl("node[s]*$|edge[s]*$|face[s]*$|time", dimName, ignore.case = TRUE), idx],
                        varDim[grepl("node[s]*$|edge[s]*$|face[s]*$", dimName, ignore.case = TRUE), idx],
                        varDim[dimName == "time", idx])
                    # make sure that the order of the matrix is layer/interface x element(face/edge/node) x time
                    if (!identical(varDim$dimIdx, newOrder)) {
                        tryCatch(dta <- aperm(dta, newOrder),
                                 error=function(e) message("Cannot reshape the result matrix"))
                    }
                }
                self[[dataSet]][[at]][[variable]] <- dta # store the whole dataset.
            }
            if (length(dim(dta)) > 2) {
                if (!identical(lyr, "all")) {
                    dta <- dta[lyr, , ]
                }
            }

            invisible(dta)
        },
        #' @description
        #' Get data on faces and assign it to the face polygon layer.
        #' @param variable Variable name for the data
        #' @param tsIdx Time index
        #' @param agg Option for aggregating the data by rows. The aggregation functions come from `matrixStats` package.
        #' @param lyr Index of the layer to get data.
        #' @param force If TRUE, the data stored in Ugrid object, if any, will be read again.
        #' @param onlyMain If TRUE, the cell elements of other domains will be removed.
        #' @param dryAsNa If TRUE (default), values for dried (waterlevel - elevation < 0) cells will be assigned NaN.
        getData4Polygon = function(variable, tsIdx=1L, agg="none", lyr=1L,
                                   force=FALSE, onlyMain=FALSE, dryAsNa=TRUE) {

            aP <- self$buildFace2DPoly()
            if (!is(aP, "sf")) {
                warning("Couldn't generate face-polygons for the file: ", self$path)
                return(NULL)
            }
            aD <- self$getData4Face2D(variable=variable, lyr=lyr, force=force)
            if (grepl("sea_surface_height", variable) & dryAsNa) {
                altitudeVar <- self$getVarName("altitude", topo="m2D", at="face")
                if (chkChr(altitudeVar)) {
                    altitude <- self$getData4Face2D(variable="altitude", lyr=lyr, force=force)
                    altitude <- as.vector(altitude)
                    dry <- abs(aD - altitude) < 1e-9
                    aD[dry] <- NaN
                } else {
                    warning("Cannot read altitude data. Values for dry faces ware not assigned as NaN.")
                }
            }
            ncVar <- self$getVarName(variable, topo="m2D", at="face")
            isTime <- self$vars[name == ncVar, isTRUE(hasTime)]
            if (onlyMain) {
                domains <- self$getData4Face2D("cell_domain_number", lyr=lyr)
                if (!chkDbl(self$md))
                    self$readDomainInfo()
                ids <- which(domains == self$md)
                aD <- if(isTime) aD[ids, ] else aD[ids]
                aP <- aP[ids, ]
            }
            if (isTime) {
                tsChk <- all(tsIdx %between% c(1, self$totalTs))
                if (tsChk) {
                    aD <- aD[, tsIdx]
                    attr(aP, "tsIdx") <- tsIdx
                } else if (agg %in% c("min", "max", "mean")) {
                    aggFun <- rowAgg(agg)
                    aD <- aggFun(aD)
                    attr(aP, "aggFun") <- paste0("rowAgg(\"", agg, "\")")
                }
            }
            digits <- if (grepl("velocity|speed", variable)) 5 else 3
            aD <- round(aD, digits=digits)
            tmp <- tryCatch(
                {aP[[variable]] <- aD},
                error=function(e) message("Couldn't assign result for: ", self$path, ".\n Reason: ", e))
            if (!is.null(tmp)) {
                aP[["ncName"]] <- basename(self$name)
                self$ret <- aP
            }
            invisible(self)
        },
        #' @description
        #' Build polygon layer for 2D-Topology
        #' @return sf object of POLYGON geometries with `faceID` field that reflexes the index of faces.
        buildFace2DPoly = function() {

            if (inherits(self$m2D$face2D, "sf"))
                return(invisible(self$m2D$face2D))
            chk <- (length(self$m2D$node$node_x) != 1 | length(self$m2D$node$node_y) != 1)
            if (chk) {
                warning("Couldn't find coordinate variables for face-nodes.")
                return(NULL)
            }
            nodeX <- self$getData4Node2D(self$m2D$node$node_x)
            nodeY <- self$getData4Node2D(self$m2D$node$node_y)
            nodeTbl <- data.table::data.table(X=nodeX, Y=nodeY)
            nodeTbl[, nodeID := .I]
            faceNode <- RNetCDF::var.get.nc(self$nc, self$m2D$topo$face_node_connectivity)
            faceNode <- t(faceNode)
            nFace <- nrow(faceNode)
            # because R has 1-based index, so if the start index is 0 (< 1) we have to add 1 to the indexes.
            if (self$m2D$topo$fnsi < 1)
                faceNode <- faceNode + 1
            faceNode <- data.table::as.data.table(faceNode)
            faceNode[, faceID := .I]
            faceNode <- data.table::melt(faceNode, id.vars="faceID", value.name="nodeID")
            faceNode <- faceNode[!is.na(nodeID)]
            faceNode <- merge(faceNode, nodeTbl, by="nodeID")
            faceNode[, nodeID := NULL]
            setorder(faceNode, faceID, variable)
            facePolygon <- tryCatch(
                sfheaders::sf_polygon(faceNode, x="X", y="Y", polygon_id="faceID", close=TRUE),
                error=function(e) {
                    warning("At least some polygons for NetCDF file: ", self$path, "are not closed.")
                    sfheaders::sf_polygon(faceNode, x="X", y="Y", polygon_id="faceID", close=FALSE)
                }
                )
            nPols <- nrow(facePolygon)
            if (nFace != nPols) {
                message("Number of generated polygons are not equal to number of faces. Aborted!")
                return(NULL)
            }
            sf::st_crs(facePolygon) <- self$crs
            if (self$tf)
                facePolygon <- sf::st_transform(facePolygon, self$newCrs)
            self$readDomainInfo()
            self$m2D$face2D <- facePolygon
            if (length(self$m2D$fids) > 0) {
                facePolygon <- facePolygon[self$m2D$fids, ] |>
                    sf::st_union() |> sf::st_as_sf()
                sf::st_geometry(facePolygon) <- "geometry"
                facePolygon$path <- self$path
                self$m2D$fRing <- facePolygon
            }
            message("The polygon is accessible at $m2D$face2D")

            invisible(self$m2D$face2D)
        },
        #' @description
        #' Build line layer for 1D-Topology
        buildEdge1DLine = function() {

            chk <- (length(self$m1D$node$node_x) != 1 | length(self$m1D$node$node_y) != 1)
            if (chk) {
                warning("Couldn't find coordinate variables for edge-nodes.")
                return(NULL)
            }
            nodeX <- self$getData4Node1D(variable=self$m1D$node$node_x)
            nodeY <- self$getData4Node1D(self$m1D$node$node_y)
            nodeTbl <- data.table::data.table(X = nodeX, Y = nodeY)
            nodeTbl[, nodeID := .I]
            edgeNode <- RNetCDF::var.get.nc(self$nc, self$m1D$topo$edge_node_connectivity)
            edgeNode <- t(edgeNode)
            # because R has 1-based index, so if the start index is 0 (< 1) we have to add 1 to the indexes.
            if (self$m1D$topo$ensi < 1)
                edgeNode <- edgeNode + 1
            edgeNode <- data.table::as.data.table(edgeNode)
            edgeNode[, edgeID := .I]
            edgeNode <- melt(edgeNode, id.vars="edgeID", value.name="nodeID")
            edgeNode <- edgeNode[!is.na(nodeID)]
            edgeNode <- merge(edgeNode, nodeTbl, by="nodeID")
            edgeNode[, nodeID := NULL]
            setorder(edgeNode, edgeID, variable)
            edgeLine <- sfheaders::sf_linestring(edgeNode, x="X", y="Y", linestring_id="edgeID")
            sf::st_crs(edgeLine) <- self$crs
            if (self$tf)
                edgeLine <- sf::st_transform(edgeLine, self$newCrs)
            self$m1D$line1D <- edgeLine

            invisible(edgeLine)
        },
        #' @description
        #' Read information about domains and identify the main domain
        readDomainInfo = function() {

            if (length(self$md) > 0)
                return(self$md)
            private$reOpen()
            if (chkChr(self$m2D$face$cell_domain_number)) {
                cdnVal <- RNetCDF::var.get.nc(self$nc, variable=self$m2D$face$cell_domain_number)
            } else if (chkChr(self$m1D$node$cell_domain_number)) {
                cdnVal <- RNetCDF::var.get.nc(self$nc, variable=self$m2D$node$cell_domain_number)
            } else if (chkChr(self$m3D$face$cell_domain_number)) {
                cdnVal <- RNetCDF::var.get.nc(self$nc, variable=self$m3D$face$cell_domain_number)
            } else {
                self$m2D$fids <- self$dims[name == self$m2D$topo$face_dimension, seq_len(length)]
                warning("Couldn't find cell_domain_number variable.")
                return(NULL)
            }
            domainTbl <- data.table(cdn = cdnVal)
            domainTbl <- unique(domainTbl[, dCount := .N, by=cdn])
            setorder(domainTbl, -dCount)
            # main cell domain number
            self$md <- domainTbl[1, cdn]
            # other neighbor domains
            self$od <- domainTbl[-1, cdn]
            self$m2D$fids <- seq_along(cdnVal)[cdnVal == domainTbl[1, cdn]]
            invisible(self$md)
        },

# class attributes-------------------------------------------------------------------------------------------------
        #' @field nc Nc object of the "UGRID-NetCDF" file
        nc=NULL,
        #' @field vars data.table of variables
        vars = data.table::data.table(),
        #' @field dims data.table of dimensions
        dims = data.table::data.table(),
        #' @field m1D list of attributes of 1D-topology
        m1D = list(),
        #' @field m2D list of attributes of 2D-topology
        m2D = list(),
        #' @field m3D list of attributes of 3D-topology
        m3D = list(),
        #' @field crs Coordinate Reference System of the original data
        crs = sf::st_crs(),
        #' @field newCrs New Coordinate Reference System
        newCrs = sf::st_crs(),
        #' @field path Path to NetCDF file
        path=character(0),
        #' @field name Name of NetCDF file
        name=character(0),
        #' @field data1D a named list of matrix stored the data for mesh1D-variables once read
        data1D=list(),
        #' @field data2D a named list of matrix stored the data for mesh2D-variables once read
        data2D=list(),
        #' @field data3D a named list of matrix stored the data for mesh3d-variables once read
        data3D=list(),
        #' @field tz Time zone
        tz=NULL,
        #' @field totalTs Number of timesteps
        totalTs=NULL,
        #' @field ts Vector of time series
        ts=NULL,
        #' @field md main cell domain number
        md=NULL,
        #' @field od other cell domain numbers
        od=NULL,
        #' @field ignoreCrsInFile If TRUE, the CRS information in the NetCDF file will be ignored.
        ignoreCrsInFile=FALSE,
        #' @field ret a field to store return result from a parallel process.
        #' So that we can get back also the whole R6 object,
        #' otherwise the changes to this R6 object that made during the parallel process will be lost.
        ret=NULL
    )
)


