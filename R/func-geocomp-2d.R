#' Get data for faces
#'
#' @param mesh a Ugrid object or a list of those.
#' @param variable Variable name for the data.
#' @param tsIdx Time index.
#' @param lyr layer or interface indexes, or "all" for the whole dataset.
#' @param agg Option for aggregating the data by rows. The aggregation functions come from `matrixStats` package.
#' @param force If TRUE, the data stored in Ugrid object, if any, will be read again.
#' @param onlyMain If TRUE, the cell elements of other domains will be removed.
#' @param dryAsNa If TRUE (default), values for dried (waterlevel - elevation < 0) cells will be assigned NaN.
#' @returns A sf object.
#' @keywords internal
getMapData <- function(mesh, variable, lyr=1L, tsIdx=1L, agg="none", force=FALSE, onlyMain=FALSE, dryAsNa=TRUE) {

    if (!is(mesh, "list"))
        mesh <- list(mesh)
    if (length(mesh) < 1)
        return(NULL)
    tsIdx <- as.integer(tsIdx)
    if (length(mesh) < 4) {
        retLst <- lapply(mesh, function(x, ...) x$getData4Polygon(...),
                         variable=variable, lyr=lyr, tsIdx=tsIdx,
                         agg=agg, onlyMain=onlyMain, dryAsNa=dryAsNa, force=force)
    } else {
        nCores <- parallel::detectCores()
        doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
        `%dopar%` <- foreach::`%dopar%`
        retLst <- foreach::foreach(x=mesh, .combine=c) %dopar% {
            x$getData4Polygon(variable=variable, lyr=lyr, tsIdx=tsIdx,
                              agg=agg, onlyMain=onlyMain, dryAsNa=dryAsNa, force=force)
            list(x)
        }
    }
    names(retLst) <- names(mesh)

    return(retLst)
}

#' Get data for faces
#'
#' @param mesh a Ugrid object or a list of those.
#' @param variable Variable name for the data.
#' @param tsIdx Time index.
#' @param agg Option for aggregating the data by rows. The aggregation functions come from `matrixStats` package.
#' @param lyr layer or interface index.
#' @param force If TRUE, the data stored in Ugrid object, if any, will be read again.
#' @param onlyMain If TRUE, the cell elements of other domains will be removed.
#' @returns A sf object.
#' @keywords internal
raster4Meshes <- function(mesh, variable, tsIdx=1L, agg="none", lyr=1L,
                       force=FALSE, onlyMain=FALSE) {
    if (!is(mesh, "list"))
        mesh <- list(mesh)
    if (length(mesh) < 1)
        return(NULL)
    tsIdx <- as.integer(tsIdx)
    if (length(mesh) < 4) {
        retLst <- lapply(mesh, function(x, ...) {
            x$getData4Polygon(variable=variable, tsIdx=tsIdx, agg=agg, lyr=lyr, force=force, onlyMain=onlyMain)
            v <- poly2Raster(x$ret, field=variable)
            x$ret <- v
            x
        },
        variable=variable, tsIdx=tsIdx, agg=agg, force=force, onlyMain=onlyMain)
    } else {
        nCores <- parallel::detectCores()
        doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
        `%dopar%` <- foreach::`%dopar%`
        retLst <- foreach::foreach(x=mesh, .combine=c) %dopar% {
            x$getData4Polygon(variable=variable, tsIdx=tsIdx, agg=agg, lyr=lyr, force=force, onlyMain=onlyMain)
            v <- poly2Raster(x$ret, field=variable)
            x$ret <- v
            list(x)
        }
    }
    names(retLst) <- names(mesh)

    return(retLst)
}

#' Get data for faces that are intersected with or crossed by a sf object
#'
#' @param x an sf object with only one feature
#' @inheritParams getMapData
#' @returns sf object of polygon (faces) with values of the variable and names of the source nc.
#' @export
getData4Sf <- function(
        x, mesh, variable, tsIdx=NULL, lyr=1L,
        agg=c("none", "max", "min", "mean"),
        onlyMain=TRUE
) {

    mesh <- getMapData(mesh=mesh, variable=variable,
                      tsIdx=tsIdx, agg=agg, lyr=lyr, onlyMain=onlyMain)
    pol <- lapply(mesh, function(x) x$ret)
    pol <- do.call(rbind, pol)
    pol <- sf::st_make_valid(pol)
    xtype <- sf::st_geometry_type(x)
    if (grepl("POINT|POLYGON", xtype)) {
        pred <- sf::st_intersects(x, pol) |> unlist()
    } else if (grepl("LINE", xtype)) {
        pred <- sf::st_crosses(x, pol) |> unlist()
    } else {
        pred <- NULL
    }
    return(pol[pred, ])
}


#' Get data at faces
#'
#' This functions extract the data at faces for some or all layers.
#'
#' @param mesh List of Ugrid objects.
#' @param variable Variable name for the data.
#' @param lyr layer or interface indexes, or "all" for the whole dataset.
#' @export
getFaceData4Var <- function(mesh, variable, lyr="all") {

    if (!is(mesh, "list"))
        mesh <- list(mesh)
    if (!any(sapply(mesh, is, "Ugrid"))) {
        message("mesh must be a list of Ugrid objects")
        return(NULL)
    }
    ncVar <- mesh[[1]]$getVarName(variable)
    if (length(ncVar) !=1) {
        message("Check variable. Nc variables for ", variable,": ", paste0(ncVar, collapse=", "))
        return(NULL)
    }
    woDta <- sapply(mesh, function(x) is.null(x$data2D$face[[variable]]))
    woMesh <- mesh[woDta]
    wiMesh <- mesh[!woDta]
    if (length(woMesh) %between% c(1, 4)) {
        woMesh <- lapply(woMesh, function(x) {
            x$getData4Face2D(variable=variable, lyr=lyr)
            x$buildFace2DPoly()
            x
        })
    } else if (length(woMesh) > 4){
        nCores <- parallel::detectCores()
        doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
        `%dopar%` <- foreach::`%dopar%`
        woMesh <- foreach::foreach(x=woMesh, .combine=c) %dopar% {
            x$getData4Face2D(variable=variable, lyr=lyr)
            x$buildFace2DPoly()
            list(x)
        }
    }
    meshOut <- c(woMesh, wiMesh)
    paths <- sapply(mesh, function(x) x$path)
    pathOut <- sapply(meshOut, function(x) x$path)
    pathIdx <- sapply(pathOut, function(x) which(paths==x), USE.NAMES = FALSE)
    ret <- meshOut[pathIdx]
    names(ret) <- names(mesh)
    return(ret)
}

#' Generate isolines from a sf object.
#'
#' @param x sf object that can form a regular grid.
#' @param field Field for values.
#' @param resolution Numeric vector of length 1 or 2 to set the spatial resolution.
#' @param nbin Number of bins
#' @param binWidth Bin-width, if given, nbin will be ignored.
#' @param asSf If TRUE, the generated raster will be converted to sf.
#' @return a SpatVector or sf LINESTRING vector object.
#' @export
genIsoline <- function(x, field, resolution=10, nbin=10,
                       binWidth=NULL, asSf=TRUE) {

    x1 <- terra::vect(x)
    # for a unknown reason, terra sometimes convert the values to characters!
    # that why we have to perform this step.
    x1[[field]] <- x[[field]]
    r <- terra::rasterize(
        x=x1,
        y=terra::rast(terra::ext(x), resolution=resolution),
        field=field, fun="mean")
    terra::crs(r) <- sf::st_crs(x)$wkt
    rRange <- terra::minmax(r)
    values <- terra::values(r)
    values <- values[!is.na(values)] |> unique()
    digits <- getOption("digits")
    if (chkDbl(digits))
        values <- round(values, digits=digits)
    rRange <- range(values)
    if (!is.null(binWidth)) {
        levels <- seq(from=rRange[1], to=rRange[2], by=binWidth)
    } else {
        levels <- classInt::classIntervals(values, n=nbin, style="pretty")$brks
    }
    contours <- terra::as.contour(r, levels=levels)
    if (asSf) {
        contours <- sf::st_as_sf(contours) |>
            sf::st_cast("LINESTRING")
    }

    return(contours)
}

poly2Raster <- function(pol, field, resolution=NULL, to4326=TRUE) {

    if (!inherits(pol, "sf")) {
        message("pol must be a sf polygon object")
        return(NULL)
    }
    if (!field %in% colnames(pol)) {
        return(NULL)
    }
    if (!rlang::is_bare_numeric(resolution)) {
        chk <- !all(grepl("POLYGON"), sf::st_geometry_type(pol, by_geometry=FALSE))
        if (chk) {
            message("If geometry type is not POLYGON/MULTIPOLYGON then resolution must be given.")
            return(NULL)
        }
        resolution <- sf::st_area(pol[sample.int(nrow(pol), 1), 1]) |> sqrt() |> pretty()
        resolution <- resolution[1]
        if (isTRUE(sf::st_is_longlat(pol))) {
            resolution <- pretty(resolution / (8*10^5)) # convert meter to degree
            resolution <- resolution[resolution > 0][1]
        }
    }
    x <- terra::vect(pol)
    x[[field]] <- pol[[field]]
    ret <- terra::rasterize(
        x=x,
        y=terra::rast(terra::ext(pol), resolution=resolution, crs=terra::crs(pol)),
        field=field, fun="mean")
    if (to4326)
        ret <- terra::project(ret, "epsg:4326")

    return(ret)
}

#' Generate functions for scaling coordinates from one bbox to another
#'
#' @param fromBb Origin bbox
#' @param toBb Destination bbox
#' @keywords internal
rescale <- function(fromBb,
                    toBb=c(-37, -44, -16, -30)
){

    dxDest <- diff(toBb[c(1,3)])
    dyDest <- diff(toBb[c(2,4)])
    dx <- dist(fromBb[c(1,3)])
    dy <- dist(fromBb[c(2,4)])
    fx <- function(x) (x - fromBb[1]) * dxDest / dx + toBb[1]
    fy <- function(y) (y - fromBb[2]) * dyDest / dy + toBb[2]
    fd <- function(d) d * (sqrt(dxDest^2 + dyDest^2) / sqrt(dx^2 + dy^2))
    return(list(fx=fx, fy=fy, fd=fd))
}

rescale2 <- function(x, toBb=c(-37, -44, -16, -30)){

    chk <- inherits(x, "sf") | inherits(x, "SpatRaster") | inherits(x, "SpatVector")
    if (!chk) {
        warning("x must be an object of class sf, SpatRaster or SpatVector")
        return(NULL)
    }
    fromBb <- sf::st_bbox(x)
    fx <-  (toBb[3] - toBb[1]) / (fromBb[3] - fromBb[1])
    fy <- (toBb[4] - toBb[2]) / (fromBb[4] - fromBb[2])
    x0 <- toBb[1] - fromBb[1] * fx
    y0 <- toBb[2] - fromBb[2] * fy
    isSf <- is(x, "sf")
    if (isSf)
        x <- terra::vect(x)
    ret <- terra::rescale(x, fx=fx, fy=fy)
    bbNew <- sf::st_bbox(ret)
    dx <- toBb[1] - bbNew[1]
    dy <- toBb[2] - bbNew[2]
    ret <- terra::shift(ret, dx=dx, dy=dy)
    if (isSf) {
        ret <- sf::st_as_sf(ret)
        sf::st_crs(ret) <- 4326
    } else {
        terra::crs(ret) <- "epsg:4326"
    }

    return(ret)
}

#' Squash the geometry of an sf object into another bbox
#'
#' @param geom a sf object.
#' @param toBb destination bbox.
#' @param rsf a list of rescaling functions returned from `rescale` function.
#' @keywords internal
squash2Bbox <- function(geom, toBb=c(-37, -44, -16, -30), rsf=NULL) {

    if (!all(is.function(rsf))) {
        bb <- sf::st_bbox(geom)
        rsf <- rescale(bb, toBb=toBb)
    }
    geomDf <- sfheaders::sf_to_df(geom) |>
        data.table::as.data.table()
    geomDf[, x := rsf$fx(x)]
    geomDf[, y := rsf$fy(y)]
    colNames <- colnames(geomDf)
    if ("multipolygon_id" %in% colNames) {
        ret <- sfheaders::sf_multipolygon(
            geomDf, x="x", y="y",
            polygon_id = "polygon_id", multipolygon_id="multipolygon_id", linestring_id="linestring_id")
        ret$polygon_id <- NULL
    } else if ("polygon_id" %in% colNames) {
        ret <- sfheaders::sf_polygon(
            geomDf, x="x", y="y",
            polygon_id = "polygon_id", linestring_id="linestring_id", close=FALSE)
        ret$polygon_id <- NULL
    } else if ("linestring_id" %in% colNames) {
        ret <- sfheaders::sf_linestring(
            geomDf, x="x", y="y", linestring_id="linestring_id")
        ret$linestring_id <- NULL
    }
    geomData <- sf::st_drop_geometry(geom)
    ret <- cbind(ret, geomData)
    sf::st_crs(ret) <- 4326

    return(ret)
}


#' Generate a LINESTRING sf object of velocity vectors
#'
#' @param x a Ugrid object.
#' @param tsIdx Index on time step.
#' @param lyr layer index.
#' @param onlyMain If TRUE, only elements of main domain will be taken.
#' @param baseLength Base length of the vector.
#' @param aRatio A ratio between vector head and vector length.
#' @param aOpen Open angle in degree of vector head.
#' @param aSide Vector head to both side or only one?
#' @param fillValue The fill value of the variable in the NetCDF.
#' @export
genVectorLayer <- function(
        x, tsIdx=NULL, lyr=1L, onlyMain=FALSE,
        baseLength=NULL, aRatio = 0.2, aOpen=30, aSide=2,
        fillValue=NULL) {

    chkVar <- c("sea_water_speed", "sea_water_x_velocity", "sea_water_y_velocity") %in%
        names(x$m2D$face)
    if (!all(chkVar)) {
        message("Not enough parameters in the NetCDF files!")
        return(NULL)
    }
    tsIdx <- as.integer(tsIdx)
    if (length(fillValue) < 1)
        fillValue <- x$vars[!is.na(fill_value), , fill_value[1]] |> as.numeric()
    if (length(fillValue) < 1)
        fillValue <- -Inf
    if (length(tsIdx) < 1)
        tsIdx <- x$totalTs
    faceX <- x$getData4Face2D(x$m2D$face$face_x, onlyMain=onlyMain)
    faceY <- x$getData4Face2D(x$m2D$face$face_y, onlyMain=onlyMain)
    ucxVar <- x$vars[grepl("sea_water_x_velocity", standard_name) & grepl("vector", long_name), name]
    ucyVar <- x$vars[grepl("sea_water_y_velocity", standard_name) & grepl("vector", long_name), name]
    ucx <- x$getData4Face2D(ucxVar, lyr=lyr, onlyMain=onlyMain)
    ucy <- x$getData4Face2D(ucyVar, lyr=lyr, onlyMain=onlyMain)
    ucmag <- sqrt(ucx ^ 2 + ucy ^ 2)
    if (x$vars[name == ucxVar, hasTime]) {
        ucx <- ucx[, tsIdx]
        ucy <- ucy[, tsIdx]
        ucmag <- ucmag[, tsIdx]
    }
    # if baseLength == NA_real_ it will not work
    hasBl <- chkDbl(baseLength)
    if (!hasBl) {
        x$buildFace2DPoly()
        baseLength <- sf::st_area(x$m2D$face2D[sample.int(n=nrow(x$m2D$face2D), size=1), ]) |>
            sqrt() |> as.numeric()
    }
    vecBeginPts <- data.table::data.table(X=faceX, Y=faceY, L=ucmag * baseLength, duX=ucx, duY=ucy)
    vecBeginPts[, A  := mapply(calcDirection, duX, duY,
                               MoreArgs=list(fillValue=fillValue), USE.NAMES = FALSE)]
    vecBeginPts <- vecBeginPts[A != fillValue]
    vecEndPts <- data.table::copy(vecBeginPts)
    vecBeginPts[, lid := paste0("v", .I)]
    vecEndPts[, X := X + L * cos(A * pi / 180)]
    vecEndPts[, Y := Y + L * sin(A * pi / 180)]
    arrPts <- data.table::copy(vecEndPts)
    vecEndPts[, lid := paste0("v", .I)]
    vecPts <- rbind(vecBeginPts[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
    arrPts[, A := A + 180 - aOpen]
    arrPts[, lid := paste0("a", .I)]
    arrPts[, X := X + L * aRatio * cos(A * pi / 180)]
    arrPts[, Y := Y + L * aRatio * sin(A * pi / 180)]
    vecEndPts[, lid := paste0("a", .I)]
    arrPts <- rbind(arrPts[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
    vec <- rbind(arrPts, vecPts)
    if (aSide > 1) {
        arrPts2 <- data.table::copy(vecEndPts)
        arrPts2[, A := A + 180 + aOpen]
        arrPts2[, lid := paste0("b", .I)]
        arrPts2[, X := X + L * aRatio * cos(A * pi / 180)]
        arrPts2[, Y := Y + L * aRatio * sin(A * pi / 180)]
        vecEndPts[, lid := paste0("b", .I)]
        arrPts2 <- rbind(arrPts2[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
        vec <- rbind(arrPts2, vec)
    }
    data.table::setorder(vec, lid)
    vec <- vec[!is.na(X)]
    ret <- NULL
    if (nrow(vec) > 0) {
        ret <- sfheaders::sf_linestring(vec, x="X", y="Y", linestring_id="lid")
        if (!is.na(x$crs)) {
            sf::st_crs(ret) <- x$crs
            if (x$tf)
                ret <- sf::st_transform(ret, x$newCrs)
        }
    }
    return(ret)
}

genVector4All <- function(
        mesh, lyr=1L, baseLength=NULL, aRatio = 0.2, aOpen=30, aSide=2,
        fillValue=NULL) {

    if (!is.list(mesh))
        mesh <- list(mesh)
    faceX <- lapply(mesh, function(x) x$getData4Face2D(x$m2D$face$face_x, onlyMain=TRUE)) |> unlist()
    faceY <- lapply(mesh, function(x) x$getData4Face2D(x$m2D$face$face_y, onlyMain=TRUE)) |> unlist()
    ucxVar <- mesh[[1]]$vars[grepl("sea_water_x_velocity", standard_name) & grepl("vector", long_name), name]
    ucyVar <- mesh[[1]]$vars[grepl("sea_water_y_velocity", standard_name) & grepl("vector", long_name), name]
    if (length(ucxVar) != 1 | length(ucyVar) != 1) {
        warning("Not enough velocity variables in the NetCDF files.")
        return(NULL)
    }
    ucx <- lapply(mesh, function(x) x$getData4Face2D(ucxVar, lyr=lyr, onlyMain=TRUE))
    ucx <- do.call(rbind, ucx)
    ucy <- lapply(mesh, function(x) x$getData4Face2D(ucyVar, lyr=lyr, onlyMain=TRUE))
    ucy <- do.call(rbind, ucy)
    if (all(is.na(ucx)) | all(is.na(ucy))) {
        warning("All velocity values are NaN")
        return(NULL)
    }
    ucmag <- sqrt(ucx^2 + ucy^2)
    hasBl <- chkDbl(baseLength)
    if (!hasBl) {
        fpol <- mesh[[1]]$buildFace2DPoly()
        baseLength <- sf::st_area(fpol[sample.int(n=nrow(fpol), size=1), ]) |>
            sqrt() |> as.numeric()
    }
    thisCrs <- mesh[[1]]$crs
    thisTf <- mesh[[1]]$tf
    thisNewCrs <- mesh[[1]]$newCrs
    thisFillValue <- mesh[[1]]$vars[name == ucxVar, fill_value]
    nCores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    retLst <- foreach::foreach(i=1:mesh[[1]]$totalTs, .combine=c) %dopar% {
        ucmagi <- ucmag[, i]
        ucxi <- ucx[, i]
        ucyi <- ucy[, i]
        ret <- tryCatch({genVector4One(
            faceX=faceX, faceY=faceY, ucmag=ucmagi, ucx=ucxi, ucy=ucyi,
            baseLength=baseLength, aRatio=aRatio, aOpen=aOpen, aSide=aSide, crs=thisCrs, newCrs=thisNewCrs,
            tf=thisTf, fillValue=thisFillValue)},
            error=function(e) e)
        return(list(ret))
    }
    return(retLst)
}

genVector4One <- function(faceX, faceY, ucmag, ucx, ucy, baseLength, aRatio=0.2, aOpen=30, aSide=2,
                          crs=NA, newCrs=NA, tf=FALSE, fillValue=NULL) {

    vecBeginPts <- data.table::data.table(X=faceX, Y=faceY, L=ucmag * baseLength, duX=ucx, duY=ucy)
    vecBeginPts[, A  := mapply(calcDirection, duX, duY,
                               MoreArgs=list(fillValue=fillValue), USE.NAMES = FALSE)]
    vecBeginPts <- vecBeginPts[A != fillValue]
    vecEndPts <- data.table::copy(vecBeginPts)
    vecBeginPts[, lid := paste0("v", .I)]
    vecEndPts[, X := X + L * cos(A * pi / 180)]
    vecEndPts[, Y := Y + L * sin(A * pi / 180)]
    arrPts <- data.table::copy(vecEndPts)
    vecEndPts[, lid := paste0("v", .I)]
    vecPts <- rbind(vecBeginPts[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
    arrPts[, A := A + 180 - aOpen]
    arrPts[, lid := paste0("a", .I)]
    arrPts[, X := X + L * aRatio * cos(A * pi / 180)]
    arrPts[, Y := Y + L * aRatio * sin(A * pi / 180)]
    vecEndPts[, lid := paste0("a", .I)]
    arrPts <- rbind(arrPts[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
    vec <- rbind(arrPts, vecPts)
    if (aSide > 1) {
        arrPts2 <- data.table::copy(vecEndPts)
        arrPts2[, A := A + 180 + aOpen]
        arrPts2[, lid := paste0("b", .I)]
        arrPts2[, X := X + L * aRatio * cos(A * pi / 180)]
        arrPts2[, Y := Y + L * aRatio * sin(A * pi / 180)]
        vecEndPts[, lid := paste0("b", .I)]
        arrPts2 <- rbind(arrPts2[, c("X", "Y", "lid")], vecEndPts[, c("X", "Y", "lid")])
        vec <- rbind(arrPts2, vec)
    }
    data.table::setorder(vec, lid)
    vec <- vec[!is.na(X)]
    ret <- NULL
    if (nrow(vec) > 0) {
        ret <- sfheaders::sf_linestring(vec, x="X", y="Y", linestring_id="lid")
        if (!is.na(crs)) {
            sf::st_crs(ret) <- crs
            if (tf)
                ret <- sf::st_transform(ret, newCrs)
        }
    }
    return(ret)
}

calcDirection <- function(dx, dy, fillValue=-999.0) {

    chk <- isTRUE(dx == fillValue) | isTRUE(dy == fillValue) | !chkDbl(dx) | !chkDbl(dy)
    if (chk)
        return(fillValue)
    d <- sqrt(dx ^ 2 + dy ^ 2)
    if (isTRUE(d == 0))
        return(fillValue)
    ret <- acos(dx / d)
    if (isTRUE(dy < 0))
        ret <- 2 * pi - ret
    ret <- 180 * ret / pi
    ret
}

#' Generate value rasters of one variable for all time steps
#'
#' @param mesh List of Ugrid objects
#' @param variable NetCDF variable to get values
#' @param lyr layer index.
#' @param folder Output folder
#' @param resolution Raster resolution
#' @param dryAsNa If TRUE, the dry area of "sea_surface_height" will be assigned to NaN
#' @export
genRaster4All <- function(mesh, variable="sea_surface_height", lyr=1L,
                          folder=tempdir(), resolution=NULL, dryAsNa=TRUE) {

    pol <- lapply(mesh, function(x) data.table::as.data.table(x$buildFace2DPoly())) |>
        data.table::rbindlist() |> sf::st_as_sf()
    pol$faceID <- NULL
    chk <- all(sapply(resolution, chkDbl)) & length(resolution) > 0
    if (!chk) {
        resolution <- sf::st_area(mesh[[1]]$m2D$face2D[sample.int(n=nrow(mesh[[1]]$m2D$face2D), size=1), ]) |>
            sqrt() |> round(8)
    }
    dta <- lapply(mesh, function(x) x$getData4Face2D(variable=variable, lyr=lyr))
    dta <- do.call(rbind, dta)
    if (grepl("sea_surface_height", variable) & dryAsNa) {
        altitudeVar <- mesh[[1]]$getVarName("altitude", topo="m2D", at="face")
        if (chkChr(altitudeVar)) {
            bl <- lapply(mesh, function(x) x$getData4Face2D(variable=x$m2D$face$altitude, lyr=lyr))
            bl <- do.call(rbind, bl) |> as.vector()
            dry <- abs(dta - bl) < 1e-9
            dta[dry] <- NaN
        } else {
            warning("Altitude variable for faces was not found. Dry areas were not assigned as NaN.")
        }
    }
    ncPath <- lapply(mesh, function(x) x$path) |> unlist() |> sort() |> paste(collapse = ";")
    fpre <- digest::digest(c(sort(ncPath), variable, lyr))
    pol2 <- terra::vect(pol)
    tempRast <- terra::rast(pol2, resolution=resolution, crs=terra::crs(pol2))
    nCores <- parallel::detectCores() %/% 2
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    rastFiles <- foreach::foreach(i=1:mesh[[1]]$totalTs, .combine=c) %dopar% {
        pol2[[variable]] <- dta[, i]
        ras <- terra::rasterize(x=pol2, y=tempRast, field=variable, fun="mean")
        fname <- file.path(folder, paste0(fpre, "_", 10000 + i, ".tif"))
        terra::writeRaster(x=ras, filename=fname, overwrite=TRUE)
        return(fname)
    }
    ret <- unlist(rastFiles)
    return(ret)
}




