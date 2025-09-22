#' Get data for faces
#'
#' @param mesh a Ugrid object or a list of those.
#' @param variable Variable name for the data
#' @param tsIdx Time index
#' @param agg Option for aggregating the data by rows. The aggregation functions come from `matrixStats` package.
#' @param force If TRUE, the data stored in Ugrid object, if any, will be read again.
#' @param onlyMain If TRUE, the cell elements of other domains will be removed.
#' @returns A sf object.
getMapData <- function(mesh, variable, tsIdx=0L,
                       agg=c("none", "max", "min", "mean"),
                       force=FALSE, onlyMain=FALSE) {

    tsIdx <- as.integer(tsIdx)
    agg <- match.arg(agg)
    aggFun <- switch(agg,
                     "max" = function(x) {
                         ret <- matrixStats::rowMaxs(x=x, na.rm=TRUE)
                         ret[is.infinite(ret)] <- NaN
                         ret
                         },
                     "min" = function(x) {
                         ret <- matrixStats::rowMins(x=x, na.rm=TRUE)
                         ret[is.infinite(ret)] <- NaN
                         ret
                     } ,
                     "mean" = function(x) matrixStats::rowMeans2(x=x, na.rm=TRUE)
                     )
    if (!is(mesh, "list"))
        mesh <- list(mesh)
    polLst <- lapply(mesh, function(aM) {
        aP <- aM$buildFace2DPoly()
        if (!is(aP, "sf")) {
            warning("Couldn't generate face-polygons for the file: ", aM$path)
            return(NULL)
        }
        aD <- aM$getData4Face2D(variable=variable, force=force)
        if (variable == "sea_surface_height") {
            altitudeVar <- aM$getVarName("altitude", topo="m2D", at="face")
            if (chkChr(altitudeVar)) {
                altitude <- aM$getData4Face2D(variable="altitude", force=force)
                altitude <- as.vector(altitude)
                dry <-  (aD - altitude) < 1e-9
                aD[dry] <- NaN
            } else {
                shiny::showNotification("Cannot find altitude data for faces! The displayed data is surface height,
                                        not only waterlevel!")
            }
        }
        ncVar <- aM$getVarName(variable, topo="m2D", at="face")
        isTime <- aM$vars[name == ncVar, isTRUE(hasTime)]
        if (onlyMain) {
            domains <- aM$getData4Face2D("cell_domain_number")
            if (!chkDbl(aM$md))
                aM$readDomainInfo()
            ids <- which(domains == aM$md)
            aD <- if(isTime) aD[ids, ] else aD[ids]
            aP <- aP[ids, ]
        }
        if (isTime) {
            tsChk <- isTRUE(tsIdx %between% c(1, aM$totalTs))
            if (tsChk) {
                aD <- aD[, tsIdx]
            } else if (is.function(aggFun)) {
                aD <- aggFun(aD)
            }
        }
        digits <- if (grepl("velocity|speed", variable)) 4 else 2
        aD <- round(aD, digits=digits)
        tmp <- tryCatch(
            {aP[[variable]] <- aD},
            error=function(e) message("Couldn't assign result for: ", aM$path, ".\n Reason: ", e))
        if (is.null(tmp))
            return(NULL)
        aP[["ncName"]] <- basename(aM$name)
        return(aP)
    })
    pol <- NULL
    for (i in seq_along(polLst)) {
        if (length(polLst[[i]]) < 1)
            next
        if (i < 2) {
            pol <- polLst[[i]]
        } else {
            if (is(polLst[[i]], "sf"))
                pol <- rbind(pol, polLst[[i]])
        }
    }
    return(pol)
}


#' Get data for faces that are intersected with or crossed by a sf object
#'
#' @param x an sf object with only one feature
#' @inheritParams getMapData
#' @returns sf object of polygon (faces) with values of the variable and names of the source nc.
#' @export
getData4Sf <- function(
        x, mesh, variable, tsIdx=NULL,
        agg=c("none", "max", "min", "mean"),
        force=FALSE, onlyMain=TRUE
) {

    pol <- getMapData(mesh=mesh, variable=variable,
                      tsIdx=tsIdx, agg=agg, force=force, onlyMain=onlyMain)
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


#' Generate isolines from a sf object.
#'
#' @param x sf object that can form a regular grid.
#' @param field Field for values.
#' @param resolution Numeric vector of length 1 or 2 to set the spatial resolution.
#' @param nbin Number of bins
#' @param binWidth Bin-width, if given, nbin will be ignored.
#' @param asSf If TRUE, the generated raster will be converted to sf.
#' @return a SpatVector or sf LINESTRING vector object.
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


#' Generate a Maplibre GL Map
#'
#' @param pol A sf object.
#' @param field The name of the column to use for classification.
#' @param n Number of classes
#' @param style Style of classification. One of: "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param valClass An object of the class `classIntervals` to set the classification manually
#' @param colPal Full name of a color palette.
#' @param colType Type of palette: "cat", "seq", "div" or "cyc".
#' @param mapId ID of the map.
#' @param continuous TRUE for continuous and FALSE for categorical legend.
#' @param reverse Should the palette be reversed?
#' @param legendTitle Legend title.
#' @param legendWidth Width of the legend.
#' @param addControls Add some controls to the map?
#' @param addLine Add polygon line?
#' @param lineColor Line color of polygons.
#' @param lineWidth Line width of polygons.
#' @param rsf a list of rescaling functions returned from `rescale` function.
#' @returns An HTML widget for a Mapbox map.
#' @export
genMap <- function(
        pol, field, values=NA, n=5, style="kmeans", valClass=NULL,
        colPal="seaborn.bright", colType="cat", mapId="map1", continuous=TRUE, reverse=FALSE,
        legendTitle=field, legendWidth="300px", legendPos="top-left",
        addControls=FALSE, addLine=FALSE,
        lineColor="grey", lineWidth=0.1,
        class=FALSE, rsf=NULL) {

    if (is.na(sf::st_crs(pol))) {
        pol <- squash2Bbox(pol, rsf=rsf)
        message("pol have no CRS! Geometry will be squashed to WGS84")
    }
    values <- unique(values)
    values <- values[!is.na(values)]
    if (length(values) < 1) {
        values <- pol[[field]] |> unique()
        values <- values[!is.na(values)]
    }
    if (inherits(valClass, "classIntervals")) {
        varClassInt <- valClass
    } else {
        varClassInt <- tryCatch(classInt::classIntervals(var=values, n=n, style=style),
                                error=function(e) NULL)
    }
    if (inherits(varClassInt, "classIntervals")) {
        varColors <- cols4all::c4a(palette=colPal, n=length(varClassInt$brks),
                                   type=colType, nm_invalid="interpolate", reverse=reverse)
        legendValue <- round(varClassInt$brks, 2)
        colExp <- mapgl::interpolate(
            column=field,
            values=varClassInt$brks,
            stops=varColors,
            na_color="grey"
        )
    } else {
        legendValue <- values
        colExp <- "grey"
        varColors <- "grey"
    }
    ret <- mapgl::maplibre(bounds=pol) |>
        mapgl::add_fill_layer(
            id=mapId, source=pol,
            fill_color=colExp, fill_opacity=1,
            tooltip=field,
            hover_options=list(fill_color="yellow", fill_opacity=1)
        )
    if (addLine) {
        ret <- mapgl::add_line_layer(
            ret, id=paste0(mapId, "_line"), source=pol,
            line_color=lineColor, line_width=lineWidth
        )
    }
    legendType <- ifelse(continuous, "continuous", "categorical")
    ret <- mapgl::add_legend(ret,
            legend_title=legendTitle, position=legendPos,
            values=legendValue, colors=varColors,
            width=legendWidth, type=legendType
        )
    if (addControls){
        ret <- mapgl::add_scale_control(ret) |>
            mapgl::add_draw_control(
                position="bottom-right",
                point_color="#002B54",
                line_color="#002B54",
                fill_color="#7BBA40",
                active_color="#8E4454",
                download_button=TRUE
                ) |>
            mapgl::add_geocoder_control() |>
            mapgl::add_navigation_control(show_zoom=FALSE)
    }

    return(ret)
}

#' Generate a Maplibre GL Map
#'
#' @param pol A sf object.
#' @param field The name of the column to use for classification.
#' @param n Number of classes
#' @param style Style of classification. One of: "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param valClass An object of the class `classIntervals` to set the classification manually
#' @param colPal Full name of a color palette.
#' @param colType Type of palette: "cat", "seq", "div" or "cyc".
#' @param mapId ID of the map.
#' @param reverse Should the palette be reversed?
#' @param legendTitle Legend title.
#' @param legendWidth Width of the legend.
#' @param addControls Add some controls to the map?
#' @param addLine Add polygon line?
#' @param lineColor Line color of polygons.
#' @param lineWidth Line width of polygons.
#' @param rsf a list of rescaling functions returned from `rescale` function.
#' @returns An HTML widget for a Mapbox map.
#' @export
genRasterMap <- function(
        pol, field, values=NA, n=5, style="kmeans", valClass=NULL,
        colPal="seaborn.bright", colType="cat", mapId="map1", reverse=FALSE,
        legendTitle=field, legendWidth="300px", legendPos="top-left", resolution=NULL,
        addControls=FALSE, opacity=1.0, class=FALSE, rsf=NULL) {

    if (is.na(sf::st_crs(pol))) {
        pol <- squash2Bbox(pol, rsf=rsf)
        message("pol have no CRS! Geometry will be squashed to WGS84")
    }
    if (!rlang::is_bare_numeric(resolution)) {
        resolution <- sf::st_area(pol[sample.int(nrow(pol), 1), 1]) |> sqrt() |> pretty()
        resolution <- resolution[1]
        if (isTRUE(sf::st_is_longlat(pol))) {
            resolution <- pretty(resolution / (8*10^5)) # convert meter to degree
            resolution <- resolution[resolution > 0][1]
        }
    }
    values <- unique(values)
    values <- values[!is.na(values)]
    if (length(values) < 1) {
        values <- pol[[field]] |> unique()
        values <- values[!is.na(values)]
    }
    if (inherits(valClass, "classIntervals")) {
        varClassInt <- valClass
    } else {
        varClassInt <- tryCatch(classInt::classIntervals(var=values, n=n, style=style), error=function(e) NULL)
    }
    if (inherits(varClassInt, "classIntervals")) {
        varColors <- cols4all::c4a(palette=colPal, n=length(varClassInt$brks),
                                   type=colType, nm_invalid="interpolate", reverse=reverse)

        labels <- format(varClassInt$brks, digits=3, scientific=TRUE)
        pal <- leaflet::colorNumeric(varColors, domain=varClassInt$brks, na.color="#FF000000")
        addLegendMod <- function(map) leaflet::addLegend(map, labels=labels, colors=varColors,
                                                         bins=length(varClassInt$brks), opacity=opacity)
    } else {
        varColors <- cols4all::c4a(palette=colPal, n=n, type=colType, nm_invalid="interpolate", reverse=reverse)
        pal <- leaflet::colorNumeric(varColors, domain=values, na.color="#FF000000")
        addLegendMod <- function(map) leaflet::addLegend(map, pal=pal, values=values, bins=n, opacity=opacity)
    }
    x <- terra::vect(pol)
    x[[field]] <- pol[[field]]
    ras <- terra::rasterize(
        x=x,
        y=terra::rast(terra::ext(pol), resolution=resolution, crs=terra::crs(pol)),
        field=field, fun="mean")
    # terra::crs(ras) <- terra::crs(pol)
    ras <- terra::project(ras, "epsg:4326")

    ret <- leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addRasterImage(ras, colors=pal, opacity=opacity) |>
        addLegendMod() |>
        leaflet.extras::addDrawToolbar(
            markerOptions = FALSE,
            circleMarkerOptions = FALSE,
            circleOptions = FALSE,
            singleFeature = FALSE,
            editOptions = leaflet.extras::editToolbarOptions()
        )

    return(ret)
}

#' Generate functions for scaling coordinates from one bbox to another
#'
#' @param fromBb Origin bbox
#' @param toBb Destination bbox
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

#' Squash the geometry of an sf object into another bbox
#'
#' @param geom a sf object.
#' @param toBb destination bbox.
#' @param rsf a list of rescaling functions returned from `rescale` function.
squash2Bbox <- function(geom, toBb=c(-37, -44, -16, -30), rsf=NULL) {

    if (!all(is.function(rsf))) {
        bb <- sf::st_bbox(geom)
        rsf <- rescale(bb, toBb=toBb)
    }
    geomDf <- sfheaders::sf_to_df(geom) |>
        data.table::as.data.table()
    geomDf[, x := rsf$fx(x)]
    geomDf[, y := rsf$fy(y)]
    gType <- sf::st_geometry_type(geom, by_geometry = FALSE) |> as.character()
    if (grepl("POLYGON", gType)) {
        ret <- sfheaders::sf_polygon(
            geomDf, x="x", y="y",
            polygon_id = "polygon_id", linestring_id="linestring_id")
        ret$polygon_id <- NULL
    } else if (grepl("LINESTRING", gType)) {
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
#' @param x a Ugrid object
#' @param tsIdx Index on time step
#' @param onlyMain If TRUE, only elements of main domain will be taken.
#' @param baseLength Base length of the vector.
#' @param aRatio A ratio between vector head and vector length.
#' @param aOpen Open angle in degree of vector head.
#' @param aSide Vector head to both side or only one?
#' @param fillValue The fill value of the variable in the NetCDF.
genVectorLayer <- function(
        x, tsIdx=NULL, onlyMain=FALSE,
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
        fillValue <- x$atts[grepl("fillvalue", name, ignore.case = TRUE), val[1]] |> as.numeric()
    if (length(fillValue) < 1)
        fillValue <- -Inf
    if (length(tsIdx) < 1)
        tsIdx <- x$totalTs
    faceX <- x$getData4Face2D(x$m2D$face$face_x)
    faceY <- x$getData4Face2D(x$m2D$face$face_y)
    ucx <- x$getData4Face2D(x$m2D$face$sea_water_x_velocity)
    ucy <- x$getData4Face2D(x$m2D$face$sea_water_y_velocity)
    ucmag <-x$getData4Face2D(x$m2D$face$sea_water_speed)
    if (x$vars[name == x$m2D$face$sea_water_x_velocity, hasTime])
        ucx <- ucx[, tsIdx]
    if (x$vars[name == x$m2D$face$sea_water_y_velocity, hasTime])
        ucy <- ucy[, tsIdx]
    if (x$vars[name == x$m2D$face$sea_water_speed, hasTime])
        ucmag <- ucmag[, tsIdx]
    if (onlyMain) {
        x$readDomainInfo()
        fids <- x$getData4Face2D(variable=x$m2D$face$cell_domain_number)
        if (length(fids) > 0) {
            chk <- (fids == x$md)
            faceX <- faceX[chk]
            faceY <- faceY[chk]
            ucx <- ucx[chk]
            ucy <- ucy[chk]
            ucmag <- ucx[chk]
        }
    }
    # if baseLength == NA_real_ it will not work
    hasBl <- chkDbl(baseLength)
    if (!hasBl) {
        x$buildFace2DPoly()
        baseLength <- sf::st_area(x$m2D$face2D[sample.int(n=nrow(x$m2D$face2D), size=1), ]) |>
            sqrt() |> as.numeric()
    }
    vecBeginPts <- data.table::data.table(X=faceX, Y=faceY, L=ucmag * baseLength, duX=ucx, duY=ucy)
    vecBeginPts[, A  := mapply(getDirection, duX, duY,
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
    setorder(vec, lid)
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


getDirection <- function(dx, dy, fillValue=-999.0) {

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

#' Split features of a linestring to segments with exact two points.
#' @param x an sf_linestring
line2Segments <- function(x) {

    if (!is(x, "sf"))
        return(NULL)
    tbl <- sf::st_coordinates(x) |> data.table::as.data.table()
    hasZ <- "Z" %in% colnames(tbl)
    tbl[, grp := seq.int(1, .N), by=L1]
    tbl[, lid := paste(L1, grp, sep="_")]
    tbl[, X2 := shift(X, type="lead")][, Y2 := shift(Y, type="lead")]
    if (hasZ)
        tbl[, Z2 := shift(Z, type="lead")]
    col1 <- grep("2", colnames(tbl), value=TRUE, invert=TRUE) |> sort()
    col2 <- grep("grp|L1|lid|2", colnames(tbl), value=TRUE) |> sort()
    tbl2 <- tbl[-.N, .SD, .SDcols = col2]
    tbl1 <- tbl[-.N, .SD, .SDcols = col1]
    data.table::setnames(tbl2, col2, col1)
    tbl <- rbind(tbl1, tbl2)
    setorder(tbl, L1, grp)
    tbl[, grp := NULL]
    xdta <- sf::st_drop_geometry(x)
    xdta$L1 <- 1:nrow(x)
    tbl <- merge(tbl, xdta, by="L1")
    if (hasZ)
        ret <- sfheaders::sf_linestring(tbl, x="X", y="Y", z="Z", linestring_id="lid", keep=TRUE)
    else
        ret <- sfheaders::sf_linestring(tbl, x="X", y="Y", linestring_id="lid", keep=TRUE)
    sf::st_crs(ret) <- sf::st_crs(x)

    return(ret)
}

# calcDischarge <- function(cr, mnc,
#                           vxVar="ucxq_velocity", vyVar="ucyq_velocity",
#                           wdVar="sea_floor_depth_below_sea_surface"){
#
#     cri <- sf::st_intersection(cr, mnc$m2D$face2D)
#     cri <- line2Segments(cri)
#     fids <- cri$faceID
#     ucx <- mnc$getData4Face2D(mnc$m2D$face[[vxVar]])
#     ucy <- mnc$getData4Face2D(mnc$m2D$face[[vyVar]])
#     if (chkChr(mnc$m2D$face[[wdVar]]) ) {
#         wd <- mnc$getData4Face2D(mnc$m2D$face[[wdVar]])
#     } else {
#         wt <- mnc$getData4Face2D(mnc$m2D$face$sea_surface_height)
#         bl <- mnc$getData4Face2D(mnc$m2D$face$altitude)
#         if (mnc$vars[name == mnc$m2D$face$altitude, !hasTime])
#             bl <- as.vector(bl)
#         wd <- wt - bl
#     }
#     fdta <- data.table::data.table(
#         faceID=fids, vx=as.vector(ucx[fids, ]), vy=as.vector(ucy[fids, ]),
#         depth=as.vector(wd[fids, ]))
#     segLen <- sf::st_length(cri) |> as.vector()
#     segXY <- sf::st_coordinates(cri) |> data.table::as.data.table()
#     beginPts <- segXY[seq.int(1, nrow(segXY), 2), ]
#     colnames(beginPts)[1:2] <- c("x1", "y1")
#     endPts <- segXY[seq.int(2, nrow(segXY), 2), ]
#     colnames(endPts)[1:2] <- c("x2", "y2")
#     endPts$L1 <- NULL
#     segXY <- cbind(beginPts, endPts)
#     segXY[, sx := x2 - x1][, sy := y2 - y1][, su := sqrt(sx^2 + sy^2)]
#     sdta <- segXY
#     for (i in 1:(mnc$totalTs - 1))
#         sdta <- rbind(sdta, segXY)
#     qtbl <- cbind(fdta, sdta)
#     # qtbl[, vp := vPerp(vx, vy, sx, sy), by=.I]
#     # qtbl[, crq := vp * depth * su]
#     # velocity magnitude
#     qtbl[, vu := sqrt(vx^2 + vy^2)]
#     qtbl[su < 1e-12, vq := 0]
#     qtbl[su > 1e-12, ny := sy / su][su > 1e-9, nx := -sx / su]
#     qtbl[su > 1e-12, vq := vx * ny + vy * nx]
#     qtbl[vu < 1e-12, crq := 0]
#     qtbl[vu > 1e-12, crq := vq * su * depth]
#     ret <- matrix(qtbl$crq, ncol=mnc$totalTs)
#     ret <- colSums(ret)
#     return(ret)
# }
#
# vPerp <- function(vx, vy, sx, sy) {
#
#     v <- c(vx, vy)
#     s <- c(sx, sy)
#     # dot product, sigma(vi.si)
#     dotp <- vx * sx + vy * sy
#     su <- sqrt(sx^2 + sy^2)
#     if (su < 1e-12)
#         return(0)
#     v_parallel <- dotp / su
#     v_perp <- v - v_parallel
#     vpu <- sqrt(sum(v_perp * v_perp))
#     if (vpu < 1e-12)
#         return(0)
#     # Compute signed angle using atan2 (radians)
#     crossp <- vx*sy - vy*sx
#     theta <- atan2(crossp, dotp)        # radians, range (-pi, pi]
#     if (theta < 0)
#         theta <- theta + 2*pi    # normalize to [0, 2*pi)
#
#     # Determine direction factor
#     if (theta < 1e-12 || abs(theta - pi) < 1e-12 || abs(theta - 2*pi) < 1e-12) {
#         d <- 0
#     } else if (theta > 0 && theta < pi) {
#         d <- -1
#     } else {  # pi < theta < 2*pi
#         d <- 1
#     }
#     ret <- vpu * d
#
#     return(ret)
# }

#' Read .pli data to sf_linestring
#'
#' This function can read file in Deltares-polyline format with or without Z values
#' and convert it to a sf LINESTRING.
#'
#' @param path Path to the .pli file (only x, y)
#' @param crs Coordinate system of the polyline
#' @export
readPli <- function(path, crs=sf::NA_crs_) {

    if (!file.exists(path)) {
        warnings("File not found: ", path)
        return(NULL)
    }
    tbl <- fread(file=path, sep="\n", header=FALSE, blank.lines.skip=TRUE)
    tbl <- tbl[!grepl("^\\*", V1)]
    if (nrow(tbl) < 4) {
        warnings("Not enough information in the file: ", path)
        return(NULL)
    }
    tbl[, V1 := stringi::stri_trim_both(V1)][, V1 := stringi::stri_replace_all_regex(V1, " +", " ")]
    tbl[, crInfo := shift(V1, n=-1)]
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
        tbl[(i + 2):(nextRow - 1), c("X", "Y", "Z") := tstrsplit(V1, split=" "), by=V1]
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
