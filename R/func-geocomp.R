#' Find faces that are crossed by a line or intersected by points.
#'
#' @param x an object of points or lines
#' @param mesh a Ugrid-class object
#' @export
findFaces <- function(x, mesh) {

    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(mesh, "Ugrid"))
    xtype <- sf::st_geometry_type(x)
    mesh$buildFace2DPoly()
    if (grepl("POINT", xtype)) {
        ret <- sf::st_intersects(x, mesh$m2D$face2D) |> unlist()
    } else if (grepl("LINE", xtype)) {
        ret <- sf::st_crosses(x, mesh$m2D$face2D) |> unlist()
    } else {
        ret <- NULL
    }

    return(ret)
}

calcFaceStations <- function(faces, mesh) {

    faces <- unlist(faces)
    stopifnot(length(faces) > 1 )
    pol <- mesh$buildFace2DPoly()
    pol <- pol[faces, ]
    faceCen <- sf::st_centroid(pol)
    ret <- sf::st_distance(faceCen[-nrow(faceCen), ], faceCen[-1, ], by_element = TRUE)
    ret <- c(0, ret)
    ret <- cumsum(ret)
    return(ret)
}

buildTemplateRaster <- function(faces, mesh, dx=500, dy=5) {
    faceSta <- calcFaceStations(faces=faces, mesh=mesh)
    depths <- mesh$getData4Any(variable="altitude", topo="m2D", at="interface")
    bbox <- c(xmin=min(faceSta), ymin=min(depths),
                     xmax=max(faceSta), ymax=max(depths))
    class(bbox) <- "bbox"
    ret <- stars::st_as_stars(bbox, dx=dx, dy=dy)
    return(ret)
}


#' @noRd
interpLayer <- function(faces, mesh, variable="mesh2d_sa1",
                        tsIdx=100, dx=500, dy=3){

    faceSta <- calcFaceStations(faces=faces, mesh=mesh)
    # depth of interfaces
    depths <- mesh$getData4Any("altitude", topo="m2D", "interface") |> as.vector()
    xRange <- seq(min(faceSta), max(faceSta), dx)
    yRange <- seq(min(depths), max(depths), dy)
    dta <- mesh$getData4Face2D(variable=variable)
    dta <- dta[, faces, tsIdx]
    dta2 <- as.data.table(t(dta))
    dta2[, x := faceSta / 1000]
    dta3 <- melt(dta2, id.vars="x", variable.name = "layer")
    # the number of layers is one less than the number of interfaces
    if (!variable %in% mesh$m2D$interface)
        depths <- depths[-1]
    dta3[, y := rep(depths, each=length(faces))]
    dta3 <- dta3[!is.na(value)]
    interpTbl <- akima::interp(x=dta3$x, y=dta3$y, z=dta3$value,
                               duplicate="mean", linear=FALSE)
    tbl <- expand.grid(x = interpTbl$x, y = interpTbl$y)
    tbl$z <- as.vector(interpTbl$z)
    tbl <- as.data.table(tbl)
    tbl[, z := nafill(z, type="nocb")]
    tbl[, z := nafill(z, type="locf")]

    return(tbl)
}

calcIsolineData <- function(depth, station, dta, tsName=NULL) {

    xyz <- expand.grid(y=depth, x=station / 1000)
    dims <- dim(dta)
    if (length(tsName) != dims[3])
        tsName <- seq_len(dims[3])
    nCores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    retLst <- foreach::foreach(i=1:dims[3], .combine=rbind) %dopar% {
        xyz$z <- as.vector(dta[, , i])
        if (length(xyz$z[!is.na(xyz$z)]) < 5)
            return(NULL)
        xyz$z  <- data.table::nafill(xyz$z, type="nocb")
        xyz$z  <- data.table::nafill(xyz$z,  type="locf")
        interpTbl <- akima::interp(x=xyz$x, y=xyz$y, z=xyz$z, duplicate="mean")
        tbl <- expand.grid(x = interpTbl$x, y = interpTbl$y)
        tbl$z <- as.vector(interpTbl$z)
        tbl$z  <- data.table::nafill(tbl$z, type="nocb")
        tbl$z  <- data.table::nafill(tbl$z,  type="locf")
        tbl$tsIdx <- tsName[i]
        tbl
    }
    tbl <- data.table::as.data.table(retLst)
    return(tbl)
}

genContourGif <- function(tbl, fps=12, fname=tempfile(fileext=".gif"), xRange=c(0, 65), xReverse=TRUE,
                          xName="Distance from Brünsbuttel [km]",
                          yName="Depth [m]",
                          nClass=7, style="kmeans", colPal="blue_teal",
                          fixedClass=NULL,
                          legendTitle="sea_water_salinity [1e-3]") {

    yRange <- tbl[, range(y, na.rm=TRUE)]
    if (!rlang::is_bare_numeric(xRange, 2))
        xRange <- tbl[, range(x, na.rm=TRUE)]
    ratio <- (dist(xRange) / dist(yRange) / 1.5) |> pretty(n=1) |> mean()
    ybreaks <- pretty(depth, n=10)
    ybreaks <- ybreaks[ybreaks %between% range(depth)]
    yaxis <- ggplot2::scale_y_continuous(limits=range(depth), breaks=ybreaks,
                                         expand=ggplot2::expansion(0.01))
    if (xReverse)
        xaxis <- ggplot2::scale_x_reverse(limits=sort(xRange, TRUE), n.breaks=10)
    else
        xaxis <- ggplot2::scale_x_continuous(limits=sort(xRange, TRUE), n.breaks=10)
    values <- tbl[x %between% xRange, unique(z)]
    if (!is.null(fixedClass))
        brks <- txt2NumVec(fixedClass)
    else {
        valClass <- tryCatch(classInt::classIntervals(var=values, n=nClass, style=style),
                             error=function(e) message(e))
        brks <- valClass$brks
    }
    g <- ggplot2::ggplot(tbl, ggplot2::aes(x = x, y = y, z=z)) +
        ggplot2::geom_contour_filled(breaks=valClass$brks) +
        yaxis +
        xaxis +
        ggplot2::coord_equal() +
        cols4all::scale_fill_discrete_c4a_cat(palette = colPal) +
        ggplot2::labs(x=xName, y=yName,
                      fill=legendTitle, subtitle="Timestep {frame_time}") +
        gganimate::transition_time(tsIdx) +
        ggplot2::theme_bw(base_size = 11, base_family = "Arial")
    ga <- gganimate::animate(g, fps=fps, width=1200, height=1200 / ratio,
                       nframes=length(unique(tbl$tsIdx)))
    gganimate::anim_save(filename=fname, animation=ga)
}

genContourFacets <- function(tbl, tsIds, xRange=c(0, 65), xReverse=TRUE,
                          xName="Distance from Brünsbuttel [km]",
                          yName="Depth [m]",
                          nClass=7, style="kmeans", colPal="blue_teal",
                          fixedClass=NULL,
                          legendTitle="sea_water_salinity [1e-3]") {

    yRange <- tbl[, range(y, na.rm=TRUE)]
    if (!rlang::is_bare_numeric(xRange, 2))
        xRange <- tbl[, range(x, na.rm=TRUE)]
    ratio <- (dist(xRange) / dist(yRange) / 1.5) |> pretty(n=1) |> mean()
    ybreaks <- pretty(yRange, n=10)
    ybreaks <- ybreaks[ybreaks %between% yRange]
    yaxis <- ggplot2::scale_y_continuous(limits=yRange, breaks=ybreaks,
                                         expand=ggplot2::expansion(0.01))
    if (xReverse)
        xaxis <- ggplot2::scale_x_reverse(limits=sort(xRange, TRUE), n.breaks=10)
    else
        xaxis <- ggplot2::scale_x_continuous(limits=sort(xRange, TRUE), n.breaks=10)
    values <- tbl[x %between% xRange, unique(z)]
    if (!is.null(fixedClass))
        brks <- txt2NumVec(fixedClass)
    else {
        valClass <- tryCatch(classInt::classIntervals(var=values, n=nClass, style=style),
                             error=function(e) message(e))
        brks <- valClass$brks
    }
    g <- ggplot2::ggplot(tbl[tsIdx %in% tsIds,], ggplot2::aes(x = x, y = y, z=z)) +
        ggplot2::geom_contour_filled(breaks=brks) +
        yaxis +
        xaxis +
        ggplot2::coord_equal() +
        cols4all::scale_fill_discrete_c4a_cat(palette = colPal, breaks=brks) +
        ggplot2::labs(x=xName, y=yName, fill=legendTitle) +
        ggplot2::facet_wrap(ggplot2::vars(tsIdx), ncol=1) +
        ggplot2::theme_bw(base_size=14, base_family="Arial")
    return(g)
}


calcStation <- function(line, pol){

    lineSec <- sf::st_intersection(line, pol)
    secMidpts <- sf::st_centroid(lineSec)
    secLength <- sf::st_length(lineSec)
    secSta <- (secLength[-length(secLength)] + secLength[-1]) / 2
    secSta <- c(0, secSta)
    secSta <- cumsum(secSta)

    return(secSta)
}
