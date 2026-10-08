#' Find faces that are crossed by a line or intersected by points.
#'
#' @param x an object of points or lines
#' @param mesh a Ugrid-class object
#' @export
findFaces <- function(x, mesh) {

    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(mesh, "Ugrid"))
    xtype <- sf::st_geometry_type(x)
    pol <- mesh$buildFace2DPoly() |>
        sf::st_make_valid()
    if (grepl("POINT", xtype)) {
        ret <- sf::st_intersects(x, pol) |> unlist()
    } else if (grepl("LINE", xtype)) {
        ret <- sf::st_crosses(x, pol) |> unlist()
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

calcIsolineData <- function(dta, tsName=NULL) {

    tsVec <- unique(dta$ts)
    nTs <- length(unique(dta$ts))
    if (length(tsName) != nTs)
        tsName <- tsVec
    data.table::setorder(dta, ts, lyr, sta)
    nCores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    retLst <- foreach::foreach(i=1:nTs, .combine=rbind) %dopar% {
        xyz <- dta[ts == tsVec[i] & !is.na(value), list(x = sta / 1000, y = depth, z = value)]
        if (length(xyz$z[!is.na(xyz$z)]) < 5)
            return(NULL)
        interpTbl <- akima::interp(x=xyz$x, y=xyz$y, z=xyz$z, duplicate="mean")
        tbl <- expand.grid(x = interpTbl$x, y = interpTbl$y)
        tbl$z <- as.vector(interpTbl$z)
        tbl <- data.table::as.data.table(tbl)
        tbl$tsIdx <- tsName[i]
        tbl
    }
    tbl <- data.table::as.data.table(retLst)
    return(tbl)
}

genContourGif <- function(tbl, fps=12, fname=tempfile(fileext=".gif"), xRange=c(0, 65), xReverse=TRUE,
                          xName="Distance from Bruensbuttel [km]",
                          yName="Depth [m]",
                          nClass=7, style="kmeans", colPal="blue_teal",
                          fixedClass=NULL,
                          legendTitle="sea_water_salinity [1e-3]") {
    if (identical(fixedClass, ""))
        fixedClass <- NULL
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
    colors <- cols4all::c4a(palette=colPal, n=length(brks), type="seq", nm_invalid="interpolate")
    g <- ggplot2::ggplot(tbl, ggplot2::aes(x = x, y = y, z=z)) +
        ggplot2::geom_contour_filled(breaks=brks) +
        yaxis +
        xaxis +
        ggplot2::coord_equal() +
        ggplot2::scale_fill_manual(values=colors,  aesthetics = 'fill', drop=FALSE) +
        ggplot2::labs(x=xName, y=yName,
                      fill=legendTitle, subtitle="Timestep {frame_time}") +
        gganimate::transition_time(tsIdx) +
        ggplot2::theme_bw(base_size = 11, base_family = "Arial")
    ga <- gganimate::animate(g, fps=fps, width=1200, height=1200 / ratio,
                       nframes=length(unique(tbl$tsIdx)))
    gganimate::anim_save(filename=fname, animation=ga)
}

genContourFacets <- function(tbl, tsIds, xRange=c(0, 65), xReverse=FALSE,
                          xName="Distance from Bruensbuttel [km]",
                          yName="Depth [m]",
                          nClass=7, style="kmeans", colPal="blue_teal",
                          fixedClass=NULL,
                          legendTitle="sea_water_salinity [1e-3]") {

    if (identical(fixedClass, ""))
        fixedClass <- NULL
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
    if (chkChr(fixedClass)) {
        brks <- txt2NumVec(fixedClass)
    }
    else {
        valClass <- tryCatch(classInt::classIntervals(var=values, n=nClass, style=style),
                             error=function(e) message(e))
        brks <- valClass$brks
    }
    colors <- cols4all::c4a(palette=colPal, n=length(brks), type="seq", nm_invalid="interpolate")
    g <- ggplot2::ggplot(tbl[tsIdx %in% tsIds,], ggplot2::aes(x = x, y = y, z=z)) +
        ggplot2::geom_contour_filled(show.legend = TRUE, breaks=brks) +
        yaxis +
        xaxis +
        ggplot2::coord_equal() +
        ggplot2::scale_fill_manual(values=colors,  aesthetics = 'fill', drop=FALSE) +
        ggplot2::labs(x=xName, y=yName, fill=legendTitle) +
        ggplot2::facet_wrap(ggplot2::vars(tsIdx), ncol=1) +
        ggplot2::theme_bw(base_size=14, base_family="Arial")

    return(g)
}

calcStation <- function(lineSec){

    secMidpts <- sf::st_centroid(lineSec)
    secLength <- sf::st_length(lineSec)
    secSta <- (secLength[-length(secLength)] + secLength[-1]) / 2
    secSta <- c(0, secSta)
    secSta <- cumsum(secSta)

    return(secSta)
}
