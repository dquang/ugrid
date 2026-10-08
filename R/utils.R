#' @keywords internal
fmtDe <- function(rdg, nsmall) {
    function(x) {
        format(round(x, rdg), decimal.mark=",", big.mark=".", nsmall=nsmall)
    }
}


#' Convert character vector to POSIXct
#'
#' The input can have different formats, including german date-time format
#' @param x character vector
#' @param tz Time zone
#' @param origin Origin of the time series.
#' @keywords internal
asPOSIXctManyFormats <- function(
    x, origin=NULL,
    tz=getOption("ugrid.timeZone"), ...) {

    if (length(origin) < 1)
        origin <- as.POSIXct("1970-01-01 00:00:00", tz=tz)
    x <- as.POSIXct(
        x,
        tz=tz,
        tryFormats=c(
            "%Y-%m-%d %H:%M:%S",
            "%Y/%m/%d %H:%M:%S",
            "%Y-%m-%d %H:%M",
            "%Y/%m/%d %H:%M",
            "%Y-%m-%d",
            "%Y/%m/%d",

            "%d.%m.%Y %H:%M:%S",
            "%d.%m.%Y %H:%M",
            "%d.%m.%Y",

            "%Y-%m-%dT%H:%M:%SZ"
        ),
        origin=origin,
        ...
    )
    return(x)
}


#' Plot a graphic with horizontal lines using colors given by val
#'
#' @param val Named vector colors
#' @export
displayColors <- function(val) {
    valID <- seq_along(val)
    if (all(is.null(names(val))))
        names(val) <- val
    dta <- data.frame(y=valID, x=rep(1, length(valID)), value=names(val))
    g <- ggplot2::ggplot(dta, ggplot2::aes(x=x, y=y, color=value)) +
        ggplot2::geom_segment(size=3, mapping=ggplot2::aes(xend=x + 1, yend=y)) +
        ggplot2::scale_color_manual(values=val) +
        ggplot2::scale_y_reverse(
            breaks=valID,
            labels=mapply(paste0, val, " (", valID, ")"),
            sec.axis=ggplot2::dup_axis(labels=names(val))
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position="none",
            panel.border=ggplot2::element_blank(),
            panel.grid=ggplot2::element_blank(),
            axis.text.x=ggplot2::element_blank(),
            axis.title=ggplot2::element_blank()
        )
    return(g)
}


#' Create axis list for plotly layout
#'
#' @param y1Range,y2Range Ranges of y1, y2 values.
#' @param y1Name,y2Name Names of y1, y2 axes.
#' @param nTick Number of ticks.
#' @param fontSize Font size.
#' @param fontFamily Font family.
#' @returns List of axis-parameters to pass to `plotly::layout` function
#' @export
createAxisLayout <- function(y1Range, y2Range=NULL,
                             y1Name="W", y2Name="Q",
                             nTick=6, fontSize=14, fontFamily="Arial") {

    y1Range <- range(y1Range, na.rm=T)
    y2Range <- range(y2Range, na.rm=T)
    y1Chk <- !any(is.infinite(y1Range))
    y2Chk <- !any(is.infinite(y2Range))
    if (!y1Chk)
        ay1 <- list()
    else
        ay1 <- list(title=y1Name)
    if (!y2Chk)
        ay2 <- list()
    else
        ay2 <- list(title=y2Name)
    if (y1Chk & y2Chk) {
        y1Length <- y1Range[2] - y1Range[1]
        y1Pretty <- pretty(y1Range, nTick - 1, nTick -1)
        nTick <- length(y1Pretty)
        y1Dtick <- y1Pretty[2] - y1Pretty[1]
        ay2Lst <- createY2(y1=y1Range, y2=y2Range, rel=1, n=nTick)
        ay1 <- list(
            range=ay2Lst$y1Range,
            dtick=ay2Lst$y1Dtick,
            tick0=ay2Lst$y1Breaks[1],
            tickmode="linear",
            tickfont=list(size=fontSize, family=fontFamily),
            title=list(text=y1Name, font=list(size=fontSize, family=fontFamily))
        )
        ay2 <- list(
            title=list(text=y2Name, font=list(size=fontSize, family=fontFamily)),
            showticklabels=TRUE,
            overlaying="y",
            tickfont=list(size=fontSize, family=fontFamily),
            range=ay2Lst$y2Range,
            dtick=ay2Lst$y2Dtick,
            tick0=ay2Lst$y2Breaks[1],
            tickmode="linear",
            side="right"
        )
    }
    return(list(ay1=ay1, ay2=ay2))
}


#' generate nice scales for two parallel axis
#'
#' @param y1 value vector or range of first axis
#' @param y2 value vector or range of second axis
#' @param rel should y2 always lower than y1?
#' @export
createY2 <- function(y1, y2, n=5, rel = 1) {

    y1Range <- range(y1, na.rm=TRUE)
    y2Range <- range(y2, na.rm=TRUE)
    y1Length <- y1Range[2] - y1Range[1]
    y2Length <- y2Range[2] - y2Range[1]
    y1Breaks <- pretty(y1Range, n=n, min.n=n)
    y2Breaks <- pretty(y2Range, n=n, min.n=n)
    n <- length(y1Breaks) - 1
    y1Dtick <- y1Breaks[2] - y1Breaks[1]
    y2Dtick <- pretty((y2Breaks[2] - y2Breaks[1]) * rel)
    y2Dtick <- max(y2Dtick)
    y2Scale <- y1Dtick / y2Dtick
    y2Shift <- y2Breaks[1] - y1Breaks[1] / y2Scale
    y2Breaks <- y1Breaks / y2Scale + y2Shift
    bndChk <- max(y2Breaks) < max(y2Range)
    if (bndChk) {
        nTick <- ceiling(y2Length / y2Dtick) + 2
        y1Breaks <- seq(from = y1Breaks[1], length.out = nTick, by = y1Dtick)
        y2Breaks <- y1Breaks / y2Scale + y2Shift
    }
    ret <- list(y2Scale = y2Scale, y2Shift = y2Shift,
                y1Dtick = y1Dtick, y2Dtick = y2Dtick,
                y1Breaks = y1Breaks, y2Breaks = y2Breaks,
                y1Range = range(y1Breaks), y2Range = range(y2Breaks))

    return(ret)
}

#' @keywords internal
blankPlotly <- function(e="Error while creating plot") {

    p <- plotly::plot_ly() |>
        plotly::add_text(x=0.5, y=0.5, text=e, hoverinfo='none')  |>
        plotly::layout(
            xaxis=list(color="white", fixedrange=TRUE),
            yaxis=list(color="white", fixedrange=TRUE)
        ) |>
        plotly::config(displayModeBar = FALSE)

    return(p)
}

#' @keywords internal
blankGgplot <- function(e="Error while creating plot") {

    g <- ggplot2::ggplot(
        data=data.frame(x=1, y=1),
        mapping=aes(x=x, y=y)
    ) +
        ggplot2::annotate("text", x=0.5, y=0.5, label=e) +
        ggplot2::theme_void(base_size=24)

    return(g)
}

#' @keywords internal
txt2NumVec <- function(x) {

    ret <- stringi::stri_replace_all_fixed(x, ",", ".") |>
        stringi::stri_split_fixed(" ") |> unlist() |>
        as.numeric()
    ret <- ret[!is.na(ret)]
    if (length(ret) < 1)
        ret <- NULL
    return(ret)
}


#' Look up river mileage for a table of points
#'
#' @param tbl Table of x, y coordinates
#' @param crsid CRS-ID
#' @param radius lookup radius
#' @param riverSearchUrl,stationSearchUrl URLs to lookup service of WSV
#' @export
xy2station <- function(
        tbl, crsid=25832,
        stationSearchUrl="https://via.bund.de/wsv/bwastr-locator/rest/stationierung/query"
) {

    colnames(tbl) <- tolower(colnames(tbl))
    tbl <- data.table::as.data.table(tbl[, c("x", "y")])
    tbl[, qid := .I][, wkid := crsid]
    tbl[, qr := sprintf(
        '{"qid": "%s",  "geometry": {"type": "Point",
		"coordinates": [%.5f, %.5f], "spatialReference": {"wkid": %s}}}',
        qid, x, y, wkid)]
    qrJson <- paste0('{"queries": [', paste(tbl$qr, collapse=","), ']}')
    qr <- httr::POST(url=stationSearchUrl, body=qrJson, httr::content_type_json())
    ct <- httr::content(qr)
    if (!hasName(ct, "result"))
        return(data.table::data.table())
    ret <- lapply(ct$result, function(x) {
        data.table::data.table(
            idx = as.integer(x$qid),
            KM=x$stationierung$km_wert,
            offset=x$stationierung$offset,
            strecken=x$strecken_name,
            river=x$bwastr_name,
            bwastrid=x$bwastrid
        )
    }) |> data.table::rbindlist(fill=TRUE)

    return(ret)
}

checkFuture <- function(feat) {

    if (!future::resolved(feat))
        return(FALSE)
    res <- future::result(feat)
    if (inherits(res$value, "error"))
        return(FALSE)
    chk <- any(sapply(res$conditions, function(x) inherits(x$condition, "error")))
    if (chk)
        return(FALSE)
    return(TRUE)
}

# check if x is a single double and not NA
chkDbl <- function(x) {
    rlang::is_bare_numeric(x, n=1) & !rlang::is_na(x)
}

# check if x is a single integer and not NA
chkInt <- function(x) {
    rlang::is_bare_integer(x, n=1) & !rlang::is_na(x)
}

# check if x is a single integer and not NA
chkChr <- function(x) {
    isTRUE(nchar(x) > 0)
}

bb2Pol <- function(bb, id="bb1", crs=sf::st_crs()) {

    bb <- as.list(bb)
    pol <- data.frame(
        x=c(bb[["xmin"]], bb[["xmax"]], bb[["xmax"]], bb[["xmin"]]),
        y=c(bb[["ymin"]], bb[["ymin"]], bb[["ymax"]], bb[["ymax"]]),
        pid=rep(id, 4)
    ) |>
        sfheaders::sf_polygon(x="x", y="y", linestring_id="pid", polygon_id="pid", close=FALSE)
    sf::st_crs(pol) <- crs
    return(pol)
}

bbLst2Pol <- function(bbLst, crs=sf::st_crs()) {

    bbId <- paste0("bb_", seq_along(bbLst))
    pol <- mapply(bb2Pol, bbLst, SIMPLIFY = FALSE) |>
        rbindlist() |>
        sf::st_as_sf(sf_column_name="geometry")
    sf::st_crs(pol) <- crs
    return(pol)
}

rowAgg <- function(agg=c("min", "max", "mean")) {
    agg <- match.arg(agg)
    ret <- switch(agg,
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
    return(ret)
}
