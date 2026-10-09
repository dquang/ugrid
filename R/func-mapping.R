#' Generate a Maplibre GL Map
#'
#' @param pol A sf object.
#' @param field The name of the column to use for classification.
#' @param values values of the dataset.
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
#' @param legendPos Position of the legend.
#' @param addControls Add some controls to the map?
#' @param addLine Add polygon line?
#' @param lineColor Line color of polygons.
#' @param lineWidth Line width of polygons.
#' @param rsf a list of rescaling functions returned from `rescale` function.
#' @returns An HTML widget for a Mapbox map.
#' @export
genMap <- function(
        pol, field, values=NA, n=5, style="kmeans", valClass=NULL,
        colPal="seaborn.bright", colType="cat", mapId="map1",
        continuous=TRUE, reverse=FALSE,
        legendTitle=field, legendWidth="300px", legendPos="top-left",
        addControls=FALSE, addLine=FALSE,
        lineColor="grey", lineWidth=0.1,
        rsf=NULL) {

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
#' @param values values of the dataset.
#' @param style Style of classification. One of: "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param valClass An object of the class `classIntervals` to set the classification manually
#' @param mapId ID of the map.
#' @param resolution Resolution of the raster.
#' @param legendTitle Legend title.
#' @param legendWidth Width of the legend.
#' @param legendPos Position of the legend.
#' @param addControls Add some controls to the map?
#' @param opacity Opacity of the legend.
#' @param rsf a list of rescaling functions returned from `rescale` function.
#' @returns An HTML widget for a Mapbox map.
#' @export
genRasterMap <- function(
        pol, field, values=NA, n=5, style="kmeans", valClass=NULL,
        mapId="map1", resolution=NULL,
        legendTitle=field, legendWidth="300px", legendPos="top-left",
        addControls=FALSE, opacity=1.0, rsf=NULL) {

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

#' Generate animation GIF of tmap from list of rasters
#'
#' @param rasLst List of raster files.
#' @param fname Output file name.
#' @param rNames Names of raster layers.
#' @param legendTitle Title for map legend.
#' @param n Number of color classes.
#' @param style Style for color classification.
#' @param colPal Color palette name.
#' @param colType Color type.
#' @param reverse Should color palette be reversed?
#' @export
genTmapAni <- function(rasLst, fname=tempfile(fileext=".gif"),
                       rNames=NULL, legendTitle="Value", n=5, style="kmeans",
                       colPal="seaborn.bright", colType="seq", reverse=FALSE){

    if (file.create(fname)) {
        unlink(fname)
    } else {
        warning("File: ", fname, "is not writeable!")
        return(NULL)
    }
    rStack <- terra::rast(rasLst)
    if (length(rNames) != length(rasLst))
        rNames <- paste0("T_", 1:length(rasLst))
    names(rStack) <- rNames
    values <- terra::values(rStack) |> as.numeric()
    colInfo <- genTmapColor(values=values, n=n, style=style, colPal=colPal,
                            colType=colType, reverse=reverse)
    colScale <- tmap::tm_scale_continuous(values=colInfo$color, ticks=colInfo$brks)

    tm <- tmap::tm_shape(rStack) +
        tmap::tm_basemap() +
        tmap::tm_raster(
            col.free=FALSE, col_alpha.free=FALSE, col.scale=colScale,
            col.legend=tmap::tm_legend(legendTitle)
        ) +
        tmap::tm_facets(pages=legendTitle) +
        tmap::tm_animate(fps = 12) +
        tmap::tm_options(facet.max = length(rasLst))
    imgRatio <-  terra::ncol(rStack) / terra::nrow(rStack)
    tmap::tmap_animation(tm, filename=fname, width=1080, heigh=1080 / imgRatio)
    tmap::tmap_save(tm, filename=paste0(dirname(fname), "facet_.png"), dpi=200)
    return(fname)
}

#' @keywords internal
genTmapRasterOutput <- function(pol, field, resolution=NULL, values=NULL, n=5, style="kmeans",
                                legendTitle="Value", zindex=401,
                                colPal="seaborn.bright", colType="seq", reverse=FALSE) {

    chk <- all(sapply(resolution, chkDbl)) & length(resolution) > 0
    if (!chk) {
        resolution <- sf::st_area(pol[sample.int(n=nrow(pol), size=1), ]) |>
            sqrt() |> round(8)
    }
    chkTf <- sf::st_can_transform(pol, 4326)
    pol2 <- terra::vect(pol)
    pol2[[field]] <- pol[[field]]
    tempRast <- terra::rast(pol2, resolution=resolution, crs=terra::crs(pol2))
    if (!chkTf) {
        pol2 <- rescale2(pol2)
        tempRast <- rescale2(tempRast)
        terra::crs(pol2) <- "epsg:4326"
        terra::crs(tempRast) <- "epsg:4326"
        message("original data has no CRS! Geometry was squashed to WGS84")
    } else {
        pol2 <- terra::project(pol2, "epsg:4326")
        tempRast <- terra::project(tempRast, "epsg:4326")
    }
    if (length(values) < 1)
        values <- pol[[field]]
    colInfo <- genTmapColor(values=values, n=n, style=style, colPal=colPal,
                            colType=colType, reverse=reverse)
    colScale <- tmap::tm_scale(values=colInfo$color, ticks=colInfo$brks)

    ras <- terra::rasterize(x=pol2, y=tempRast, field=field, fun="mean")
    tm <- tmap::tm_shape(ras) +
        tmap::tm_raster(
            col.free=FALSE, col_alpha.free=FALSE, col.scale=colScale,
            col.legend=tmap::tm_legend(legendTitle),
            zindex=zindex
        )
    if (chkTf)
        tm <- tm + tmap::tm_basemap(server="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")
    return(tm)
}

#' @keywords internal
genRasterTmap <- function(ras, colScale, legendTitle, zindex=401) {

    if (!inherits(ras, "SpatRaster"))
        return(NULL)
    if (nchar(terra::crs(ras)) < 1)
        ras <- rescale2(ras)
    tm <- tmap::tm_shape(ras) +
        tmap::tm_raster(
            col.free=FALSE, col_alpha.free=FALSE, col.scale=colScale,
            col.legend=tmap::tm_legend(legendTitle),
            zindex=zindex
        ) + tmap::tm_basemap(server="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png")

    return(tm)
}

#' Color classification for leaflet map
#'
#' @param values A numeric vector of values to classify
#' @param n Number of classes
#' @param style Style of classification. One of: "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param colPal Full name of a color palette.
#' @param colType Type of palette: "cat", "seq", "div" or "cyc".
#' @param reverse Should the palette be reversed?
#' @export
genTmapColor <- function(
        values=NA, n=5, style="kmeans",
        colPal="seaborn.bright", colType="seq", reverse=FALSE
) {

    values <- unique(values)
    values <- values[!is.na(values)]
    varClassInt <- tryCatch(classInt::classIntervals(var=values, n=n, style=style), error=function(e) NULL)
    if (inherits(varClassInt, "classIntervals")) {
        varColors <- cols4all::c4a(palette=colPal, n=length(varClassInt$brks),
                                   type=colType, nm_invalid="interpolate", reverse=reverse)
    } else {
        varColors <- cols4all::c4a(palette=colPal, n=n, type=colType,
                                   nm_invalid="interpolate", reverse=reverse)
    }
    ret <- list(color=varColors, brks=varClassInt$brks)
    return(ret)
}

#' Color classification for leaflet map
#'
#' @param values A numeric vector of values to classify
#' @param pol a data.frame.
#' @param field The name of the column to use for classification.
#' @param n Number of classes
#' @param style Style of classification. One of: "fixed", "sd", "equal", "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "maximum", or "box".
#' @param valClass An object of the class `classIntervals` to set the classification manually
#' @param colPal Full name of a color palette.
#' @param colType Type of palette: "cat", "seq", "div" or "cyc".
#' @param reverse Should the palette be reversed?
#' @param opacity Legend opacity
#' @export
genColorInfo <- function(
        values=NA, pol=NULL, field=NULL, n=5, style="kmeans", valClass=NULL,
        colPal="seaborn.bright", colType="cat", reverse=FALSE, opacity=1.0
) {

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
    ret <- list(pal=pal, legend=addLegendMod, colors=varColors)

    return(ret)
}
