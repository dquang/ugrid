#' Shiny module for output as vectors
#' @keywords internal
map2dUi <- function(id) {

    ns <- shiny::NS(id)
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("map2d-sb"), width = "25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::sliderInput(ns("lyr"), "Select a layer", min=1L, max=10L,
                                       value=1L, step=1L, pre="Layer "),
                    shiny::p("To update the values for variables and time step, please select a case below."),
                    shiny::selectInput(ns("ncVar1"), "Select variable", choices=""),
                    shiny::selectizeInput(ns("tsIdx1"), "Select time step", choices=""),
                    shiny::radioButtons(ns("agg1"), "Select aggregation",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(ns("ncNames1"), "Select domain(s)",
                                              choices="", multiple=TRUE, search=TRUE),
                    shinyWidgets::virtualSelectInput(
                            ns("feats"), "Features of Interest (to select touching domains)", choices="",
                            multiple=TRUE, search=TRUE),
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select a case for the main map",
                                              choices="", multiple=FALSE,
                                              autoSelectFirstOption=TRUE, hideClearButton=FALSE),
                    shinyWidgets::virtualSelectInput(ns("cases2"), "Select a case for the second map (to compare)",
                                                     choices="", multiple=FALSE,
                                                     autoSelectFirstOption=FALSE, hideClearButton=FALSE),
                    bslib::input_task_button(ns("findDomains"), "Find relevant domains!")
                ),
                bslib::accordion_panel(
                    title="Isolines", icon=shiny::icon("bars-staggered"),
                    shiny::checkboxInput(ns("isoline"), "Add isoline", value=FALSE),
                    shiny::textInput(ns("resolution"), "Raster resolution",
                                     placeholder="numbers in CRS unit"),
                    shiny::sliderInput(ns("nbin"), "Number of bins", 1, 100, 10),
                    shiny::textInput(ns("binWidth"), "Bin width", placeholder="number in CRS unit"),
                    shinyWidgets::prettySwitch(ns("dryAsNa"), "For water level, treat dry as NaN", value=FALSE)
                ),
                bslib::accordion_panel(
                    title="Symbology", icon=shiny::icon("gears"),
                    shiny::sliderInput(ns("nClass"), "Number of classes", 1, 15, 5),
                    shiny::selectInput(
                        ns("clsStyle"), "Classification style",
                           choices=c("kmeans", "pretty", "quantile", "equal", "fischer",
                                     "hclust", "bclust",
                                     "jenks", "dpih", "headtails", "maximum", "box")
                           ),
                    colorPaletteInput(ns("colPal"), "Color palette"),
                    shinyWidgets::prettySwitch(ns("continuous"), "Continous palette", value=TRUE),
                    shinyWidgets::prettySwitch(ns("colReverse"), "Reversed palette", value=FALSE),
                    shinyWidgets::prettyCheckboxGroup(
                        ns("colFilters"), label="Palette filters",
                        choices=c("Colorblind friendly"="cbf",
                                  "High fairness" = "fair",
                                  "Contrast with white" = "ctW",
                                  "Contrast with black" = "crB"
                                  )
                        ),
                    shiny::selectizeInput(ns("colSeries"), "Color series", multiple=TRUE,
                                          choices=cols4all::c4a_series(type=, as.data.frame = FALSE),
                                          selected=c("brewer", "cols4all", "matplotlib", "tableau"))
                ),
                bslib::accordion_panel(
                    title="Compare", icon=shiny::icon("microscope"),
                    shiny::selectInput(ns("ncVar2"), "Select variable", choices=""),
                    shiny::selectizeInput(ns("tsIdx2"), "Select time step", choices=""),
                    shiny::radioButtons(ns("agg2"), "Select aggregation",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(ns("ncNames2"), "Select domain(s)",
                                                     choices="", multiple=TRUE, search=TRUE)
                ),
                bslib::accordion_panel(
                    title="Export data", icon=shiny::icon("file-export"),
                    shiny::actionButton(ns("prepDl"), "Prepare data for downloading..."),
                    shiny::downloadButton(ns("dlMap"), "Download map data!")
                )
            )
        ),
        shiny::fluidRow(
            column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genMap"), label="Generate map...                ", width='100%'))),
            column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genCmpMap"), label="Generate comparing map...      ", width='100%'))),
            column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genMapVector"), label="Generate velocity vector map...", width='100%')))
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                mapgl::maplibreOutput(ns("map2d"), height="550px"))
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-cmp-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                mapgl::maplibreCompareOutput(ns("map2dCmp"), height="550px")
            )
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-vector-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                mapgl::maplibreOutput(ns("map2dVector"), height="550px")
            ),
            shiny::sliderInput(
                ns("tsIdxVector"), label="Time step for vector map", min=1, max=100, value=1, step=1,
                animate=shiny::animationOptions(interval=2000, playButton="Play", pauseButton="Pause")
            )
        )

    )
}

#' @keywords internal
map2dServer <- function(id, cman) {
    shiny::moduleServer(id=id, function(input, output, session) {
        shinyjs::hide(id="lyr")
        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="map2d-cmp-card")
        shinyjs::hide(id="map2d-card")
        map2d <- shiny::reactiveVal()
        map2dCmp <- shiny::reactiveVal()
        map2dVec <- shiny::reactiveVal()
        gpkgFile <- shiny::reactiveVal()

        observeEvent(input$findDomains, {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            selectedCases <- c(input$cases1, input$cases2) |> unique()
            selectedCases <- selectedCases[nchar(selectedCases) > 0]
            if (length(selectedCases) < 1) {
                shiny::showNotification("Please select a case first!")
                return(NULL)
            }
            feats <- cman$layer[cman$layer$fid %in% input$feats, ]
            if (isTRUE(nrow(feats) < 1)) {
                shiny::showNotification("Please select at least one feature of interest first!")
                return(NULL)
            }
            hashes <- cman$tbl[caseName %in% selectedCases, hash]
            thisHashes <- hashes[!hashes %in% names(cman$ugrids)]
            nCores <- parallel::detectCores()
            doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
            `%dopar%` <- foreach::`%dopar%`
            tbl <- data.table::copy(cman$tbl)
            if (length(cman$ugrids) > 0) {
                progress$set(value=0.3, message=paste0("Generating face polygons for: ",
                                                       length(cman$ugrids), " domains if necessary..."))
                cMeshes <- foreach::foreach(aM=cman$ugrids, .combine=c) %dopar% {
                    aM$buildFace2DPoly()
                    list(aM)
                }
                names(cMeshes) <- names(cman$ugrids)
                cman$ugrids <- cMeshes
            }
            if (length(thisHashes) > 0) {
                progress$set(value=0.6, message=paste0("Generating face polygons for: ",
                                                       length(thisHashes), " more domains..."))
                meshes <- foreach::foreach(aH=thisHashes, .combine=c) %dopar% {
                    aM <- Ugrid$new(tbl[hash==aH, path], crs=tbl[hash==aH, crsid])
                    aM$buildFace2DPoly()
                    ret <- list(aM)
                    names(ret) <- aH
                    ret
                }
                cman$ugrids <- c(cman$ugrids, meshes)
            }
            progress$set(value=0.8, message=paste0("Finding relevant domains..."))
            ringLst <- lapply(hashes, function(x) {
                if (is.na(sf::st_crs(cman$ugrids[[x]]$m2D$fRing))) {
                    if (is.na(tbl[hash == x, crsid])) {
                        return(x)
                    } else {
                        cman$ugrids[[x]]$crs <- tbl[hash == x, crsid]
                        sf::st_crs(cman$ugrids[[x]]$m2D$face2D) <- tbl[hash == x, crsid]
                        sf::st_crs(cman$ugrids[[x]]$m2D$fRing) <- tbl[hash == x, crsid]
                    }
                }
                pol <- sf::st_transform(cman$ugrids[[x]]$m2D$fRing, "EPSG:4326")
                return(pol)
            })
            woCrs <- ringLst[sapply(ringLst, chkChr)] |> unlist()
            if (length(woCrs) > 0) {
                woCrsCases <- tbl[hash %in% woCrs, unique(caseName)] |> paste(collapse=", ")
                shiny::showNotification(paste0("Following cases have no CRS: ", woCrsCases, ".
                                               Please check again or assign a CRS to the cases."))
                return(NULL)
            } else {
                fRings <- data.table::rbindlist(ringLst, ignore.attr = TRUE) |> sf::st_as_sf()
                fRings <- merge(fRings, tbl[, c("caseName", "path")], by="path")
                if (!inherits(feats, "sf")) {
                    shiny::showNotification("Check input!")
                    return(NULL)
                }
                pols <- feats[grepl("POLYGON", feats$ftype, fixed=TRUE), ]
                polsOverlap <- sf::st_overlaps(pols, fRings)
                pRet <- list()
                for (i in seq_along(polsOverlap)) {
                    pRet[[pols$fid[i]]] <- fRings$path[polsOverlap[[i]]]
                }
                lines <- feats[grepl("LINESTRING", feats$ftype, fixed=TRUE), ]
                lineInt <- sf::st_intersects(lines, fRings)
                lineRet <- list()
                for (i in seq_along(lineInt)) {
                    lineRet[[lines$fid[i]]] <- fRings$path[lineInt[[i]]]
                }
                selectedPath <- c(polsOverlap, lineInt) |> unlist(use.names = FALSE) |> unique() |> sort()
                selectedHash1 <- tbl[path %in% fRings$path[selectedPath] & caseName %in% input$cases1, hash]
                selectedHash2 <- tbl[path %in% fRings$path[selectedPath] & caseName %in% input$cases2, hash]
                shinyWidgets::updateVirtualSelect(inputId="ncNames1", selected=selectedHash1)
                shinyWidgets::updateVirtualSelect(inputId="ncNames2", selected=selectedHash2)
                cman$selectedByLines <- lineRet
                cman$selectedByPols <- pRet
                cman$selectedHash1 <- selectedHash1
                cman$selectedHash2 <- selectedHash2
                progress$set(value=0.95, message=paste0("Done."))
            }

        })

        palTbl <- shiny::reactive({
            getC4aTable(
                type=c("cat", "seq"), n=input$nClass + 1,
                filters=input$colFilters, series=input$colSeries
            )
        })
        shiny::observeEvent(palTbl(), {
            updateColorPaletteInput(
                inputId="colPal", reverse=input$colReverse, continuous=input$continuous,
                selected=input$colPal, palTbl=palTbl())
        })
        vecData <- shiny::reactiveVal()
        shiny::observeEvent(input$genMapVector, {
            progress <- shiny::Progress$new()
            progress$set(0.1, message="Generating vector layer for all time steps. It will take a while!")
            chk <- any(sapply(input$ncNames1, chkChr))
            if (!chk) {
                shiny::showNotification("Please select one or some domains.")
                return(NULL)
            }
            selectedMeshes <- lapply(cman$tbl[hash %in% input$ncNames1, path], addUgrid, cman=cman)
            # TODO: separate vector layers for each domains so that they can be reused.
            ret <- genVector4All(selectedMeshes)
            vecData(ret)
            # display the vector map of the selected time step
            tsId <- as.integer(input$tsIdx1)
            lgT <- paste0("Velocity at: ", selectedMeshes[[1]]$ts[tsId])
            if (inherits(ret[[tsId]], "sf")) {
                map1 <- mapgl::maplibre(bounds=ret[[tsId]]) |>
                    mapgl::add_line_layer(source=ret[[tsId]], id=session$ns("map2d-vector"),
                                          line_color="black", line_width=0.3) |>
                    mapgl::add_scale_control()|>
                    mapgl::add_geocoder_control(provider="osm") |>
                    mapgl::add_navigation_control(show_zoom=FALSE)
                map2dVec(map1)
            } else {
                shiny::showNotification("Velocity map is not available for the selected time step!")
            }
            progress$set(value=0.9, message="Done. Please use the time step slider to explore the velocity map.")
            shinyjs::show(id="map2d-vector-card")
            shinyjs::hide(id="map2d-cmp-card")
            shinyjs::hide(id="map2d-card")
            progress$close()

        })

        mapData1 <- shiny::reactive({
            agg1 <- input$agg1
            tsIdx1 <- ifelse(input$agg1 == "none", input$tsIdx1, -1L)
            ncNames1 <- input$ncNames1[nchar(input$ncNames1) > 0]
            ncVar1 <- input$ncVar1
            lyr <- input$lyr
            if (length(ncNames1) < 1)
                return(NULL)
            selectedMeshes <- lapply(cman$tbl[hash %in% ncNames1, path], addUgrid, cman=cman)
            if (length(selectedMeshes) < 1)
                return(NULL)
            meshLst <- getMapData(mesh=selectedMeshes, variable=ncVar1, lyr=lyr, tsIdx=tsIdx1, agg=agg1)
            polLst <- list()
            for (i in seq_along(meshLst)) {
                polLst[[i]] <- meshLst[[i]]$ret
                meshLst[[i]]$ret <- NULL
                thisHash <- cman$tbl[path == meshLst[[i]]$path, hash]
                cman$ugrids[[thisHash]] <- meshLst[[i]]
            }
            ret <- data.table::rbindlist(polLst) |> sf::st_as_sf()
            if (is(ret, "sf")) {
                res <- sf::st_area(ret[sample.int(nrow(ret), 1), ]) |> sqrt() |> pretty()
                shiny::updateTextInput(inputId="resolution",
                                       label=paste0("Raster resolution (suggest: ", res[1], ")"))
            }
            return(ret)
        })

        mapData2 <- shiny::reactive({
            ncNames2 <- input$ncNames2[nchar(input$ncNames2) > 0]
            if (length(ncNames2) < 1)
                ncNames2 <- input$ncNames1
            lyr <- input$lyr
            selectedMeshes <- lapply(cman$tbl[hash %in% ncNames2, path], addUgrid, cman=cman)
            if (length(selectedMeshes) < 1)
                return(NULL)
            agg2 <- input$agg2
            tsIdx2 <- ifelse(agg2 == "none", input$tsIdx2, -1L)
            ncVar2 <- input$ncVar2
            meshLst <- getMapData(mesh=selectedMeshes, variable=ncVar2, lyr=lyr, tsIdx=tsIdx2, agg=agg2)
            polLst <- list()
            for (i in seq_along(meshLst)) {
                polLst[[i]] <- meshLst[[i]]$ret
                meshLst[[i]]$ret <- NULL
                thisHash <- cman$tbl[path == meshLst[[i]]$path, hash]
                cman$ugrids[[thisHash]] <- meshLst[[i]]
            }
            ret <- data.table::rbindlist(polLst) |> sf::st_as_sf()
            return(ret)
        })

        isolines1 <- shiny::reactive({
            ncNames1 <- input$ncNames1[nchar(input$ncNames1) > 0]
            if (length(ncNames1) < 1)
                return(NULL)
            selectedMeshes <- lapply(cman$tbl[hash %in% ncNames1, path], addUgrid, cman=cman)
            if (!isTRUE(input$isoline) | is.na(selectedMeshes[[1]]$crs)) {
                return(NULL)
            } else if (inherits(mapData1(), "sf")) {
                mdta <- isolate(mapData1())
                if (input$ncVar1 == "sea_surface_height" & input$dryAsNa)
                    mdta[[input$ncVar1]][mdta[[input$ncVar1]] < 1e-9] <- NaN
                stdRes <- dist(sf::st_coordinates(mdta[1, ]))[1] |> pretty()
                binWidth <- as.numeric(input$binWidth)
                res <- strsplit(input$resolution, split=" ")
                res <- as.numeric(input$binWidth)
                res <- res[!is.na(res)]
                if (!length(res) %in% 1:2)
                    res <- stdRes[1]
                if (is.na(binWidth))
                    binWidth <- NULL
                dta <- tryCatch({
                    genIsoline(x=mdta, field=input$ncVar1, nbin=input$nbin,
                               resolution=res, binWidth=binWidth)
                }, error = function(e)
                    shiny::showNotification("Error while generating isolines. Check parameters!")
                )
                return(dta)
            }
        })

        isolines2 <- shiny::reactive({

            ncNames2 <- input$ncNames2[nchar(input$ncNames2) > 0]
            if (length(ncNames2) < 1)
                ncNames2 <- input$ncNames1
            selectedMeshes <- lapply(cman$tbl[hash %in% ncNames2, path], addUgrid, cman=cman)
            if (length(selectedMeshes) < 1)
                return(NULL)
            if (!isTRUE(input$isoline) | is.na(selectedMeshes[[1]]$crs)) {
                return(NULL)
            } else if (inherits(mapData2(), "sf")) {

                mdta <- isolate(mapData2())
                if (input$ncVar2 == "sea_surface_height" & input$dryAsNa)
                    mdta[[input$ncVar1]][mdta[[input$ncVar2]] < 1e-9] <- NaN
                stdRes <- dist(sf::st_coordinates(mdta[1, ]))[1] |> pretty()
                binWidth <- as.numeric(input$binWidth)
                res <- strsplit(input$resolution, split=" ")
                res <- as.numeric(input$binWidth)
                res <- res[!is.na(res)]
                if (!length(res) %in% 1:2)
                    res <- stdRes[1]
                if (is.na(binWidth))
                    binWidth <- NULL
                dta <- tryCatch({
                    genIsoline(x=mdta, field=input$ncVar2, nbin=input$nbin,
                               resolution=res, binWidth=binWidth)
                }, error = function(e)
                    shiny::showNotification("Error while generating contours. Check parameters!")
                )
                return(dta)
            }
        })

        shiny::observeEvent(input$genMap, {

            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading data from NetCDF...")
            mdta1 <- mapData1()
            lyr <- input$lyr
            progress$set(value=0.6, message="Generating Map...")
            chkDta <- inherits(mdta1, "sf")
            if (!chkDta) {
                showNotification("Check input!")
            } else {
                aM <- cman$ugrids[[input$ncNames1[1]]]
                ncVar1 <- aM$getVarName(input$ncVar1)
                varAtt <- aM$atts[varName == ncVar1]
                ts1 <- paste0("At: ", aM$ts[as.integer(input$tsIdx1)])
                lgT1 <- sprintf("%s (%s) [%s]",
                                varAtt[grepl("long_name", name), val],
                                ifelse(input$agg1 == "none", ts1, input$agg1),
                                varAtt[grepl("unit", name), val])
                rsf <- rescale(sf::st_bbox(mdta1))
                map1 <- genMap(pol=mdta1, field=input$ncVar1, n=input$nClass, style=input$clsStyle,
                               legendTitle=lgT1, mapId=session$ns("map1"), addControls=TRUE,
                               colPal=input$colPal, continuous=input$continuous, reverse=input$colReverse, rsf=rsf)
                if (input$isoline)
                    progress$set(value=0.7, message="Generating isolines....")
                iline <- isolines1()
                if (inherits(iline, "sf")) {
                    if (is.na(sf::st_crs(iline)))
                        iline <- squash2Bbox(iline, , rsf=rsf)
                    map1 <- mapgl::add_line_layer(map1, source=iline, id=session$ns("map1_isoline"),
                                                  tooltip="level", line_color="black", line_width=0.2)
                    # set lables every 1000 m
                    nPoints <- sf::st_length(iline) %/% 1000 |> as.integer()
                    nPoints[nPoints < 1] <- 1
                    if (sf::st_is_longlat(iline)) {
                        pts <- sf::st_segmentize(iline, dfMaxLength=1000) |>
                            sf::st_centroid()
                    } else {
                        pts <- sf::st_line_sample(iline, n=nPoints) |>
                            sf::st_as_sf()
                    }
                    pts[["level"]] <- iline[["level"]]
                    if (nrow(pts) > 0) {
                        pts <- sf::st_cast(pts, "POINT")
                        map1 <- mapgl::add_symbol_layer(map1, source=pts, id=session$ns("map1_label"),
                                                        text_field=list("get", "level")
                        )
                    }
                }
                mapgl::maplibre_proxy("map2d") |>
                    mapgl::clear_controls()
                map2d(map1)
                shinyjs::show(id="map2d-card")
                shinyjs::hide(id="map2d-cmp-card")
                shinyjs::hide(id="map2d-vector-card")
                progress$set(value=0.9, message="Done. Loading map....")
            }
        })

        shiny::observeEvent(input$genCmpMap, {

            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading data from NetCDF...")
            mdta1 <- mapData1()
            mdta2 <- mapData2()
            if (input$isoline)
                progress$set(value=0.6, message="Generating isolines....")
            iline1 <- isolines1()
            iline2 <- isolines2()
            chkDta <- inherits(mdta1, "sf") & inherits(mdta2, "sf")
            if (!chkDta) {
                showNotification("Check input!")
            } else {
                ncNames2 <- input$ncNames2[nchar(input$ncNames2) > 0]
                if (length(ncNames2) < 1)
                    ncNames2 <- input$ncNames1
                progress$set(value=0.75, message="Generating maps....")
                values <- c(mdta1[[input$ncVar1]], mdta2[[input$ncVar2]]) |> unique()
                valClass <- if (length(values) < 2) NULL else
                    classInt::classIntervals(var=values, n=input$nClass, style=input$clsStyle)
                aM1 <- cman$ugrids[[input$ncNames1[1]]]
                ncVar1 <- aM1$getVarName(input$ncVar1)
                varAtt1 <- aM1$atts[varName == ncVar1]
                aM2 <- cman$ugrids[[ncNames2[1]]]
                ncVar2 <- aM2$getVarName(input$ncVar2)
                varAtt2 <- aM2$atts[varName == ncVar2]
                ts1 <- paste0("At: ", aM1$ts[as.integer(input$tsIdx1)])
                ts2 <- paste0("At: ", aM2$ts[as.integer(input$tsIdx2)])
                lgT1 <- sprintf("%s (%s) [%s]",
                                varAtt1[grepl("long_name", name), val],
                                ifelse(input$agg1 == "none", ts1, input$agg1),
                                varAtt1[grepl("unit", name), val])
                lgT2 <- sprintf("%s (%s) [%s]",
                                varAtt2[grepl("long_name", name), val],
                                ifelse(input$agg2 == "none", ts2, input$agg2),
                                varAtt2[grepl("unit", name), val])
                map1 <- genMap(mdta1, field=input$ncVar1, valClass=valClass,
                               legendTitle=lgT1, mapId=session$ns("map1"), colPal=input$colPal,
                               continuous=input$continuous, reverse=input$colReverse)
                if (inherits(iline1, "sf")) {
                    map1 <- mapgl::add_line_layer(
                        map1,  source=iline1, line_color="black", id=session$ns("map1_isoline"),
                        line_width=0.2, tooltip="level")
                    # set lables every 1000 m
                    nPoints <- sf::st_length(iline1) %/% 1000 |> as.integer()
                    nPoints[nPoints < 1] <- 1
                    if (sf::st_is_longlat(iline1)) {
                        pts <- sf::st_segmentize(iline1, dfMaxLength=1000) |>
                            sf::st_centroid()
                    } else {
                        pts <- sf::st_line_sample(iline1, n=nPoints) |>
                            sf::st_as_sf()
                    }
                    pts[["level"]] <- iline1[["level"]]
                    if (nrow(pts) > 0) {
                        pts <- sf::st_cast(pts, "POINT")
                        map1 <- mapgl::add_symbol_layer(map1, source=pts, id=session$ns("map1_label"),
                                                        text_field=list("get", "level")
                        )
                    }
                }
                map2 <- genMap(mdta2, field=input$ncVar2, valClass=valClass,
                               mapId=session$ns("map2"), colPal=input$colPal, legendTitle=lgT2,
                               continuous=input$continuous, reverse=input$colReverse, legendPos="top-right")
                if (inherits(iline2, "sf")) {
                    map2 <- mapgl::add_line_layer(
                        map2, source=iline2, id=session$ns("map2_isoline"),
                        line_color="black", line_width=0.2, tooltip="level")
                    # set lables every 1000 m
                    nPoints <- sf::st_length(iline2) %/% 1000 |> as.integer()
                    nPoints[nPoints < 1] <- 1
                    if (sf::st_is_longlat(iline2)) {
                        pts <- sf::st_segmentize(iline2, dfMaxLength=1000) |>
                            sf::st_centroid()
                    } else {
                        pts <- sf::st_line_sample(iline2, n=nPoints) |>
                            sf::st_as_sf()
                    }
                    pts[["level"]] <- iline2[["level"]]
                    if (nrow(pts) > 0) {
                        pts <- sf::st_cast(pts, "POINT")
                        map2 <- mapgl::add_symbol_layer(map2, source=pts, id=session$ns("map1_label"),
                                                        text_field=list("get", "level")
                        )
                    }
                }
                map2dCmp(mapgl::compare(map1, map2))
                shinyjs::show(id="map2d-cmp-card")
                shinyjs::hide(id="map2d-card")
                shinyjs::hide(id="map2d-vector-card")
                progress$set(value=0.9, message="Done. Loading maps....")
            }
        })

        shiny::observeEvent(input$genMapVector, {

            progress <- shiny::Progress$new()
            progress$set(value=0.3, message="Reading data from NetCDF...")
            vdta <- vecData()
            progress$set(value=0.6, message="Generating Map...")
            on.exit(progress$close())
            chkDta <- inherits(vdta, "sf")
            if (!chkDta) {
                showNotification("Check input!")
            } else {
                aM <- cman$ugrids[[input$ncNames1[1]]]
                ncVar1 <- aM$getVarName(input$ncVar1)
                varAtt <- aM$atts[varName == ncVar1]
                lgT <- paste0("Velocity at: ", aM$ts[as.integer(input$tsIdx)])
                map1 <- mapgl::maplibre(bounds=vdta) |>
                    mapgl::add_line_layer(source=vdta, id=session$ns("map2d-vector"),
                                          line_color="black", line_width=0.3) |>
                    mapgl::add_scale_control()|>
                    mapgl::add_geocoder_control(provider="osm") |>
                    mapgl::add_navigation_control(show_zoom=FALSE)
                map2dVec(map1)
                progress$set(value=0.9, message="Done. Loading map....")
                shinyjs::show(id="map2d-vector-card")
                shinyjs::hide(id="map2d-cmp-card")
                shinyjs::hide(id="map2d-card")
            }
        })

        shiny::observeEvent(input$tsIdxVector, {
            mprox <- mapgl::maplibre_proxy(mapId="map2dVector")
            vec <- vecData()[[input$tsIdxVector]]
            if (inherits(vec, "sf"))
                mapgl::set_source(mprox, layer_id=session$ns("map2d-vector"), source=)
            else
                shiny::showNotification("Velocity map is not available for the selected time step!")
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$cases1, {
            caseHash <- cman$tbl[caseName %in% input$cases1, hash]
            if (length(caseHash) < 1)
                return(NULL)
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading general information for the project.")
            addedHash <- names(cman$ugrids)
            intHash <- intersect(addedHash, caseHash)
            sampleHash <- if (length(intHash) > 0) intHash[1] else caseHash[1]
            aM <- addUgrid(path=cman$tbl[hash == sampleHash, path], cman=cman)
            ncVars <- aM$m2D$face
            ncVars <- ncVars[!ncVars %in% aM$m2D$topo]
            if (any(aM$vars[!is.na(ndims), ndims > 2])) {
                shinyjs::show("lyr")
                shiny::updateSliderInput(inputId="lyr", max=aM$dims[name == aM$m2D$topo$layer_dimension, length])
            } else {
                shinyjs::hide("lyr")
            }
            shiny::updateSelectInput(inputId="ncVar1", choices=names(ncVars))
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE, selected=tsIds[2])
                shiny::updateSliderInput(inputId="aniTsIdx", max=aM$totalTs)
                shiny::updateSliderInput(inputId="tsIdxVector", max=aM$totalTs)
                if (!chkChr(input$cases2))
                    shiny::updateSelectizeInput(inputId="tsIdx2", choices=tsIds, server=TRUE, selected=tsIds[2])
            }
            ncLst <- cman$tbl$hash
            tbl <- rbind(cman$tbl[caseName %in% input$cases1], cman$tbl[!caseName %in% input$cases1])
            ncLst <- shinyWidgets::prepare_choices(tbl, label=ncName,
                                                   value=hash, group_by=caseName, alias=path)
            shinyWidgets::updateVirtualSelect(inputId="ncNames1", choices=ncLst)
            if (!chkChr(input$cases2)) {
                shiny::updateSelectInput(inputId="ncVar2", choices=names(ncVars))
                shinyWidgets::updateVirtualSelect(inputId="ncNames2", choices=ncLst)
            }
            progress$set(value=0.9, message="Done.")
        }, ignoreInit=TRUE)

        shiny::observeEvent(input$cases2, {
            caseHash <- cman$tbl[caseName %in% input$cases2, hash]
            if (length(caseHash) < 1)
                return(NULL)
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading general information for the project.")
            addedHash <- names(cman$ugrids)
            intHash <- intersect(addedHash, caseHash)
            sampleHash <- if (length(intHash) > 0) intHash[1] else caseHash[1]
            aM <- addUgrid(path=cman$tbl[hash == sampleHash, path], cman=cman)
            ncVars <- aM$m2D$face
            ncVars <- ncVars[!ncVars %in% aM$m2D$topo]
            shiny::updateSelectInput(inputId="ncVar2", choices=names(ncVars))
            if (any(aM$vars[!is.na(ndims), ndims > 2])) {
                shinyjs::show("lyr")
                shiny::updateSliderInput(inputId="lyr", max=aM$dims[name == aM$m2D$topo$layer_dimension, length])
            } else {
                shinyjs::hide("lyr")
            }
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx2", choices=tsIds, server=TRUE, selected=tsIds[2])
                if (!chkChr(input$cases1))
                    shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE, selected=tsIds[2])
            }
            tbl <- rbind(cman$tbl[caseName %in% input$cases2], cman$tbl[!caseName %in% input$cases2])
            ncLst <- shinyWidgets::prepare_choices(tbl, label=ncName,
                                                   value=hash, group_by=caseName, alias=path)
            shinyWidgets::updateVirtualSelect(inputId="ncNames2", choices=ncLst)
            if (!chkChr(input$cases1)) {
                shiny::updateSelectInput(inputId="ncVar1", choices=names(ncVars))
                shinyWidgets::updateVirtualSelect(inputId="ncNames1", choices=ncLst)
            }
            progress$set(value=0.9, message="Done.")
        }, ignoreInit=TRUE)

        observeEvent(input$findDomains, {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            selectedCases <- c(input$cases1, input$cases2) |> unique()
            selectedCases <- selectedCases[nchar(selectedCases) > 0]
            if (length(selectedCases) < 1) {
                shiny::showNotification("Please select a case first!")
                return(NULL)
            }
            feats <- cman$layer[cman$layer$fid %in% input$feats, ]
            if (isTRUE(nrow(feats) < 1) | !inherits(feats, "sf")) {
                shiny::showNotification("Please select at least one feature of interest first!")
                return(NULL)
            }
            hashes <- cman$tbl[caseName %in% selectedCases, hash]
            thisHashes <- hashes[!hashes %in% names(cman$ugrids)]
            nCores <- parallel::detectCores()
            doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
            `%dopar%` <- foreach::`%dopar%`
            tbl <- data.table::copy(cman$tbl)
            if (length(cman$ugrids) > 0) {
                progress$set(value=0.3, message=paste0("Generating face polygons for: ",
                                                       length(cman$ugrids), " domains if necessary..."))
                cMeshes <- foreach::foreach(aM=cman$ugrids, .combine=c) %dopar% {
                    aM$buildFace2DPoly()
                    list(aM)
                }
                names(cMeshes) <- names(cman$ugrids)
                cman$ugrids <- cMeshes
            }
            if (length(thisHashes) > 0) {
                progress$set(value=0.6, message=paste0("Generating face polygons for: ",
                                                       length(thisHashes), " more domains..."))
                meshes <- foreach::foreach(aH=thisHashes, .combine=c) %dopar% {
                    aM <- Ugrid$new(tbl[hash==aH, path], crs=tbl[hash==aH, crsid])
                    aM$buildFace2DPoly()
                    ret <- list(aM)
                    names(ret) <- aH
                    ret
                }
                cman$ugrids <- c(cman$ugrids, meshes)
            }
            progress$set(value=0.8, message=paste0("Finding relevant domains..."))
            ringLst <- lapply(hashes, function(x) {
                if (is.na(sf::st_crs(cman$ugrids[[x]]$m2D$fRing))) {
                    if (is.na(tbl[hash == x, crsid])) {
                        return(x)
                    } else {
                        cman$ugrids[[x]]$crs <- tbl[hash == x, crsid]
                        sf::st_crs(cman$ugrids[[x]]$m2D$face2D) <- tbl[hash == x, crsid]
                        sf::st_crs(cman$ugrids[[x]]$m2D$fRing) <- tbl[hash == x, crsid]
                    }
                }
                pol <- sf::st_transform(cman$ugrids[[x]]$m2D$fRing, "EPSG:4326")
                return(pol)
            })
            woCrs <- ringLst[sapply(ringLst, chkChr)] |> unlist()
            if (length(woCrs) > 0) {
                woCrsCases <- tbl[hash %in% woCrs, unique(caseName)] |> paste(collapse=", ")
                shiny::showNotification(paste0("Following cases have no CRS: ", woCrsCases, ".
                                               Please check again or assign a CRS to the cases."))
                return(NULL)
            } else {
                # ignore.attr = TRUE because there are sometimes MULTIPOLYGON inside
                fRings <- data.table::rbindlist(ringLst, ignore.attr = TRUE) |> sf::st_as_sf()
                fRings <- merge(fRings, tbl[, c("caseName", "path")], by="path")
                pols <- feats[grepl("POLYGON", feats$ftype, fixed=TRUE), ]
                polsOverlap <- sf::st_overlaps(pols, fRings)
                pRet <- list()
                for (i in seq_along(polsOverlap)) {
                    pRet[[pols$fid[i]]] <- fRings$path[polsOverlap[[i]]]
                }
                lines <- feats[grepl("LINESTRING", feats$ftype, fixed=TRUE), ]
                lineInt <- sf::st_intersects(lines, fRings)
                lineRet <- list()
                for (i in seq_along(lineInt)) {
                    lineRet[[lines$fid[i]]] <- fRings$path[lineInt[[i]]]
                }
                selectedPath <- c(polsOverlap, lineInt) |> unlist(use.names = FALSE) |> unique() |> sort()
                if (length(selectedPath) < 1) {
                    progress$set(value=0.95, message=paste0("No domains found intersected / overlapped with the selected features"))
                } else {
                    selectedHash1 <- tbl[path %in% fRings$path[selectedPath] & caseName %in% input$cases1, hash]
                    selectedHash2 <- tbl[path %in% fRings$path[selectedPath] & caseName %in% input$cases2, hash]
                    shinyWidgets::updateVirtualSelect(inputId="ncNames1", selected=selectedHash1)
                    shinyWidgets::updateVirtualSelect(inputId="ncNames2", selected=selectedHash2)
                    cman$selectedByLines <- lineRet
                    cman$selectedByPols <- pRet
                    cman$selectedHash1 <- selectedHash1
                    cman$selectedHash2 <- selectedHash2
                    progress$set(value=0.95, message=paste0("Done."))
                }
            }
        })

        output$map2d <- mapgl::renderMaplibre({
            map2d()
        })

        output$map2dCmp <- mapgl::renderMaplibreCompare({
            map2dCmp()
        })

        output$map2dVector <- mapgl::renderMaplibre({
            map2dVec()
        })

        shiny::observeEvent(input$prepDl, {

            progress <- shiny::Progress$new()
            on.exit(progress$close())
            shinyjs::hide("dlMap")
            progress$set(value=0.3, message="Reading data from NetCDF if not yet...")
            mdta1 <- isolate(mapData1())
            mdta2 <- NULL
            withMap2 <- !is.null(isolate(map2dCmp()))
            if (withMap2)
                mdta2 <- isolate(mapData2())
            withIsoline <- isolate(input$isoline)
            iline1 <- NULL
            iline2 <- NULL
            if (withIsoline) {
                iline1 <- isolate(isolines1())
                if (withMap2)
                    iline2 <- isolate(isolines2())
            }
            aM <- cman$ugrids[[input$ncNames1[1]]]
            ncVar1 <- aM$getVarName(input$ncVar1)
            varAtt <- aM$atts[varName == ncVar1]
            ts1 <- aM$ts[as.integer(input$tsIdx)]
            ts2 <- aM$ts[as.integer(input$tsIdx2)]
            fids <- cman$tbl[hash %in% input$ncNames1, paste(hash, collapse = ",")]
            lgT1 <- sprintf("%s_%s_%s",
                            varAtt[grepl("long_name", name), val],
                            ifelse(input$agg1 == "none", ts1, input$agg1),
                            varAtt[grepl("unit", name), val])
            lgT2 <- sprintf("%s_%s_%s",
                            varAtt[grepl("long_name", name), val],
                            ifelse(input$agg2 == "none", ts2, input$agg2),
                            varAtt[grepl("unit", name), val])
            thisLabel <- session$ns("dlMapLabel")
            pid <- session$ns("")
            progress$set(value=0.7, message="Sending the task to a background process...")
            promises::future_promise(
                ugrid:::prepareData4Download(mdta1=mdta1, mdta2=mdta2, iline1=iline1,
                                             iline2=iline2, fids=fids, pid=pid, lgT1=lgT1, lgT2=lgT2)
            ) |>
                promises::then(
                    onFulfilled = function(value) {
                        gpkgFile(value)
                        shinyjs::show("dlMap")
                        shiny::showNotification("Map data is ready for downloading!")
                    },
                    onRejected = function(reason) {
                        showNotification(paste0("Fail to prepare map data for downloading. Reason: ", reason$message))
                    }
                )
            progress$set(value=0.9, message="Data is being prepared in background. The download button will be enabled when the preparation is done...")
        })

        output$dlMap <- shiny::downloadHandler(
            filename=paste0(input$ncVar1, "_map_data.gpkg"),
            contentType="application/geopackage",
            content=function(file) {
                file.copy(from=gpkgFile(), to=file)
            }
        )
    })
}
