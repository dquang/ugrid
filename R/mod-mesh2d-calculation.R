#' Shiny module for
map2dCalcUi <- function(id) {

    ns <- shiny::NS(id)
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("map2d-sb"), width="25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::p("To update the values for variables and time step, please select a case below."),
                    shiny::textInput(ns("resolution"), label="Raster resolution",
                                     placeholder="one ore two numbers seperated by a space for raster resolution"),
                    shiny::hr(),
                    shiny::h4("First raster parameters"),
                    shiny::selectInput(ns("ncVar1"), "Variable", choices=""),
                    shiny::selectizeInput(ns("tsIdx1"), "Time step", choices=""),
                    shiny::radioButtons(ns("agg1"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(
                        ns("ncNames1"), "NetCDF files / domains", choices="", multiple=TRUE, search=TRUE),
                    bslib::tooltip(
                        shinyWidgets::virtualSelectInput(
                            ns("feats"), "Features of Interest (to select touching domains)", choices="",
                            multiple=TRUE, search=TRUE),
                        "To select domains from all project that are touched or intersected with given features.
                        Please select the features from the list"),
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select case for the first raster",
                                              choices="", multiple=TRUE, autoSelectFirstOption=TRUE),
                    shinyWidgets::virtualSelectInput(ns("cases2"), "Select case for the second raster",
                                                     choices="", multiple=TRUE, autoSelectFirstOption=TRUE),
                    bslib::input_task_button(ns("findDomains"), "Find relevant domains!"),
                    shiny::hr(),
                    shiny::h4("Second raster parameters"),
                    shiny::selectizeInput(ns("ncVar2"), "Variable", choices="", multiple=TRUE,
                                          options=list(maxItems=1)),
                    shiny::selectizeInput(ns("tsIdx2"), "Time step", choices=""),
                    shiny::radioButtons(ns("agg2"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(ns("ncNames2"), "NetCDF files / domains",
                                              choices="", multiple=TRUE, search=TRUE)
                    ),
                bslib::accordion_panel(
                    title="Classification & Symbology", icon=shiny::icon("gears"),
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
                    shiny::selectizeInput(
                        ns("colSeries"), "Color series", multiple=TRUE,
                        choices=list("ColorBrewer"="brewer", "Carto"="carto", "cols4all"="cols4all",
                                  "Qualitative palettes"="poly", "Microsoft Power BI"="powerbi",
                                  "Scientific colour maps"="scio", "Python library Seaborn"="seaborn",
                                  "Tableau"="tableau"),
                        selected=c("tableau", "brewer", "powerbi", "cols4all"))
                ),
                bslib::accordion_panel(
                    title="Raster calculation", icon=shiny::icon("microscope"),
                    shiny::selectInput(ns("operator"), "Raster operator", choices=c("-", "+", "*", "/")),
                    bslib::input_task_button(ns("calculate"), "Calculate & generate map")
                ),
                bslib::accordion_panel(
                    title="Export data", icon=shiny::icon("file-export"),
                    shiny::actionButton(ns("prepDl"), "Prepare data for downloading...", disabled=TRUE),
                    shiny::downloadButton(ns("dlMap"), "Download map data!")
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(6, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genMap"), label="Generate map...", width='100%')))
            # shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
            #     id=ns("genCmpMap"), label="Generate comparing map...", width='100%')))
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                leaflet::leafletOutput(ns("map2d"), height="550px"))
            # bslib::input_task_button(ns("getFeat"), "Get drawn features")
            ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-cmp-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                leaflet::leafletOutput(ns("map2dCmp"), height="550px")
            )
        )

    )
}

map2dCalcServer <- function(id, cman) {

    shiny::moduleServer(id=id, function(input, output, session) {

        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="map2d-cmp-card")
        shinyjs::hide(id="map2d-card")
        map2d <- shiny::reactiveVal()
        map2dCmp <- shiny::reactiveVal()
        gpkgFile <- shiny::reactiveVal()
        bbox1 <- shiny::reactiveVal()
        bbox2 <- shiny::reactiveVal()

        shiny::observe({
            palTbl <- getC4aTable(
                type="cat", n=input$nClass + 1,
                filters=input$colFilters, series=input$colSeries
            )
            updateColorPaletteInput(
                inputId="colPal", reverse=input$colReverse, continuous=input$continuous,
                palTbl=palTbl)

        })

        mapData1 <- shiny::reactive({
            ret <- NULL
            ncNames1 <- input$ncNames1[nchar(input$ncNames1) > 0]
            if (length(ncNames1) > 0) {
                selectedMeshes <- lapply(cman$tbl[hash %in% ncNames1, path], addUgrid, cman=cman)
                tsIdx <- ifelse(input$agg1 == "none", input$tsIdx1, -1L)
                ret <- getMapData(mesh=selectedMeshes, variable=input$ncVar1, tsIdx=tsIdx, agg=input$agg1)
                if (is(ret, "sf")) {
                    bbox1(round(sf::st_bbox(ret)))
                    res <- sf::st_area(ret[sample.int(nrow(ret), 1), ]) |> sqrt() |> pretty()
                    shiny::updateTextInput(inputId="resolution",
                                           label=paste0("Raster resolution (suggest: ", res[1], ")"))
                }
            }
            return(ret)
        })
        mapData2 <- shiny::reactive({

            ncNames2 <- input$ncNames2[nchar(input$ncNames2) > 0]
            if (length(ncNames2) < 1)
                ncNames2 <- input$ncNames1
            ncVar2 <- if (chkChr(input$ncVar2)) input$ncVar2 else input$ncVar1
            ret <- NULL
            if (length(ncNames2) > 0) {
                selectedMeshes <- lapply(cman$tbl[hash %in% ncNames2, path], addUgrid, cman=cman)
                tsIdx <- ifelse(input$agg2 == "none", input$tsIdx2, -1L)
                ret <- getMapData(mesh=selectedMeshes, variable=ncVar2, tsIdx=tsIdx, agg=input$agg2)
                bbox2(round(sf::st_bbox(ret)))
            }
            return(ret)
        })
        calculatedRaster <- shiny::reactive({

            ncVar2 <- if (chkChr(input$ncVar2)) input$ncVar2 else input$ncVar1
            mdta1 <- mapData1()
            mdta2 <- mapData2()
            if (!is(mdta1, "sf") | !is(mdta2, "sf"))
                return(NULL)
            crs1 <- sf::st_crs(mdta1)
            crs2 <- sf::st_crs(mdta2)
            if (!sf::st_can_transform(crs2, crs1)) {
                shiny::showNotification("Coordinate systems of datasets are not defined or incompatible!")
                return(NULL)
            }
            mdta2 <- sf::st_transform(mdta2, crs1)
            bb1 <- terra::ext(mdta1)
            bb2 <- terra::ext(mdta2)
            if (!identical(bb1, bb2)) {
                bb <- bbLst2Pol(list(bb1, bb2)) |> terra::ext()
            } else {
                bb <- bb1
            }
            resolution <- strsplit(input$resolution, split=" ")[[1]]
            if (!rlang::is_bare_numeric(resolution)) {
                resolution <- sf::st_area(mdta1[sample.int(nrow(mdta1), 1), 1]) |> sqrt() |> pretty()
                resolution <- resolution[1]
                if (isTRUE(sf::st_is_longlat(mdta1))) {
                    # convert meter to degree with a rough ratio.
                    resolution <- pretty(resolution / (8*10^5))
                    resolution <- resolution[resolution > 0][1]
                }
            }
            tempRast <- terra::rast(bb, resolution=resolution, crs=terra::crs(mdta1))
            memInfo <- terra::mem_info(tempRast, print=FALSE)
            notFit <- memInfo[["fits_mem"]] < 1.0
            if (notFit) {
                msg <- shiny::markdown(
                    paste0("Raster is too large. It would need *",
                           round(memInfo[["needed"]], 3), " Gb* of RAM but only *",
                           round(memInfo[["available"]] * memInfo[["memfrac"]], 3),
                           " Gb* allocated. *Abort!*"))
                shiny::showNotification(msg)
                return(NULL)
            }
            x1 <- terra::vect(mdta1)
            # for a unknown reason, terra sometimes convert the values to characters!
            # that why we have to perform this step.
            x1[[input$ncVar1]] <- mdta1[[input$ncVar1]]
            ras1 <- terra::rasterize(
                x=x1,
                y=tempRast,
                field=input$ncVar1, fun="mean")
            x2 <- terra::vect(mdta2)
            x2[[ncVar2]] <- mdta2[[ncVar2]]
            ras2 <- terra::rasterize(
                x=x2,
                y=tempRast,
                field=ncVar2, fun="mean")
            if (exists(input$operator, mode="function")) # security check
                ras <- do.call(input$operator, list(ras1, ras2))
            else
                ras <- NULL
            return(ras)
        })
        shiny::observeEvent(input$genMap, {

            progress <- shiny::Progress$new()
            progress$set(value=0.3, message="Reading data from NetCDF...")
            mdta1 <- mapData1()
            progress$set(value=0.6, message="Generating Map...")
            on.exit(progress$close())
            chkDta <- inherits(mdta1, "sf")
            if (!chkDta) {
                showNotification("Check input!")
            } else {
                selectedMeshes <- lapply(cman$tbl[hash %in% input$ncNames1, path], addUgrid, cman=cman)
                aM <- selectedMeshes[[1]]
                ncVar <- aM$getVarName(input$ncVar1)
                varAtt <- aM$atts[varName == ncVar]
                ts1 <- paste0("At: ", aM$ts[as.integer(input$tsIdx1)])
                lgT1 <- sprintf("%s (%s) [%s]", input$ncVar1,
                                ifelse(input$agg1 == "none", ts1, input$agg1),
                                varAtt[grepl("unit", name), val])
                map1 <- genRasterMap(pol=mdta1, field=input$ncVar1, n=input$nClass, style=input$clsStyle,
                                     legendTitle=lgT1, mapId=session$ns("map1"), addControls=TRUE,
                                     colPal=input$colPal, reverse=input$colReverse)
                map2d(map1)
                shinyjs::show(id="map2d-card")
                shinyjs::hide(id="map2d-cmp-card")
                progress$set(value=0.9, message="Done. Loading map....")
            }
        })
        shiny::observeEvent(input$calculate, {

            ras <- calculatedRaster()
            if (!is(ras, "SpatRaster")) {
                shiny::showNotification("Check input!")
                return(NULL)
            }
            terra::crs(ras) <- "epsg:25833"
            values <- terra::values(ras, na.rm=TRUE)
            mm <- range(values)
            varClassInt <- tryCatch(
                classInt::classIntervals(var=values, n=input$nClass, style=input$clsStyle),
                error=function(e) NULL)
            if (inherits(varClassInt, "classIntervals")) {
                varColors <- cols4all::c4a(palette=input$colPal, n=length(varClassInt$brks), nm_invalid="interpolate",
                                           reverse=input$colReverse)

                labels <- format(varClassInt$brks, digits=3, scientific=(varClassInt$brks[1] < 0.001))
                pal <- leaflet::colorNumeric(varColors, domain=varClassInt$brks, na.color="#FF000000")
                addLegendMod <- function(map) leaflet::addLegend(map, labels=labels, colors=varColors,
                                                                 bins=length(varClassInt$brks), opacity=1)
            } else {
                varColors <- cols4all::c4a(palette=input$colPal, n=input$nClass,
                                           nm_invalid="interpolate", reverse=input$colReverse)
                pal <- leaflet::colorNumeric(varColors, domain=values, na.color="#FF000000")
                addLegendMod <- function(map) leaflet::addLegend(map, pal=pal, values=values, bins=input$nClass, opacity=1)
            }
            map1 <- leaflet::leaflet() |>
                leaflet::addTiles() |>
                leaflet::addRasterImage(ras, colors=pal, opacity=1) |>
                addLegendMod()
            shinyjs::show(id="map2d-card")
            shinyjs::hide(id="map2d-cmp-card")
            map2d(map1)
        })

        output$map2d <- mapgl::renderMaplibre({
            map2d()
        })
        observeEvent(input$findDomains, {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            selectedCases <- c(input$cases1, input$cases2) |> unique()
            selectedCases <- selectedCases[nchar(selectedCases) > 0]
            if (length(selectedCases) < 1) {
                shiny::showNotification("Please select a case first!")
                return(NULL)
            }
            feats <- cman$layer[cman$layer$featId %in% input$feats, ]
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
                fRings <- data.table::rbindlist(ringLst, ignore.attr = TRUE) |> sf::st_as_sf()
                fRings <- merge(fRings, tbl[, c("caseName", "path")], by="path")
                pols <- feats[grepl("POLYGON", feats$ftype, fixed=TRUE), ]
                polsOverlap <- sf::st_overlaps(pols, fRings)
                pRet <- list()
                for (i in seq_along(polsOverlap)) {
                    pRet[[pols$featId[i]]] <- fRings$path[polsOverlap[[i]]]
                }
                lines <- feats[grepl("LINESTRING", feats$ftype, fixed=TRUE), ]
                lineInt <- sf::st_intersects(lines, fRings)
                lineRet <- list()
                for (i in seq_along(lineInt)) {
                    lineRet[[lines$featId[i]]] <- fRings$path[lineInt[[i]]]
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
            shiny::updateSelectInput(inputId="ncVar1", choices=names(ncVars))
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE)
                if (!chkChr(input$cases2))
                    shiny::updateSelectizeInput(inputId="tsIdx2", choices=tsIds, server=TRUE)
            }
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
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx2", choices=tsIds, server=TRUE)
                if (!chkChr(input$cases1))
                    shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE)
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
        shiny::observeEvent(input$prepDl, {

            progress <- shiny::Progress$new()
            on.exit(progress$close())
            shinyjs::hide("dlMap")
            progress$set(value=0.3, message="Reading data from NetCDF if not yet...")
            mdta1 <- isolate(mapData())
            mdta2 <- NULL
            withMap2 <- !is.null(isolate(map2dCmp()))
            if (withMap2)
                mdta2 <- isolate(mapData2())
            withIsoline <- isolate(input$isoline)
            iline1 <- NULL
            iline2 <- NULL
            if (withIsoline) {
                iline1 <- isolate(isolines())
                if (withMap2)
                    iline2 <- isolate(isolines2())
            }
            aM <- cman$ugrids[[input$ncNames[1]]]
            ncVar <- aM$getVarName(input$ncVar)
            varAtt <- aM$atts[varName == ncVar]
            ts1 <- aM$ts[as.integer(input$tsIdx)]
            ts2 <- aM$ts[as.integer(input$tsIdx2)]
            fids <- cman$tbl[hash %in% input$ncNames, paste(hash, collapse = ",")]
            lgT1 <- sprintf("%s_%s_%s",
                            varAtt[grepl("long_name", name), val],
                            ifelse(input$agg == "none", ts1, input$agg),
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
