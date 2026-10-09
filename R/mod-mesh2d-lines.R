map2dLineUi <- function(id) {

    ns <- shiny::NS(id)
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    tmpFolder <- tempdir()
    shiny::addResourcePath("raster", tmpFolder)
    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("map2d-sb"), width="25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::h4("First raster parameters"),
                    shiny::p("To update the values for variables and time step, please select a case below."),
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select case(s)",
                                                     choices=character(0), multiple=TRUE, autoSelectFirstOption=TRUE),
                    shiny::sliderInput(ns("lyr"), "Select a layer", min=1L, max=10L, value=1L, step=1L, pre="Layer "),
                    shiny::selectInput(ns("ncVar1"), "Variable", choices=character(0)),
                    shiny::selectizeInput(ns("tsIdx1"), "Time step", choices=character(0)),
                    shiny::radioButtons(ns("agg1"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(
                        ns("ncNames1"), "NetCDF files / domains", choices=character(0), multiple=TRUE, search=TRUE),
                    bslib::tooltip(
                        shinyWidgets::virtualSelectInput(
                            ns("feats"), "Features of Interest (to select touching domains)", choices=character(0),
                            multiple=TRUE, search=TRUE),
                        "To select domains from all project that are touched or intersected with given features.
                        Please select the features from the list"),
                    bslib::input_task_button(ns("sliceData"), "Slice data for the line!!"),
                    shiny::hr()
                ),
                bslib::accordion_panel(
                    title="Export data", icon=shiny::icon("file-export"),
                    shiny::actionButton(ns("prepDl"), "Prepare data for downloading...", disabled=TRUE),
                    shiny::downloadButton(ns("dlMap"), "Download map data!", disabled=TRUE)
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(6, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genOverview"), label="Generate domain overview", width='100%')))
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-card"),
            mapgl::maplibreOutput(ns("map2d"), height="550px")
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-line-plot"),
            plotly::plotlyOutput(ns("linePlot"), height="600px")
        )
    )
}

map2dLineServer <- function(id, cman) {

    shiny::moduleServer(id=id, function(input, output, session) {

        shinyjs::hide(id="lyr")
        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="map2d-card")
        shinyjs::hide(id="map2d-line-plot")
        map2d <- shiny::reactiveVal()
        mapData1 <- shiny::reactive({
            ncNames1 <- input$ncNames1[nchar(input$ncNames1) > 0]
            agg1 <- input$agg1
            tsIdx1 <- ifelse(input$agg1 == "none", input$tsIdx1, -1L)
            ncVar1 <- input$ncVar1
            selectedMeshes <- lapply(cman$tbl[hash %in% ncNames1, path], addUgrid, cman=cman)
            if (length(selectedMeshes) < 1)
                return(NULL)
            meshLst <- getMapData(mesh=selectedMeshes, variable=ncVar1, tsIdx=tsIdx1, agg=agg1)
            polLst <- list()
            for (i in seq_along(meshLst)) {
                polLst[[i]] <- data.table::data.table(meshLst[[i]]$ret)
                meshLst[[i]]$ret <- NULL
                thisHash <- cman$tbl[path == meshLst[[i]]$path, hash]
                cman$ugrids[[thisHash]] <- meshLst[[i]]
            }
            ret <- sf::st_as_sf(data.table::rbindlist(polLst))
            return(ret)
        })
        frings <- shiny::reactive({
            selectedCases <- cman$cases[cman$cases %in% input$cases1]
            if (length(selectedCases) < 1) {
                shiny::showNotification("Please select at least a case first!")
                return(NULL)
            }
            tbl <- data.table::copy(cman$tbl)
            hashes <- cman$tbl[caseName %in% selectedCases, hash]
            withPoly <- sapply(hashes, function(x) {if (inherits(cman$ugrids[[x]]$m2D$fRing, "sf")) x else NULL})
            toBuildPoly <- hashes[!hashes %in% withPoly]
            tbl <- data.table::copy(cman$tbl)
            if (length(toBuildPoly) > 0) {
                shiny::withProgress({
                    nCores <- parallel::detectCores()
                    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
                    `%dopar%` <- foreach::`%dopar%`
                    meshes <- foreach::foreach(aH=toBuildPoly, .combine=c) %dopar% {
                        aM <- Ugrid$new(tbl[hash==aH, path], crs=tbl[hash==aH, crsid])
                        aM$buildFace2DPoly()
                        aM$buildEdge1DLine()
                        ret <- list(aM)
                        names(ret) <- aH
                        ret
                    }
                    for (i in seq_along(meshes))
                        cman$ugrids[[names(meshes[i])]] <- meshes[[i]] # meshes[[i]] is given without name.
                }, message="Building domain polygons...")

            }
            polLst <- lapply(hashes, function(x) {
                ring <- cman$ugrids[[x]]$m2D$fRing
                ring$caseName <- tbl[hash == x, caseName]
                ring$label <- basename(ring$path)
                ring$path <- NULL
                ring$hash <- x
                ring
            })
            pol <- do.call(rbind, polLst) |>
                sf::st_cast("MULTIPOLYGON") |>
                sf::st_cast("POLYGON", group_or_split = TRUE)
            lineLst <- lapply(hashes, function(x) {
                line <- cman$ugrids[[x]]$m1D$line1D
                if (!inherits(line, "sf"))
                    return(NULL)
                line$caseName <- tbl[hash == x, caseName]
                line$label <- basename(cman$ugrids[[x]]$path)
                line$hash <- x
                line
            })
            line1D <- do.call(rbind, lineLst)
            if (is.na(sf::st_crs(pol))) {
                pol <- squash2Bbox(pol)
                if (inherits(line1D, "sf"))
                    line1D <- squash2Bbox(line1D)
            } else {
                pol <- sf::st_transform(pol, 4326)
                if (inherits(line1D, "sf"))
                    line1D <- sf::st_transform(line1D, 4326)
            }

            return(list(ring=pol, line1D=line1D))
        })
        shiny::observeEvent(input$genOverview, {
            pol <- frings()$ring
            line1D <- frings()$line1D
            map <- mapgl::maplibre(bounds=pol) |>
                mapgl::add_fill_layer(id="domains", source=pol, tooltip="label",
                                      fill_color="#002B54", fill_opacity=0.5) |>
                mapgl::add_draw_control(download_button=TRUE)
            if (inherits(line1D, "sf"))
                map <- mapgl::add_line_layer(map, id="line1D", source=line1D, line_color="#8E4454", tooltip="label")
            if (inherits(cman$layer, "sf")) {
                lines <- cman$layer[grepl("LINE", cman$layer$ftype), ]
                if (nrow(lines) > 0) {
                    pts <- sf::st_centroid(lines)
                    map <- mapgl::add_line_layer(map, id="lines", source=lines,line_color="#8E4454", tooltip="fname") |>
                        mapgl::add_symbol_layer(source=pts, id="lines_label", text_field=list("get", "fname"))
                }
                dpols <- cman$layer[grepl("POLYGON", cman$layer$ftype), ]
                if (nrow(dpols) > 0) {
                    dpts <- sf::st_centroid(dpols)
                    map <- mapgl::add_fill_layer(map, id="dpols", source=dpols, fill_color="#8E4454",
                                                 tooltip="fname", fill_opacity=1) |>
                        mapgl::add_symbol_layer(source=dpts, id="dpols_label", text_field=list("get", "fname"))
                }

            }
            map2d(map)
            shinyjs::show(id="map2d-card")
            shinyjs::hide(id="map2d-line-plot")
        })
        shiny::observeEvent(input$map2d_drawn_features, {
            mprox <- mapgl::mapboxgl_proxy("map2d")
            feats <- mapgl::get_drawn_features(mprox)
            lines <- feats[grepl("LINESTRING", sf::st_geometry_type(feats)), ]
            lines <- lines[!lines$id %in% cman$layer$id, ]
            if (nrow(lines) > 0) {
                lines$ftype <- sf::st_geometry_type(lines)
                startIdx <- ifelse(is(cman$layer, "sf"), nrow(cman$layer), 0)
                lines$fname <- paste0("Line_", seq_len(nrow(lines)) + startIdx)
                cman$layer <- rbind(cman$layer, lines[, c("id", "ftype", "fname")])
            }
        })
        output$map2d <- mapgl::renderMaplibre({
            map2d()
        })
        output$linePlot <- plotly::renderPlotly({
            shiny::withProgress({
                ncVar1 <- input$ncVar1
                lname <- paste(ncVar1, input$feats, input$cases1, collapse=";") |>
                    digest::digest()
                tbl <- cman$lyrDta[[lname]]
                if (!inherits(tbl, "data.table"))
                    return(NULL)
                caseHashes <- cman$tbl[caseName %in% input$cases1 & hash %in% names(cman$ugrids), hash]
                aM <- cman$ugrids[[caseHashes[1]]]
                vName <- aM$getVarName(ncVar1)
                varTbl <- aM$vars[name == vName]
                varTbl[!is.na(unit), long_name := paste0(long_name, " [", unit, "]")]
                isTime <- varTbl[, hasTime]
                yTitle <- varTbl$long_name
                if ("lyr" %in% colnames(tbl))
                    tbl <- tbl[lyr == input$lyr][, lyr := NULL]
                if (nrow(tbl[!is.na(value)]) < 1) {
                    shiny::showNotification("Data for this layer is all NA!")
                    return(NULL)
                }
                if (input$agg1 != "none") {
                    af <- get(input$agg1)
                    tbl <- tbl[, af(value, na.rm=TRUE), by=sta]
                    setnames(tbl, "V1", "value")
                }
                p <- plotly::plot_ly(data=tbl, type="scatter", mode="lines") |>
                    plotly::layout(xaxis=list(title="Chainage [m]"), yaxis=list(title=yTitle))
                if (input$agg1 != "none" | !isTime) {
                    p <- plotly::add_lines(p, x=~sta, y=~value, name=paste0(yTitle, " (", input$agg1, ")"))
                } else {
                    p <- plotly::add_lines(p, frame=~ts, x=~sta, y=~value, name=yTitle,
                                           hovertext=~paste("faceID: ", faceID, ". Value: ", round(value, 3))) |>
                        plotly::animation_slider(currentvalue=list(prefix="Timestep", visible=TRUE,
                                                                   font=list(color="red"))) |>
                        plotly::animation_opts(easing="bounce-in", frame = 500)
                }
                p
            }, message = "Generating plot...")
        })

        shiny::observeEvent(input$sliceData, {
            selectedCases <- cman$cases[cman$cases %in% input$cases1]
            if (length(selectedCases) != 1) {
                shiny::showNotification("Please select only one case first!")
                return(NULL)
            }
            fRing <- frings()$ring
            if (!sf::st_can_transform(fRing, 4326)) {
                shiny::showNotification("Cannot tranform faces to EPSG:4326!")
                return(NULL)
            }
            lines <- cman$layer[cman$layer$id %in% input$feats, ]
            if (!isTRUE(nrow(lines) > 0)){
                shiny::showNotification("Please select a line for calculation. Did you upload or draw some?")
                return(NULL)
            }
            lineInt <- sf::st_intersects(lines, fRing)
            lineRet <- lapply(seq_along(lineInt), function(i) fRing$hash[lineInt[[i]]]) |> unlist()
            ncVar1 <- input$ncVar1
            # TODO: take more lines
            lineMesh <- lapply(lineRet, function(x) cman$ugrids[[x]])
            shinyjs::hide(id="map2d-card")
            shinyjs::show(id="map2d-line-plot")
            if (length(lineMesh) < 1) {
                shiny::showNotification("No crossed domains found, please check input!")
            } else {
                lname <- paste(ncVar1, input$feats, input$cases1, collapse=";") |>
                    digest::digest()
                if (!inherits(cman$lyrDta[[lname]], "data.table")) {
                    lineMesh <- getFaceData4Var(mesh=lineMesh, variable=ncVar1)
                    aM <- lineMesh[[1]]
                    vName <- aM$getVarName(ncVar1)
                    ncUnit1 <- aM$vars[name == vName, unit]
                    isTime <- aM$vars[name == vName, hasTime]
                    faces <- lapply(seq_along(lineMesh), function(j) {
                        pol <- lineMesh[[j]]$m2D$face2D
                        pol$faceName <- basename(lineMesh[[j]]$path)
                        pol <- pol[pol$faceID %in% lineMesh[[j]]$m2D$fids, ]
                        fidx <- cman$tbl[path %in% lineMesh[[j]]$path, idx]
                        pol$faceID <- fidx * 10^6 + pol$faceID
                        pol
                        })
                    faces <- do.call(rbind, faces)
                    faces <- sf::st_transform(faces, 4326)
                    lineInt <- tryCatch(sf::st_intersects(lines, faces),
                                        error=function(e) sf::st_intersects(lines, sf::st_make_valid(faces))) |>
                        unlist()
                    lineFaces <- faces[lineInt, ]
                    lineSec <- sf::st_intersection(lineFaces, lines)
                    lineSec <- lineSec[, "faceID"]
                    midPts <- sf::st_centroid(lineSec) |> sf::st_as_sfc()
                    distances <- sf::st_line_project(sf::st_as_sfc(lines), midPts)
                    distTbl <- data.table::data.table(d=distances, idx=seq_along(distances))
                    data.table::setorder(distTbl, d)
                    lineSec <- lineSec[distTbl$idx, "faceID"]
                    lineSec$sta <- calcStation(lineSec)
                    lineSecTbl <- sf::st_drop_geometry(lineSec)
                    dta <- lapply(seq_along(lineMesh), function(j) {
                        tbl <- lineMesh[[j]]$data2D$face[[ncVar1]]
                        tDim <- dim(tbl)
                        nDim <- length(tDim)
                        fidx <- cman$tbl[path %in% lineMesh[[j]]$path, idx]
                        fids <- fidx * 10^6 + seq_len(nrow(lineMesh[[j]]$m2D$face2D))
                        if (nDim > 2) {
                            tbl <- matrix(tbl, nrow = tDim[1] * tDim[2], ncol = tDim[3])
                            tbl <- data.table::data.table(tbl)
                            tbl[, lyr := rep(seq_len(tDim[1]), times=tDim[2])]
                            tbl[, faceID := rep(fids, each=tDim[1])]
                        } else if (nDim > 1 & isTime) {
                            tbl <- data.table::data.table(tbl)
                            tbl[, faceID := fids]
                        } else if (nDim > 1 & !isTime) {
                            stop("This kind of variable is not considered!")
                        } else {
                            tbl <- data.table::data.table(value=tbl)
                            tbl[, faceID := fids][, ts := 0L]
                        }
                        if (isTime) {
                            tbl <- data.table::melt(tbl, measure.vars=paste0("V", seq_len(aM$totalTs)),
                                                    variable.name="tsName")
                            tsTbl <- data.table::data.table(tsName=paste0("V", seq_len(aM$totalTs)),
                                                            ts=aM$ts)
                            tbl <- merge(tbl, tsTbl, by="tsName")
                            tbl[, tsName := NULL]
                        }
                        tbl
                    }) |>
                        data.table::rbindlist()
                    ldta <- merge(dta, lineSecTbl, by="faceID")
                    data.table::setorder(ldta, sta, ts)
                    cman$lyrDta[[lname]] <- ldta
                }
                shiny::showNotification("Data for the line was generated!")
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
            ncVars <- ncVars[!ncVars %in% unlist(aM$m2D$topo)]
            ncVars <- data.table::data.table(name=unlist(ncVars), choice=names(ncVars))
            ncVars <- merge(ncVars, aM$vars[, c("name", "long_name")], by="name")
            varChoices <- ncVars$choice
            names(varChoices) <- ncVars$long_name
            if (any(aM$vars$ndims > 2)) {
                shinyjs::show("lyr")
                nLyr <- aM$dims[name == aM$m2D$topo$layer_dimension, as.integer(length)]
                shiny::updateSliderInput(inputId="lyr", max=nLyr)
            }
            shiny::updateSelectInput(inputId="ncVar1", choices=varChoices)
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE, selected=tsIds[2])
            }
            tbl <- rbind(cman$tbl[caseName %in% input$cases1], cman$tbl[!caseName %in% input$cases1])
            ncLst <- shinyWidgets::prepare_choices(tbl, label=ncName,
                                                   value=hash, group_by=caseName, alias=path)
            shinyWidgets::updateVirtualSelect(inputId="ncNames1", choices=ncLst)
            progress$set(value=0.9, message="Done.")
        }, ignoreInit=TRUE)
    })
}
