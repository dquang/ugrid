#' Shiny module for
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
                    shiny::p("To update the values for variables and time step, please select a case below."),
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
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select case(s)",
                                                     choices="", multiple=TRUE, autoSelectFirstOption=TRUE),
                    shinyWidgets::virtualSelectInput(ns("cases2"), "Select a reference case",
                                                     choices="", multiple=FALSE, autoSelectFirstOption=FALSE),
                    bslib::input_task_button(ns("findDomains"), "Plot data for the line!"),
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

        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="map2d-card")
        shinyjs::hide(id="map2d-line-plot")
        map2d <- shiny::reactiveVal()
        linePlot <- shiny::reactiveVal()
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
            if (is.na(sf::st_crs(pol)))
                pol <- squash2Bbox(pol)
            else
                pol <- sf::st_transform(pol, 4326)
            return(pol)
        })
        shiny::observeEvent(input$genOverview, {
            pol <- frings()
            map <- mapgl::maplibre(bounds=pol) |>
                mapgl::add_fill_layer(id="domains", source=pol, tooltip="label",
                                      hover_options=list(fill_color="yellow", fill_opacity=1),
                                      fill_color="caseName", fill_opacity=0.5) |>
                mapgl::add_draw_control(
                    download_button=TRUE, source="domains")
            map2d(map)
            shinyjs::show(id="map2d-card")
            shinyjs::hide(id="map2d-line-plot")
        })

        output$map2d <- mapgl::renderMaplibre({
            map2d()
        })
        output$linePlot <- plotly::renderPlotly({
            linePlot()
        })
        shiny::observeEvent(input$tsIdxAni, {
            ncNames1 <- input$ncNames1[nchar(input$ncNames1) > 0]
            selectedPath <- cman$tbl[hash %in% ncNames1, path]
            thisHash <- digest::digest(c(sort(selectedPath), input$ncVar1))
            if (!thisHash %in% names(rasters))
                return(NULL)
            legendTitle <- shiny::HTML(
                paste0(rasters[[thisHash]]$legendTitle, rasters[[thisHash]]$ts[input$tsIdxAni], ")"))
            ras <- terra::rast(rasters[[thisHash]]$files[input$tsIdxAni])
            tm <- genRasterTmap(ras, colScale=rasters[[thisHash]]$colScale, legendTitle=legendTitle)
            map2d(tm)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$findDomains, {
            selectedCases <- cman$cases[cman$cases %in% input$cases1]
            if (length(selectedCases) != 1) {
                shiny::showNotification("Please select only one case first!")
                return(NULL)
            }
            fRing <- frings()
            mprox <- mapgl::mapboxgl_proxy("map2d")
            feats <- mapgl::get_drawn_features(mprox)
            feats <- feats[is.na(feats$hash), ]
            if (nrow(feats) < 1)
                return(NULL)
            geomType <- sf::st_geometry_type(feats)
            lines <- feats[grepl("LINESTRING", geomType, fixed=TRUE), ]
            pols <- feats[grepl("POLYGON", geomType, fixed=TRUE), ]
            lineInt <- sf::st_intersects(lines, fRing)
            polsOverlap <- sf::st_overlaps(pols, fRing)
            lineRet <- lapply(seq_along(lineInt), function(i) fRing$hash[lineInt[[i]]])
            polRet <- lapply(seq_along(polsOverlap), function(i) fRing$hash[polsOverlap[[i]]])
            ncVar1 <- input$ncVar1
            tsIdx1 <- input$tsIdx1
            agg1 <- input$agg1
            # TODO: take more lines
            i <- 1
            lineMesh <- lapply(lineRet[[i]], function(x) cman$ugrids[[x]])
            if (length(lineMesh) > 0) {
                lineMesh <- getFaceData4Var(mesh=lineMesh, variable=ncVar1)
                lineDta <- lapply(seq_along(lineMesh), function(j) {
                    ret <- data.table::data.table(lineMesh[[j]]$data2D$face[[ncVar1]])
                    ret$faceID <- j * 10^6 + 1:nrow(ret)
                    ret
                    })
                lineDta <- data.table::rbindlist(lineDta)
                lineFaces <- lapply(seq_along(lineMesh), function(j) {
                    ret <- lineMesh[[j]]$m2D$face2D
                    ret$faceID <- j * 10^6 + ret$faceID
                    ret[lineMesh[[j]]$m2D$fids, ]
                })
                lineFaces <- do.call(rbind, lineFaces)
                if (is.na(sf::st_crs(lineFaces)))
                    lineFaces <- squash2Bbox(lineFaces)
                else
                    lineFaces <- sf::st_transform(lineFaces, 4326)
                thisInt <- sf::st_intersects(lines[i, ], lineFaces) |> unlist()
                thisIntersection <- sf::st_intersection(lines[i, ], lineFaces[thisInt, ])
                secLength <- sf::st_length(thisIntersection)
                lenSecLength <- length(secLength)
                secSta <- (secLength[-lenSecLength] + secLength[-1]) / 2
                secSta <- c(0, secSta)
                secSta <- cumsum(secSta)
                fids <- lineFaces$faceID[thisInt]
                thisDta <- lineDta[faceID %in% fids]
                colnames(thisDta) <- c(format(lineMesh[[1]]$ts, format="%d.%m.%y %H:%M"), "faceID")
                thisDta$sta <- secSta
                thisDta
                dta <- melt(thisDta[, -c("faceID")], id.vars="sta")
                p <- plotly::plot_ly(data=dta, mode="lines", type="scatter",
                                     x=~sta, y=~value, frame=~variable,
                                     name=ncVar1, showlegend=FALSE) |>
                    plotly::layout(
                        xaxis=list(title="Lage (m)"), yaxis=list(title=ncVar1)
                    ) |>
                    plotly::animation_opts(easing='bounce-in')
                shinyjs::hide(id="map2d-card")
                shinyjs::show(id="map2d-line-plot")
                linePlot(p)
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
                shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE, selected=tsIds[2])
            }
            tbl <- rbind(cman$tbl[caseName %in% input$cases1], cman$tbl[!caseName %in% input$cases1])
            ncLst <- shinyWidgets::prepare_choices(tbl, label=ncName,
                                                   value=hash, group_by=caseName, alias=path)
            shinyWidgets::updateVirtualSelect(inputId="ncNames1", choices=ncLst)
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

# testUi <- function(request) {
#     shiny::addResourcePath("www", system.file("app/www", package="ugrid"))
#     shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
#
#     bslib::page_navbar(
#         title = div(img(src="img/bfg-logo.png", height = "50px"),
#                     style = "padding-left:150px; padding-right:20px;"),
#         id = "navbar",
#         window_title = "Ergebnisse des Sobek-Modells",
#         selected = "tpLine",
#         theme = bslib::bs_theme(bootswatch="cerulean"),
#         header=shiny::tagList(
#             shinyjs::useShinyjs(),
#             tags$head(
#                 tags$link(rel="icon", href="favicon.ico"),
#                 tags$style(".navbar-header {height: 50px; min-height:25px; padding:0px; margin:0px;}"),
#                 tags$style(".navbar-static-top {margin-bottom: 2px; padding:0px;}"),
#                 tags$style(
#                     HTML(notificationStyle)
#                 )
#             )
#         ),
#         bslib::nav_panel(
#             title = "Results as Graphics", value = "tpLine",
#             map2dLineUi("retLine")
#         )
#     )
# }
# testServer <- function(input, output, session) {
#     if (!exists("cman")) {
#         cman <- initCaseManager()
#     }
#     observeEvent(cman$cases, {
#         shinyWidgets::updateVirtualSelect(inputId="retLine-cases1", choices=cman$cases)
#     })
#     map2dLineServer("retLine", cman=cman)
# }
# shiny::shinyApp(ui=testUi, server=testServer, options=list(launch.browser=TRUE))
