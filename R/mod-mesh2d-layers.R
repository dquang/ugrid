#' Shiny module for output as rasters.
map2dLayerUi <- function(id) {

    ns <- shiny::NS(id)
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    tmpFolder <- tempdir()
    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("map2
                  d-sb"), width="25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::p("To update the values for variables and time step, please select a case below."),
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select case(s)",
                                                     choices="", multiple=TRUE, autoSelectFirstOption=TRUE),
                    shiny::hr(),
                    shiny::h4("First raster parameters"),
                    shiny::selectInput(ns("ncVar1"), "Variable", choices=""),
                    shiny::selectizeInput(ns("tsIdx1"), "Time step", choices="", multiple = TRUE),
                    shiny::radioButtons(ns("agg1"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(
                        ns("ncNames1"), "NetCDF files / domains", choices="", multiple=TRUE, search=TRUE),
                    bslib::tooltip(
                        shinyWidgets::virtualSelectInput(
                            ns("feats"), "Features of Interest (to select touching domains)",
                            choices=NULL, multiple=FALSE, search=TRUE),
                        "To select domains from all project that are touched or intersected with given features.
                        Please select the features from the list"),
                    bslib::input_task_button(ns("slideData"), "Slice data for the line!"),
                    shiny::hr()
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
                    shiny::textInput(ns("breaks"), "Manual classification",
                                     placeholder="numeric values seperated by spaces"),
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
                                          choices=cols4all::c4a_series(type="cat", as.data.frame = F),
                                          selected=c("brewer", "cols4all", "matplotlib", "tableau"))
                )
            )
        ),
        bslib::accordion(
            open=c("Data source"), multiple=FALSE,
            bslib::accordion_panel(
                title="Graphics", icon=shiny::icon("chart-area"),
                shiny::fluidRow(
                    shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                        id=ns("genOverview"), label="Generate domain overview", width='100%'))),
                    shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                        id=ns("genPlot"), label="Plot data for selected time steps", width='100%'))),
                    shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                        id=ns("genAnimation"), label="Generate animation for all steps", width='100%')))
                ),
                bslib::card(
                    height="75vh", full_screen=TRUE, id=ns("map2d-card"),
                    mapgl::maplibreOutput(ns("map2d"), height="550px")
                ),
                bslib::card(
                    height="75vh", full_screen=TRUE, id=ns("map2d-line-plot"),
                    shiny::plotOutput(ns("layerPlot"), height="600px")
                )
            )
        )
    )
}

map2dLayerServer <- function(id, cman) {

    shiny::moduleServer(id=id, function(input, output, session) {

        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="map2d-card")
        shinyjs::hide(id="map2d-line-plot")
        map2d <- shiny::reactiveVal()
        layerPlot <- shiny::reactiveVal()
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
            nDomains <- length(toBuildPoly)
            if (nDomains > 0) {
                lapply(toBuildPoly, function(aH) {
                    aM <- addUgrid(tbl[hash==aH, path], cman)
                    aM$buildFace2DPoly()
                })
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
            lines <- cman$layer[grepl("LINESTRING", cman$layer$ftype), ]
            pts <- sf::st_centroid(lines)
            dpols <- cman$layer[grepl("POLYGON", cman$layer$ftype), ]
            map <- mapgl::maplibre(bounds=pol) |>
                mapgl::add_fill_layer(id="domains", source=pol, tooltip="label",
                                      fill_color="#002B54", fill_opacity=0.5
                                      ) |>
                mapgl::add_line_layer(id="lines", source=lines,
                                      line_color="#8E4454", tooltip="fname") |>
                mapgl::add_symbol_layer(source=pts, id="lines_label", text_field=list("get", "fname")) |>
                mapgl::add_draw_control(download_button=TRUE)
            if (nrow(dpols) > 0) {
                dpts <- sf::st_centroid(dpols)
                map <- mapgl::add_fill_layer(map, id="dpols", source=dpols, fill_color="#8E4454",
                                             tooltip="fname", fill_opacity=1) |>
                    mapgl::add_symbol_layer(source=dpts, id="dpols_label", text_field=list("get", "fname"))
            }
            map2d(map)
            shinyjs::show(id="map2d-card")
            shinyjs::hide(id="map2d-line-plot")
        })

        output$map2d <- mapgl::renderMaplibre({
            map2d()
        })
        output$layerPlot <- shiny::renderPlot({
            layerPlot()
        })
        shiny::observeEvent(input$map2d_drawn_features, {
            mprox <- mapgl::mapboxgl_proxy("map2d")
            feats <- mapgl::get_drawn_features(mprox)
            lines <- feats[is.na(feats$caseName), ]
            lines <- lines[grepl("LINESTRING", sf::st_geometry_type(lines)), ]
            lines <- lines[!lines$id %in% cman$layer$id, ]
            if (nrow(lines) > 0) {
                lines$ftype <- sf::st_geometry_type(lines)
                startIdx <- ifelse(is(cman$layer, "sf"), nrow(cman$layer), 0) + 1
                lines$fname <- paste0("Line_", seq_len(nrow(lines)) + startIdx)
                lines$id <- seq_len(nrow(lines)) + startIdx
                cman$layer <- rbind(cman$layer, lines[, c("id", "ftype", "fname")])
                featChoices <- shinyWidgets::prepare_choices(cman$layer, label=fname, value=id, group_by=ftype)
                shinyWidgets::updateVirtualSelect(inputId="feats", choices=featChoices, selected=input$feats)
            }
        })
        shiny::observeEvent(input$slideData, {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            selectedCases <- cman$cases[cman$cases %in% input$cases1]
            if (length(selectedCases) != 1) {
                shiny::showNotification("Please select only one case first!")
                return(NULL)
            }
            lines <- cman$layer[cman$layer$id %in% input$feats, ]
            if (!isTRUE(nrow(lines) > 0)){
                shiny::showNotification("Please select a line for calculation. Did you upload or draw some?")
                return(NULL)
            }
            progress$set(value=0.3, message="Getting domain polygons...")
            fRing <- frings()
            progress$set(value=0.4, message="Calculating intersections...")
            lineInt <- sf::st_intersects(lines, fRing)
            lineRet <- lapply(seq_along(lineInt), function(i) fRing$hash[lineInt[[i]]]) |> unlist()
            ncVar1 <- input$ncVar1
            # TODO: take more lines
            lineMesh <- lapply(lineRet, function(x) cman$ugrids[[x]])
            if (length(lineMesh) > 0) {
                lname <- paste(ncVar1, input$feats, input$cases1, collapse=";") |>
                    digest::digest()
                if (!inherits(cman$lyrInt[[lname]], "data.table")) {
                    progress$set(value=0.6, message="Reading data...")
                    lineMesh <- getFaceData4Var(mesh=lineMesh, variable=ncVar1)
                    dta <- sapply(lineMesh, function(x) as.vector(x$data2D$face[[ncVar1]])) |> unlist()
                    aM <- lineMesh[[1]]
                    vName <- aM$getVarName(ncVar1)
                    ncUnit1 <- aM$atts[varName == vName & grepl("unit", name), val]
                    mDim <- dim(aM$data2D$face[[ncVar1]])
                    mDim[2] <- as.integer(length(dta) / mDim[1] / mDim[3])
                    dta <- array(dta, dim=mDim)
                    faces <- lapply(seq_along(lineMesh), function(j) lineMesh[[j]]$m2D$face2D)
                    faces <- do.call(rbind, faces)
                    if (is.na(sf::st_crs(faces)))
                        faces <- squash2Bbox(faces)
                    else
                        faces <- sf::st_transform(faces, 4326)
                    lineInt <- sf::st_intersects(lines, faces) |> unlist()
                    lineFaces <- faces[lineInt, ]
                    lineDta <- dta[, lineInt, ]
                    station <- calcStation(line=lines, pol=lineFaces)
                    lineFaces$km <- station
                    depth <- aM$getData4Any(variable=aM$m2D$layer$altitude, topo="m2D", at="layer")
                    tsName <- aM$ts
                    progress$set(value=0.7, message="Interpolating data...")
                    ldta <- calcIsolineData(depth=depth, station=station, dta=lineDta, tsName=tsName)
                    cman$lyrInt[[lname]] <- ldta
                    map <- mapgl::maplibre(bounds=lines) |>
                        mapgl::add_fill_layer(source=lineFaces, id="faces", tooltip = "km",
                                              fill_color="grey", fill_opacity=0.5,
                                              hover_options = list(fill_color="blue", fill_opacity=1)) |>
                        mapgl::add_line_layer(source=lines, id="line")
                    map2d(map)
                    shinyjs::show(id="map2d-card")
                    shinyjs::hide(id="map2d-line-plot")
                }
                progress$set(value=0.9, message="Data for the line was generated!")
            }
        })
        shiny::observeEvent(input$genPlot, {
            ncVar1 <- input$ncVar1
            lname <- paste(ncVar1, input$feats, input$cases1, collapse=";") |>
                digest::digest()
            ldta <- cman$lyrInt[[lname]]
            if (!data.table::is.data.table(ldta)) {
                shiny::showNotification("No data found. Please generate data for the selected parameters first!")
                return(NULL)
            }
            caseHashes <- cman$tbl[caseName %in% input$cases1 & hash %in% names(cman$ugrids), hash]
            aM <- cman$ugrids[[caseHashes[1]]]
            vName <-  aM$getVarName(ncVar1)
            ncUnit1 <- aM$atts[varName == vName & grepl("unit", name), val]
            tsName <- aM$ts
            legendTitle <- paste0(ncVar1, " [", ncUnit1,"]")
            shinyjs::hide(id="map2d-card")
            shinyjs::show(id="map2d-line-plot")
            g <- genContourFacets(
                tbl=ldta, tsIds=tsName[as.integer(input$tsIdx1)],
                nClass=input$nClass, style=input$clsStyle, colPal=input$colPal,
                fixedClass=input$breaks, legendTitle=legendTitle
            )
            layerPlot(g)
        })
        shiny::observeEvent(input$cases1, {
            caseHash <- cman$tbl[caseName %in% input$cases1, hash]
            if (length(caseHash) < 1)
                return(NULL)
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading general information for the project.")
            if (!"b33c21d09941aa9213cb046a10b7ce68" %in% cman$layer$id) {
                nokAchse <- sf::st_read(dsn=system.file("geodata.gpkg", package="ugrid"), layer="NOK_river_axis") |>
                    sf::st_set_geometry("geometry") |> sf::st_zm(drop=TRUE)
                cman$layer <- rbind(cman$layer, nokAchse)
            }
            addedHash <- names(cman$ugrids)
            intHash <- intersect(addedHash, caseHash)
            sampleHash <- if (length(intHash) > 0) intHash[1] else caseHash[1]
            aM <- addUgrid(path=cman$tbl[hash == sampleHash, path], cman=cman)
            ncVars <- aM$m2D$layer
            ncVars <- ncVars[!ncVars %in% unlist(aM$m2D$topo)]
            ncVars <- ncVars[ncVars %in% aM$vars[hasTime==TRUE, name]]
            if (length(ncVars) < 1) {
                shiny::showNotification("Found no variables for layers in case: ", input$cases1, type="error")
                shinyjs::hide("lyr")
                return(NULL)
            }
            shiny::updateSelectInput(inputId="ncVar1", choices=names(ncVars))
            shinyjs::show("lyr")
            shiny::updateSliderInput(inputId="lyr", max=aM$dims[name == aM$m2D$topo$layer_dimension, length])
            if (length(aM$totalTs) > 0) {
                tsIds <- seq.int(1, aM$totalTs, 1)
                names(tsIds) <- aM$ts
                shiny::updateSelectizeInput(inputId="tsIdx1", choices=tsIds, server=TRUE, selected=tsIds[2])
                shiny::updateSliderInput(inputId="tsIdxAni", max=length(tsIds))
            }
            tbl <- rbind(cman$tbl[caseName %in% input$cases1], cman$tbl[!caseName %in% input$cases1])
            ncLst <- shinyWidgets::prepare_choices(tbl, label=ncName,
                                                   value=hash, group_by=caseName, alias=path)
            shinyWidgets::updateVirtualSelect(inputId="ncNames1", choices=ncLst)
            progress$set(value=0.9, message="Done.")
        }, ignoreInit=TRUE)
    })
}
