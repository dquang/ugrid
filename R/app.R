# mboxToken <- read.csv("~/mbox.key", header=FALSE)[1, 1]
gen2DMapLibre <- function(
        value, facePol, polColumn, n, style, colPal, colType,
        legendTitle, mapId=polColumn,
        legendId=mapId, legendPos="top-left",
        varClassInt=NULL, asList=FALSE
        ) {
    value <- value[!is.na(value)] |> unique()
    if (length(value) < 1)
        return(NULL)
    if (is.null(varClassInt)) {
        varClassInt <- tryCatch(classInt::classIntervals(var=value, n=n, style=style),
                                error=function(e) NULL)
    }
    if (length(varClassInt) > 0) {
        varColors <- cols4all::c4a(palette=colPal, n=length(varClassInt$brks), type=colType)
        legendValue <- round(varClassInt$brks, 2)
        colExp <- mapgl::interpolate(
            column=polColumn,
            values=varClassInt$brks,
            stops=varColors,
            na_color="grey"
        )
    } else {
        legendTitle <- paste0(legendTitle, " - No value classification!")
        legendValue <- value
        colExp <- "grey"
        varColors <- "grey"
    }

    faceMap <- mapgl::maplibre(bounds=facePol) |>
        mapgl::add_fill_layer(
            id=mapId, source=facePol,
            fill_color=colExp, fill_opacity=1,
            tooltip=polColumn,
            hover_options=list(fill_color="yellow", fill_opacity=1)
        )  |>
        mapgl::add_legend(
            legendTitle,
            values=legendValue, colors=varColors,
            unique_id=legendId,
            width="300px", type="continuous",
            position=legendPos
        )
    if (isTRUE(asList)) {
        ret <- list(
            map=faceMap,
            varClassInt=varClassInt,
            legendTitle=legendTitle,
            legendValue=legendValue,
            legendColors=varColors,
            legendId=legendId,
            legendPos=legendPos)
    } else {
        ret <- faceMap
    }

    return(ret)
}

nokUi <- function() {
    shiny::addResourcePath("www", system.file("app/www", package="ugrid"))
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    tagList(
        shinyjs::useShinyjs(),

        tags$head(
            tags$link(rel="icon", href="img/favicon.ico"),
            tags$style(".navbar-header {height: 50px; min-height:25px; padding:0px; margin:0px;}"),
            tags$style(".navbar-static-top {margin-bottom: 2px; padding:0px;}"),
            tags$style(
                HTML(
                    ".shiny-notification {
							height: 100px;
							width: 800px;
							position:fixed;
							top: calc(50% - 50px);;
							left: calc(50% - 400px);;
					}
					.selectize-input { word-wrap : break-word;};
					.selectize-input { word-break: break-word;}
					.selectize-dropdown {word-wrap : break-word;}
					 ")
            )
        ),
        navbarPage(title = div(img(src="img/bfg-logo.png", height = "50px"),
                               style = "padding-left:150px; padding-right:20px;"),
                   id = "navbar",
                   windowTitle = "NOK-East",
                   selected = "Input",
                   theme = bslib::bs_theme(bootswatch="cerulean",
                                           # primary="#002B54", secondary="#9C9894", success="#7BBA40",
                                           # info="#BCD9F3", warning="#F0CCC4", danger="#592133",
                                           brand=system.file("app/www/brand.yml", package="ugrid")
                                           ),
                   fluid = TRUE,
                   tabPanel(
                       "Input",
                       fluidRow(
                           column(
                               6,
                               shiny::textInput("prj", "Path to the output folder",
                                                value="Z:/M/M2/work/promny/NOK/Work/Modelle/250930_Test",
                                                placeholder="only a path to public folder on Z:",
                                                width="75vh"),
                               shiny::selectizeInput("crsid", "Select CRS", choices="EPSG:31467",
                                                     selected="EPSG:31467", multiple=FALSE),
                               shiny::selectizeInput("ncFiles", "Select _map.nc", choices="",
                                                     options=list(maxItems=1))
                               ),
                           column(
                               6,
                               leaflet::leafletOutput("map2d", height = "80vh")
                               )
                       )
                   ),

# 3D UI -----------------------------------------------------------------------------------------------------------
                   tabPanel(
                       "3D Data",
                       fluidRow(
                           column(3, shiny::selectizeInput("nc3DVar", "Select variable", choices="",
                                                           options=list(maxItems=1))),
                           column(3, shiny::selectizeInput("m3DTs", "Select timesteps (multiple choices possible)",
                                                           choices="", multiple=TRUE)),
                           column(3, shiny::sliderInput("nbins", "Number of contour bins", min=2, max=15, value=5))
                       ),
                       plotOutput("mesh3d", height = "80vh")
                   ),
# 2D UI -----------------------------------------------------------------------------------------------------------
                   tabPanel(
                       "2D Data",
                       fluidRow(
                           column(3, shiny::selectizeInput("nc2DVar", "Select variable", choices="",
                                                           options=list(maxItems=1))),
                           column(3, shiny::selectizeInput("m2DTs", "Select a timestep", choices="",
                                                           options=list(maxItems=1))),
                           column(3, shiny::selectizeInput("m2DTs2", "Select another timestep to compare", choices="",
                                                           options=list(maxItems=1)))
                       ),
                       fluidRow(
                           column(3, shiny::numericInput("ncat", "Number of categories",
                                                         min=1, value=5, max=10)),
                           column(3, shiny::selectInput("classIntStyle", "Classification method",
                                                        choices=c("equal", "quantile", "fisher", "sd",
                                                                  "kmeans", "pretty", "fixed",
                                                                  "hclust", "bclust", "dpih",
                                                                  "headtails", "box"),
                                                        selected="fisher")),
                           column(3, colorPaletteInput("faceColPalette", "Color palette",
                                                        # choices=cols4all::c4a_palettes(type="seq"),
                                                        selected="brewer.pu_rd"))
                       ),
                       fluidRow(
                           column(3, shiny::actionButton("genMap2d", "Generate map...")),
                           column(3, shiny::actionButton("compare", "Compare two maps...")),
                           column(3, shiny::radioButtons("compareMode", "Compare mode",
                                                         choices=c("swipe", "sync"), inline=TRUE))

                       ),
                       mapgl::maplibreOutput("mesh2d", height = "80vh"),
                       mapgl::maplibreCompareOutput("mesh2dCompare", height = "80vh")
                    ),
                    tabPanel("Color palettes", colSelectionUi("colPal"))
        )
    )
}

nokServer <- function(input, output, session) {

    updateSelectizeInput(inputId="crsid", choices=crsidLst, server=TRUE, selected="EPSG:31467")
    zPath <- ifelse(.Platform$OS.type =="unix", "~/freigaben", "Z:")
    nok <- sf::st_read(file.path(zPath, "/M/M2/work/duong/prog/ugrid/dev/vernet_NOK.shp"))
    nok <- sf::st_transform(nok, 4326)
    nok <- sf::st_zm(nok, drop=TRUE)
    meshNc <- reactiveValues()
    lines <- reactiveValues()
    prj <- reactiveVal()
    thisNc <- reactiveVal()
    face2DMap <- reactiveVal()
    # two maps for comparing, have to make two because the classification should include values from both
    face2DMapLeft <- reactiveVal()
    face2DMapRight <- reactiveVal()
    colorInfo <- reactiveVal()
    observeEvent(input$prj, {
        inPrj <- sub("Z:|.*mt[12]*\\.fs\\.bafg\\.de\\fachdaten", zPath, input$prj, ignore.case = TRUE)
        inPrj <- gsub("\\\\", "/", inPrj)
        if (file.exists(inPrj)) {
            prj(inPrj)
            ncLst <- list.files(path=inPrj, pattern="_map.nc$", ignore.case = TRUE, full.names = TRUE)
            names(ncLst) <- basename(ncLst)
            updateSelectizeInput(inputId="ncFiles", choices=ncLst, selected="")
        }
    })
    observeEvent(input$ncFiles, {
        if (file.exists(input$ncFiles)) {
            try({
                mesh <- Ugrid$new(input$ncFiles, newCrs=4326)
                if (is.na(mesh$crs))
                    mesh$crs <- sf::st_crs(input$crsid)
                mesh$buildFace2DPoly()
                meshNc[[input$ncFiles]] <- mesh
                thisNc(input$ncFiles)
                faceDimId <- mesh$dims[name == mesh$m2D$topo$face_dimension, id]
                m3DVars <- mesh$vars[ndims == 3 & (dim1 ==faceDimId | dim2 ==faceDimId | dim3 ==faceDimId)]
                m3DNames <- m3DVars$name
                names(m3DNames) <- m3DVars$long_name
                updateSelectizeInput(inputId="nc3DVar", choices=m3DNames, selected="mesh2d_sa1")
                m2DVars <- mesh$vars[ndims < 3 & (dim1 == faceDimId | dim2 == faceDimId)]
                expPat <- paste0(paste(mesh$m2D$topo$face_coordinates, collapse="|"), "|_Numlimdt$")
                m2DVars <- m2DVars[dim1Name %in% c(mesh$m2D$topo$face_dimension, "time") & !grepl(expPat, name)]
                m2DNames <- m2DVars$name
                names(m2DNames) <- m2DVars$long_name
                updateSelectizeInput(inputId="nc2DVar", choices=m2DNames, selected=m2DNames[2])
                tsId <- seq_along(mesh$ts)
                names(tsId) <- mesh$ts
                for (aId in c("m2DTs2", "m2DTs", "m3DTs"))
                    updateSelectInput(inputId=aId, choices=tsId)
            })
        }
    })
    output$mesh3d <- shiny::renderPlot({
        if ((nchar(input$nc3DVar) > 0) & (length(input$m3DTs) > 0)) {
            tsId <- as.integer(input$m3DTs)
            faces <- findFaces(x=nok, mesh=meshNc[[input$ncFiles]])
            tbl <- lapply(tsId, function(ii) {
                ret <- interpLayer(faces, meshNc[[input$ncFiles]], variable=input$nc3DVar, tsIdx = ii)
                ret[, tsIdx :=  meshNc[[input$ncFiles]][["ts"]][ii] ]
                return(ret)
            })
            tbl2 <- data.table::rbindlist(tbl)
            legendTitle <- paste0(
                meshNc[[input$ncFiles]][["vars"]][name == input$nc3DVar, long_name], " [",
                meshNc[[input$ncFiles]][["atts"]][varName == input$nc3DVar & name=="units", val], "]"
            )
            browser()
            g <-
                ggplot2::ggplot(tbl2[x < 65], ggplot2::aes(x = x, y = y, z=z)) +
                ggplot2::geom_contour_filled(bins=input$nbins) +
                ggplot2::scale_x_reverse() +
                ggplot2::scale_y_continuous(limits=c(-10, 0)) +
                ggplot2::coord_equal() +
                cols4all::scale_fill_discrete_c4a_cat(palette = colorInfo()$palette) +
                ggplot2::labs(x="Distance from Brünsbuttel [km]", y="Depth [m]",
                     fill=legendTitle)+
                ggplot2::facet_wrap(ggplot2::vars(tsIdx), ncol=1) +
                ggplot2::theme_bw(base_size = 14, base_family = "Arial")

            return(g)
        }
    })

    observeEvent(input$genMap2d, {
        if ((nchar(input$nc2DVar) > 0) & (length(input$m2DTs) > 0)) {
            shinyjs::runjs('$(".maplibregl_compare").hide();')
            shinyjs::runjs('$(".maplibregl").show();')
            shinyjs::hide("compareMode")
            tsId <- as.integer(input$m2DTs)
            params <- list(
                tsId=tsId,
                variable=input$nc2DVar,
                ncFiles=input$ncFiles,
                ncat=input$ncat, style=input$classIntStyle,
                colPal=input$faceColPalette
            )
            curMap <- face2DMap()
            if (!inherits(curMap$map, "maplibregl")| !identical(curMap$params, params)) {
                tbl <- meshNc[[input$ncFiles]]$getData4Face2D(variable=input$nc2DVar)
                pol <- meshNc[[input$ncFiles]]$m2D$face2D
                if (length(dim(tbl)) > 1)
                    tbl <- tbl[, tsId]
                pol[[input$nc2DVar]] <- round(tbl, 3)
                legendTitle <- paste0(
                    meshNc[[input$ncFiles]][["vars"]][name == input$nc2DVar, long_name], " [",
                    meshNc[[input$ncFiles]][["atts"]][varName == input$nc2DVar & name=="units", val], "]"
                )
                faceMap <- gen2DMapLibre(
                            value=tbl, facePol=pol, polColumn=input$nc2DVar,
                            n=input$ncat, style=input$classIntStyle,
                            colPal=input$faceColPalette, colType=colorInfo()$type,
                            legendTitle=legendTitle, legendId=paste0(input$nc2DVar, "_", tsId)
                        )
                face2DMap(list(map=faceMap, params=params))
            }
        }
    })
    observeEvent(input$compare, {
        chk <- (nchar(input$nc2DVar) > 0) & (length(input$m2DTs) > 0) & (length(input$m2DTs2) > 0)
        if (chk) {
            shinyjs::runjs('$(".maplibregl_compare").show();')
            shinyjs::runjs('$(".maplibregl").hide();')
            shinyjs::show("compareMode")
            tsId1 <- as.integer(input$m2DTs)
            params1 <- list(
                tsId=tsId1,
                variable=input$nc2DVar,
                ncFiles=input$ncFiles,
                ncat=input$ncat, style=input$classIntStyle,
                colPal=input$faceColPalette
            )
            tsId2 <- as.integer(input$m2DTs2)
            params2 <- list(
                tsId=tsId2,
                variable=input$nc2DVar,
                ncFiles=input$ncFiles,
                ncat=input$ncat, style=input$classIntStyle,
                colPal=input$faceColPalette
            )
            curMap1 <- face2DMapLeft()
            curMap2 <- face2DMapRight()
            tbl <- meshNc[[input$ncFiles]]$getData4Face2D(variable=input$nc2DVar)
            tbl <- round(tbl, 3)
            if (length(dim(tbl)) > 1) {
                tbl1 <- tbl[, tsId1]
                tbl2 <- tbl[, tsId2]
                tbl <- as.vector(tbl[, c(tsId1, tsId2)])
            } else {
                tbl1 <- tbl2 <- tbl
            }
            chkMap <- (!inherits(curMap1$mapObj$map, "maplibregl") | !inherits(curMap2$mapObj$map, "maplibregl")) |
                !identical(curMap1$params, params1) | !identical(curMap2$params, params2)
            if (chkMap) {
                pol1 <- meshNc[[input$ncFiles]]$m2D$face2D
                pol2 <- pol1
                pol1[[input$nc2DVar]] <- tbl1
                legendTitle1 <- paste0(
                    meshNc[[input$ncFiles]][["vars"]][name == input$nc2DVar, long_name], " [",
                    meshNc[[input$ncFiles]][["atts"]][varName == input$nc2DVar & name=="units", val], "]. Timestep: ",
                    meshNc[[input$ncFiles]]$ts[tsId1]
                )
                faceMap1 <- gen2DMapLibre(
                    value=tbl, facePol=pol1, polColumn=input$nc2DVar,
                    n=input$ncat, style=input$classIntStyle,
                    colPal=input$faceColPalette, colType=colorInfo()$type,
                    legendTitle=legendTitle1, legendId=paste0(input$nc2DVar, "_", tsId1),
                    asList=TRUE
                )
                face2DMapLeft(list(mapObj=faceMap1, params=params1))
                pol2[[input$nc2DVar]] <- tbl2
                legendTitle2 <- paste0(
                    meshNc[[input$ncFiles]][["vars"]][name == input$nc2DVar, long_name], " [",
                    meshNc[[input$ncFiles]][["atts"]][varName == input$nc2DVar & name=="units", val], "]. Timestep: ",
                    meshNc[[input$ncFiles]]$ts[tsId2]
                )
                faceMap2 <- gen2DMapLibre(
                    value=tbl, facePol=pol2, polColumn=input$nc2DVar,
                    n=input$ncat, style=input$classIntStyle,
                    varClassInt=faceMap1$varClassInt,
                    colPal=input$faceColPalette, colType=colorInfo()$type,
                    legendTitle=legendTitle2,
                    legendPos="top-right", asList=TRUE,
                    mapId=paste0(input$nc2DVar, "_cmp")
                )
                face2DMapRight(list(mapObj=faceMap2, params=params2))
            }
        } else {
            shiny::showNotification("Check parameters!")
        }
    })
    output$mesh2d <- shiny::withProgress(
        mapgl::renderMaplibre({face2DMap()$map}),
        message="Generating map..."
    )
    output$mesh2dCompare <-
        mapgl::renderMaplibreCompare({

            map1 <- face2DMapLeft()$mapObj$map
            map2 <- face2DMapRight()$mapObj$map
            if (!inherits(map1, "maplibregl") | !inherits(map2, "maplibregl"))
                return(NULL)
            mapgl::compare(map1, map2, mode=input$compareMode)
            })
    callModule(colSelectionServer, "colPal", result=colorInfo)
    observeEvent(
        colorInfo(),
        {
            updateSelectizeInput(inputId="faceColPalette", selected=colorInfo()$palette,
                                 choices=cols4all::c4a_palettes(type=colorInfo()$type,
                                                                series=colorInfo()$series))
        },
        ignoreNULL = TRUE, ignoreInit = TRUE
        )

}

# App tour --------------------------------------------------------------------------------------------------------


# guide <- cicerone::Cicerone$
#     new()$
#     step(
#         el = "text_inputId",
#         title = "Text Input",
#         description = "This is where you enter the text you want to print."
#     )$
#     step(
#         "submit_inputId",
#         "Send the Text",
#         "Send the text to the server for printing"
#     )
#' @export
nokApp <- function() {
    shiny::shinyApp(ui=nokUi, server=nokServer)
}

