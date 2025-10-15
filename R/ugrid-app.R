# This file contain the main shiny app for postprocessing of ugrid data
# The main UI contain the setting of the Case

notificationStyle <- ".shiny-notification {
							height: 100px;
							width: 800px;
							position:fixed;
							top: calc(50% - 50px);;
							left: calc(50% - 400px);;
					}
					.selectize-input { word-wrap : break-word;};
					.selectize-input { word-break: break-word;}
					.selectize-dropdown {word-wrap : break-word;}
					 "
# source: https://stackoverflow.com/questions/46559251/how-to-add-multiple-line-breaks-conveniently-in-shiny
nBrks <- function(n){shiny::HTML(rep("<br/>", n))}

#' UI function of main shiny app
#' The structure of the ui and the big-buttons was inspired by this app: https://github.com/voronoys/voronoys_sc
#' @keywords internal
appUi <- function(request) {

    shiny::addResourcePath("www", system.file("app/www", package="ugrid"))
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))

     bslib::page_navbar(
         title = div(img(src="img/bfg-logo.png", height = "50px"),
                     style = "padding-left:150px; padding-right:20px;"),
         id = "navbar",
         window_title = "Ergebnisse des Sobek-Modells",
         selected = "tpHome",
         theme = bslib::bs_theme(bootswatch="cerulean"),
         header=shiny::tagList(
             shinyjs::useShinyjs(),
             tags$head(
                 tags$link(rel="icon", href="favicon.ico"),
                 tags$style(".navbar-header {height: 50px; min-height:25px; padding:0px; margin:0px;}"),
                 tags$style(".navbar-static-top {margin-bottom: 2px; padding:0px;}"),
                 tags$style(HTML(notificationStyle))
             )
         ),
         uiHome(),
         uiSetting(),
         uiResultMap(),
         uiRasterCalculation(),
         uiResultGraphic(),
         uiResultLayer()
        )
}

uiHome <- function() {

    bslib::nav_panel(
        title="Home", value="tpHome",
        h2('Postprocessing tools for UGRID-NetCDF data'),
        bslib::card(
            p("This Shiny app presents the model results using various templates.
              You can select a theme using the large buttons at the bottom or the menu at the top."),
        ),
        nBrks(1),
        bslib::card(
            fluidRow(
                column(2, p()),
                column(3,
                       actionButton(
                           "btnH2Set", label=HTML("<br> Settings"),
                           icon=icon(name="list-alt", "fa-3x"),
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(3,
                       actionButton(
                           inputId = "btnH2Map", label=HTML("<br> Results as maps"),
                           icon=icon(name="chart-area", "fa-3x"),
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(3,
                       actionButton(
                           inputId = "btnH2Rast", label=HTML("<br> Raster calculation"),
                           icon=icon(name="code-branch", "fa-3x"),
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(1, p())
            ),
            nBrks(2),
            fluidRow(
                column(2, p()),
                column(3,
                       actionButton(
                           "btnH2Profile", label=HTML("<br> Long profile"),
                           icon=icon(name="chart-line", "fa-3x"),
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(3,
                       actionButton(
                           inputId = "btnH2Sta", label=HTML("<br> Results at Station"),
                           icon=icon(name="archive", "fa-3x"), disabled = TRUE,
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(3,
                       actionButton(
                           "btnH2Layer", label=HTML("<br> Vertical results"),
                           icon=icon(name="map", "fa-3x"),
                           style='height: 150px; width: 300px; font-size: 20px')),
                column(1, p())
            )
        )
    )
}

uiSetting <- function() {

    sbVer <- shiny::markdown(
        paste0("Package ", "`ugrid`", " Version: *",
               packageVersion("ugrid"), "* - erstellt am: *",
               packageDate("ugrid"), "*")
        )
    bslib::nav_panel(
        title = "Settings", value = "tpSetting",
        sbVer,
        bslib::accordion(
            open="Case management",
            bslib::accordion_panel(
                title="Case management",
                shinyWidgets::prettySwitch("initUgrid", "Make Ugrid NetCDF ready to work with when adding cases!"),
                shiny::h3("Add case"),
                shiny::fluidRow(
                    shiny::column(9, shinyWidgets::textInputIcon(
                        "casePath", "Add a case using format Name=/path/to/folder",
                        value=getOption("ugrid.case"), width="100%")),
                    shiny::column(3, shinyWidgets::virtualSelectInput(
                        "caseCrs", "CRS", choices=crsidChoices, multiple=FALSE,
                        autoSelectFirstOption=FALSE, search=TRUE, hideClearButton=FALSE))
                ),
                shiny::div(class="d-grid gap-2", bslib::input_task_button("addCase", "Add case from path", width="50%")),
                shiny::selectizeInput("cases", "Case(s) to update CRS", choices="", multiple=TRUE, width="100%"),
                shinyWidgets::prettyToggle("keepCrs", value=TRUE, label_off="Ignore CRS in NetCDF files.",
                                           label_on="Keep CRS in NetCDF files."),
                shiny::div(class="d-grid gap-2", bslib::input_task_button(
                    "updateCrs", "Apply selected CRS to selected cases", width="50%")),
                shiny::hr(),
                shiny::selectizeInput("moreCase", "Select cases to work with",
                                      choices=ctbl$caseName, multiple=TRUE, width="100%"),
                shiny::div(class="d-grid gap-2",
                           bslib::input_task_button("addSelectedCase", "Add selected cases", width="50%"))
            ),
            bslib::accordion_panel(
                title="Features of interest",
                shiny::p("You can upload your features of interest from following different formats that are supported by sf packages,
                         or deltares-polyline format (.pli, .pliz).
                         Before uploading the data, please make sure that the CRS information is in the dataset
                         or select one CRS from the list above."),
                shiny::fileInput("featFile", "Upload features of interest", multiple=TRUE)
                ),
            bslib::accordion_panel(
                title="Time setting",
                shinyWidgets::airDatepickerInput ("time", "Time input")
            )
        )
    )
}

uiResultMap <- function() {

    bslib::nav_panel(
        title = "Result as Map", value = "tpMap",
        map2dUi("retMap")
    )
}

uiRasterCalculation <- function() {

    bslib::nav_panel(
        title = "Raster Calculation", value = "tpRaster",
        map2dCalcUi("raster")
    )
}

uiResultGraphic <- function() {

    bslib::nav_panel(
        title = "Result as Graphics", value = "tpLine",
        map2dLineUi("retLine")
    )
}

uiResultLayer <- function() {

    bslib::nav_panel(
        title = "3D-Result", value = "tpLayer",
        map2dLayerUi("retLayer")
    )
}

appServer <- function(input, output, session) {

    tmap::tmap_mode("view")
    # increasing max filesize to upload to 100Mb
    options(shiny.maxRequestSize=100*1024^2)
    options(ugrid.pattern="_map\\.nc$")
    if (!exists("cman")) {
        cman <- initCaseManager()
    }
    shiny::updateSelectizeInput(inputId="addSelectedCase", choices=ctbl$caseName, server=TRUE)
    map2dServer(id="retMap", cman=cman)
    map2dCalcServer(id="raster", cman=cman)
    map2dLineServer(id="retLine", cman=cman)
    map2dLayerServer(id="retLayer", cman=cman)
    observeEvent(input$btnH2Map, {
        bslib::nav_select(id="navbar", selected="tpMap")
    })
    observeEvent(input$btnH2Rast, {
        bslib::nav_select(id="navbar", selected="tpRaster")
    })
    observeEvent(input$btnH2Profile, {
        bslib::nav_select(id="navbar", selected="tpLine")
    })
    observeEvent(input$btnH2Set, {
        bslib::nav_select(id="navbar", selected="tpSetting")
    })
    observeEvent(input$btnH2Layer, {
        bslib::nav_select(id="navbar", selected="tpLayer")
    })
    observeEvent(input$featFile, {
        fInfo <- isolate(input$featFile) |> data.table::as.data.table()
        fInfo[, newPath := paste0(dirname(datapath), "/", name)]
        file.rename(from=fInfo$datapath, to=fInfo$newPath)
        if (nrow(fInfo) > 1) {
            dsnFile <- grep("\\.shp$", fInfo$newPath, value=TRUE, ignore.case = TRUE)
            if (!file.exists(dsnFile)) {
                shiny::showNotification("Multiple files were uploaded, but no .shp files was found!
                                        Uploading multiple files is currently supported for Shapefile only!")
                return(NULL)
            }
        } else {
            dsnFile <- fInfo$newPath[1]
        }
        isPli <- grepl("\\.pli$|\\.pli$", dsnFile, ignore.case=TRUE)
        if (isPli) {
            lyr <- tryCatch(readPli(dsnFile), error=function(e) e)
        } else {
            lyr <- tryCatch(sf::st_read(dsn=dsnFile), error=function(e) e)
        }
        if (inherits(lyr, "error")) {
            shiny::showNotification(paste("Error while reading uploaded file: ", lyr$message))
        } else if (nrow(lyr) > 0) {
            if (!is.na(sf::st_crs(lyr))) {
                lyr <- sf::st_transform(lyr, "EPSG:4326")
            } else {
                if (nchar(input$caseCrs) < 1) {
                    shiny::showNotification("Cannot read CRS from uploaded file,
                                            please choose one from the list above before uploading the data.")
                    return(NULL)
                } else {
                    sf::st_crs(lyr) <- input$caseCrs
                    lyr <- sf::st_transform(lyr, "EPSG:4326")
                }
            }
            startIdx <- ifelse(is(cman$layer, "sf"), nrow(cman$layer) + 1, 1)
            ids <- seq.int(from=startIdx, length.out=nrow(lyr))
            allCols <- colnames(lyr)
            nameColIdx <- grep("name", allCols, ignore.case=TRUE)[1]
            if (!chkInt(nameColIdx))
                lyr$fname <- paste0(basename(dsnFile), "_", ids)
            else
                colnames(lyr)[nameColIdx] <- "fname"
            lyr$id <- paste0(digest::digest(dsnFile), "_", ids)
            lyr$ftype <- sf::st_geometry_type(lyr, by_geometry=TRUE)
            ftypes <- sf::st_geometry_type(lyr, by_geometry=TRUE)
            lyr <- lyr[, c("id", "fname", "ftype")]
            if (is(cman$layer, "sf"))
                lyr <- sf::st_set_geometry(lyr, attr(cman$layer, "sf_column"))
            lyr <- sf::st_zm(lyr, drop=TRUE)
            cman$layer <- rbind(lyr, cman$layer)
        }
    })
    observeEvent(cman$layer, {
        if (is(cman$layer, "sf")) {
            featChoices <- shinyWidgets::prepare_choices(cman$layer, label=fname, value=id, group_by=ftype)
            shinyWidgets::updateVirtualSelect(inputId="retMap-feats", choices=featChoices)
            shinyWidgets::updateVirtualSelect(inputId="raster-feats", choices=featChoices)
            shinyWidgets::updateVirtualSelect(inputId="retLayer-feats", choices=featChoices)
            shinyWidgets::updateVirtualSelect(inputId="retLine-feats", choices=featChoices)
        }
    })
    observeEvent(input$addCase, {
        newCase <- strsplit(input$casePath, split="=", fixed=TRUE)[[1]]
        if (length(newCase) != 2) {
            shiny::showNotification("Check input. Path must be in format: Case name = /path/to/folder")
        } else {
            newCase <- trimws(newCase)
            caseLst <- gsub("Z:", "~/freigaben", newCase[2], ignore.case = TRUE) |> path.expand() |>
                stringi::stri_replace_all_fixed("\\", "/")
            names(caseLst) <- newCase[1]
            thisCrs <- ifelse (chkChr(input$crs), input$crs, NA_character_)
            addCases(caseLst=caseLst, cman=cman, crs=thisCrs, ignoreCrsInFile=!input$keepCrs, initUgrid = input$initUgrid)
        }
    })
    observeEvent(input$addSelectedCase, {
        if (length(input$moreCase) > 0) {
            caseLst <- ctbl[caseName %in% input$moreCase, casePath]
            names(caseLst) <- ctbl[caseName %in% input$moreCase, caseName]
            thisCrs <- ifelse (chkChr(input$crs), input$crs, NA_character_)
            addCases(caseLst=caseLst, cman=cman, crs=thisCrs, ignoreCrsInFile=!input$keepCrs, initUgrid = input$initUgrid)
        }
    })
    observeEvent(input$updateCrs, {
        chk <- isTRUE(all(nchar(input$cases) > 0)) & chkChr(input$caseCrs)
        if (!chk) {
            shiny::showNotification("Please select at least one case and select also CRS.")
        } else {
            cman$tbl[caseName %in% input$cases, crsid := input$caseCrs]
            ncHash <- cman$tbl[caseName %in% input$cases, hash]
            addedHash <- names(cman$ugrids)
            addedHash <- addedHash[addedHash %in% ncHash]
            for (aH in addedHash) {
                if (is.na(cman$ugrids[[aH]]$crs) | !input$keepCrs) {
                    aM <- cman$ugrids[[aH]]
                    aM$crs <- sf::st_crs(input$caseCrs)
                    if (is(aM$m2D$face2D, "sf"))
                        sf::st_crs(aM$m2D$face2D) <- input$caseCrs
                    if (is(aM$m2D$fRing, "sf"))
                        sf::st_crs(aM$m2D$fRing) <- input$caseCrs
                    if (is(aM$m1D$line1D, "sf"))
                        sf::st_crs(cman$ugrids[[aH]]$m1D$line1D) <- input$caseCrs
                    cman$ugrids[[aH]] <- aM
                }
            }
            shiny::showNotification("Done.")
        }
    })
    observeEvent(cman$cases, {
        shiny::updateSelectizeInput(inputId="cases", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="retMap-cases1", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="retMap-cases2", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="raster-cases1", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="raster-cases2", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="retLine-cases1", choices=cman$cases)
        shinyWidgets::updateVirtualSelect(inputId="retLayer-cases1", choices=cman$cases)
    })
}

#' A shiny app for observing ugrid data stored in NetCDF files
#'
#' @export
ugridApp <- function(...) {
    shiny::shinyApp(ui=appUi, server=appServer, ...)
}
