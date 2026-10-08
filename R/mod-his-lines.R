#' Shiny module for
his2dLineUi <- function(id) {

    ns <- shiny::NS(id)
    shiny::addResourcePath("img", system.file("app/www/img", package="ugrid"))
    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("his2d-sb"), width="25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::h4("First raster parameters"),
                    shiny::p("To update the values for variables and time step, please select a case below."),
                    shinyWidgets::virtualSelectInput(ns("cases1"), "Select case(s)", updateOn="close", search=TRUE,
                                                     choices=character(0), multiple=TRUE, autoSelectFirstOption=TRUE),
                    shinyWidgets::virtualSelectInput(ns("locations"), "Select location(s)",
                                                     updateOn="close", search=TRUE, position="top left",
                                                     choices=character(0), multiple=TRUE, autoSelectFirstOption=FALSE),
                    shinyWidgets::virtualSelectInput(ns("y1Var"), "Variable(s) for the left axis", choices=character(0),
                                                     multiple=TRUE, search=TRUE, position="top left"),
                    shinyWidgets::virtualSelectInput(ns("y2Var"), "Variable(s) for the right axis", choices=character(0),
                                                     multiple=TRUE, search=TRUE, position="top left"),
                    shinyWidgets::virtualSelectInput(ns("y3Var"), "Variable(s) for the subplot left axis", search=TRUE,
                                                     position="top left", choices=character(0), multiple=TRUE, disabled=TRUE),
                    shinyWidgets::virtualSelectInput(ns("y4Var"), "Variable(s) for the subplot right axis", position="top left",
                                              choices=character(0), multiple=TRUE, disabled=TRUE, search=TRUE),
                    shinyWidgets::virtualSelectInput(ns("cases2"), "Select a reference case",
                                                     search=TRUE, choices=character(0), disabled=TRUE, position="top left",
                                                     autoSelectFirstOption=FALSE),
                    bslib::input_task_button(ns("readData"), "Read data")
                ),
                bslib::accordion_panel(
                    title="Export data", icon=shiny::icon("file-export"),
                    shiny::actionButton(ns("prepDl"), "Prepare data for downloading...", disabled=TRUE),
                    shiny::downloadButton(ns("dlMap"), "Download map data!", disabled=TRUE)
                )
            )
        ),
        # shiny::fluidRow(
        #     shiny::column(6, shiny::div(class="d-grid gap-2", bslib::input_task_button(
        #         id=ns("genOverview"), label="Generate domain overview", width='100%')))
        # ),
        bslib::accordion(
            open=c("Data source"), multiple=TRUE,
            bslib::accordion_panel(
                title="Plot", icon=shiny::icon("chart-line"),
                bslib::card(
                    height="75vh", full_screen=TRUE, id=ns("his2d-line-plot"),
                    plotly::plotlyOutput(ns("linePlot"), height="600px")
                )
            ),
            bslib::accordion_panel(
                title="Plot settings", icon=shiny::icon("gears"),
                bslib::card(
                    height="75vh", full_screen=TRUE, id=ns("his2d-line-plot-setting")
                )
            )
        )
    )
}

his2dLineServer <- function(id, cman) {

    shiny::moduleServer(id=id, function(input, output, session) {


        shinyjs::hide(id="dlMap")
        shinyjs::hide(id="his2d-line-plot")
        varTbl <- reactiveVal()
        idTbl <- reactiveVal()
        lineDta <- reactiveVal()
        # shiny::observeEvent(cman$cases, {
        #     shinyWidgets::updateVirtualSelect(inputId="cases1", choices=cman$cases)
        #     shinyWidgets::updateVirtualSelect(inputId="cases2", choices=cman$cases)
        # })
        output$linePlot <- plotly::renderPlotly({
            y1Var <- input$y1Var
            y2Var <- input$y2Var
            y3Var <- input$y3Var
            y4Var <- input$y4Var
            locations <- input$locations
            htbl <- cman$htbl
            cases1 <- input$cases1[input$cases1 %in% htbl$caseName]
            variables <- c(y1Var, y2Var, y3Var, y4Var)
            pChk <- (length(locations) > 0) & (length(cases1) > 0) & (length(y1Var) > 0)
            if (!pChk) {
                shiny::showNotification("Please check if cases, variables, and locations were selected!")
                return(NULL)
            }
            tbls <- getVarLoc(varTbl=varTbl(), idTbl=idTbl(), variables=variables, locations=locations)
            allUnits <- tbls$vTbl[, unique(unit)]
            if (length(allUnits) > 4) {
                msg <- paste("Only support maximal 4 type of units. And you have: ", paste(allUnits, collapse=", "),
                             ". Please remove some variables or locations.")
                shiny::showNotification(msg)
                return(NULL)
            }
            dta <- lineDta()
            y1Unit <- tbls$vTbl[name %in% y1Var, unit[1]]
            y2Unit <- tbls$vTbl[name %in% y2Var, unit[1]]
            y3Unit <- tbls$vTbl[name %in% y3Var, unit[1]]
            y4Unit <- tbls$vTbl[name %in% y4Var, unit[1]]
            y1Var <- tbls$vTbl[unit %in% y1Unit & name %in% y1Var, unique(name)]
            y2Var <- tbls$vTbl[unit %in% y2Unit & name %in% y2Var, unique(name)]
            y3Var <- tbls$vTbl[unit %in% y3Unit & name %in% y3Var, unique(name)]
            y4Var <- tbls$vTbl[unit %in% y4Unit & name %in% y4Var, unique(name)]
            y1Range <- range(dta[variable %in% y1Var, value], na.rm=TRUE)
            y1Length <- y1Range[2] - y1Range[1]
            y1Pretty <- pretty(y1Range)
            fontSize <- 14
            fontFamily <- "Arial"
            tsRange <- dta[, range(ts)]
            y1Name <- tbls$vTbl[name %in% y1Var, label[1]]
            y2Name <- tbls$vTbl[name %in% y2Var, label[1]]
            ay1 <- list(
                range=y1Pretty,
                tickfont=list(size=fontSize, family=fontFamily),
                dtick=y1Pretty[2] - y1Pretty[1],
                tick0=y1Pretty[1],
                title=list(text=y1Name, font=list(size=fontSize, family=fontFamily))
            )
            ay2 <- list(
                showticklabels=TRUE,
                tickfont=list(size=fontSize, family=fontFamily),
                overlaying="y",
                side="right",
                showgrid=TRUE,
                ticklabelposition="right"
            )
            ax <- list(title=list(text="Zeit", font=list(size=fontSize, family=fontFamily)),
                       tickfont=list(size=fontSize, family=fontFamily),
                       nticks=20, range=tsRange)
            y2 <- length(y2Var) > 0

            if (y2) {
                y1Dtick <- y1Pretty[2] - y1Pretty[1]
                y2Range <- range(dta[variable %in% y2Var, value], na.rm=TRUE)
                ayLst <- createAxisLayout(y1Range, y2Range, nTick=5,
                                          fontSize=fontSize, fontFamily=fontFamily,
                                          y1Name=y1Name, y2Name=y2Name)
                ay1 <- ayLst$ay1
                ay2 <- ayLst$ay2
            } else {
                y2Range <- y1Range
                y2Length <- y1Length
                y2Scale <- 1
                y2Pretty <- y1Pretty
            }
            plotMode <- "lines"
            colorVals <- cols4all::c4a(palette="cols4all.line9", n=length(variables) * length(locations),
                                       nm_invalid="interpolate")

            p0 <- plotly::plot_ly(colors=colorVals,
                                  hoverinfo="text+x",
                                  mode=plotMode, type="scatter") |>
                plotly::layout(hovermode="x unified", xaxis=ax,
                               margin=list(t=50, b=50, r=50, l=50),
                               yaxis=list(tickfont=list(size=fontSize, family=fontFamily),
                                          title=list(font=list(size=fontSize, family=fontFamily))),
                               yaxis2=list(tickfont=list(size=fontSize, family=fontFamily),
                                           title=list(font=list(size=fontSize, family=fontFamily))),
                               xaxis=ax,
                               legend=list(x=1.05, font=list(size=fontSize, family=fontFamily),
                                           groupclick="toggleitem")
                ) |>
                plotly::config(
                    displaylogo=FALSE, editable=TRUE, locale="de",
                    toImageButtonOptions=list(format="png", scale=2, filename="Abbildung"),
                    modeBarButtonsToAdd=list("eraseshape",
                                             "drawclosedpath",
                                             "drawopenpath",
                                             "drawrect",
                                             "drawcircle",
                                             "drawline",
                                             "hoverClosestCartesian",
                                             "hoverCompareCartesian"
                    )
                )
            p <- plotly::layout(p0, yaxis=ay1, yaxis2=ay2)
            p <- plotly::add_trace(p, data=dta[variable %in% y1Var], color=~name, linetype=~caseName,
                                   x=~ts, y=~value) |>
                plotly::add_trace(data=dta[variable %in% y2Var], yaxis="y2",
                                  color=~name, linetype=~caseName,
                                  x=~ts, y=~value)
            p

        })
        shiny::observeEvent(input$readData, {
            y1Var <- input$y1Var
            y2Var <- input$y2Var
            y3Var <- input$y3Var
            y4Var <- input$y4Var
            locations <- input$locations
            htbl <- cman$htbl
            cases1 <- input$cases1[input$cases1 %in% htbl$caseName]
            pChk <- (length(c(y1Var, y2Var, y3Var, y4Var)) > 0) & (length(locations) > 0) & (length(cases1) > 0)
            if (!pChk) {
                shiny::showNotification("Please check if cases, variables, and locations were selected!")
                return(NULL)
            }
            cases2 <- input$cases2[input$cases2 %in% htbl$caseName]
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            tbls <- getVarLoc(varTbl=varTbl(), idTbl=idTbl(),
                              variables=c(y1Var, y2Var, y3Var, y4Var), locations=locations)
            allUnits <- tbls$vTbl[, unique(unit)]
            dta <- lapply(tbls$vuTbl$name, function(v) {
                loc <- tbls$vuTbl[name %in% v, location]
                ids <- tbls$iTbl[location %in% loc, id]
                cases <- lapply(tbls$vTbl[name == v, unique(hash)], function(x) cman$his[[x]])
                vdta <- lapply(cases, function(x) {
                    tbl <- x$getData4Ids(variable=v, ids=ids, at=loc)
                    tbl <- data.table::melt(tbl, id.vars = "ts", variable.name="location", variable.factor=FALSE)
                    tbl[, variable := v]
                    tbl[, caseName := htbl[hash == digest::digest(x$path), caseName]]
                }) |>
                    data.table::rbindlist()
                vdta
            }) |>
                data.table::rbindlist()
            dta[, name := paste0(variable, "_at_", location)]
            shinyjs::show(id="his2d-line-plot")
            lineDta(dta)
        })

        shiny::observeEvent(c(input$cases1, input$cases2), {
            caseHash <- cman$htbl[caseName %in% c(input$cases1, input$cases2), hash]
            if (length(caseHash) < 1)
                return(NULL)
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(value=0.3, message="Reading general information for the cases")
            addedHash <- names(cman$his)
            hash2Add <- setdiff(caseHash, addedHash)
            for (ah in hash2Add) {
                ap <- cman$htbl[hash == ah, path]
                cman$his[[ah]] <- HisNc$new(ap)
            }
            sampleHash <- ifelse(length(hash2Add) > 0, hash2Add[1], addedHash[1])
            aH <- cman$his[[sampleHash]]
            tsInfo <- lapply(caseHash, function(x) {
                ret <- cman$his[[x]]$getTsInfo()
                ret$varTbl$hash <- x
                ret$idTbl$hash <- x
                ret
                })
            vTbl <- lapply(tsInfo, function(x) x$varTbl) |> data.table::rbindlist()
            vTbl[!is.na(unit), label := paste0(long_name, " [", unit, "]")]
            vTbl[is.na(unit), label := long_name]
            vTbl <- vTbl[!grepl("coordinate", name)]
            setorder(vTbl, location, unit, long_name)
            iTbl <- lapply(tsInfo, function(x) x$idTbl) |> data.table::rbindlist()
            iTbl[, id := trimws(id)]
            iTbl <- unique(iTbl)
            iTbl[nchar(id) < 1, id := paste0(location, "_", 1:.N), by=c("location", "hash")]
            iTbl[, idx := .I]
            varTbl(vTbl)
            idTbl(iTbl)
            varChoices <- shinyWidgets::prepare_choices(unique(vTbl[, .SD, .SDcols = -"hash"]),
                                                        label=label, value=name, group_by=location)
            for (aI in c("y1Var", "y2Var", "y3Var", "y4Var"))
                shinyWidgets::updateVirtualSelect(inputId=aI, choices=varChoices)
            locChoices <- shinyWidgets::prepare_choices(unique(iTbl[, .SD, .SDcols = -c("hash", "idx")]),
                                                        label=id, value=id, group_by=location)
            shinyWidgets::updateVirtualSelect(inputId="locations", choices=locChoices)
            progress$set(value=0.9, message="Done.")
        }, ignoreInit=TRUE)
    })
}
