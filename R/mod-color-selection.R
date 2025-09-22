#' Shiny module for overviewing generating color palettes
#'
#' This module is based on the source code of `cols4all::c4a_gui`
#' @rdname color-selection
#' @export
colSelectionUi <- function(id) {
    ns <- NS(id)
    c4aTypes <- cols4all::c4a_types()
    c4aTypes <- c4aTypes[grep("^biv", c4aTypes$type, invert=TRUE), ]
    c4aSeries <- cols4all::c4a_series()
    shiny::tagList(
            shinyjs::useShinyjs(),
            shiny::fluidRow(
                shiny::column(3, shiny::radioButtons(ns("palType"), label="Palette type", inline=TRUE,
                                       choiceNames=c4aTypes$description,
                                       choiceValues=c4aTypes$type, selected="seq")),
                shiny::column(3, shiny::sliderInput(ns("nColor"), "Number of colors", min=2, value=5, max=36)),
                shiny::column(3, shiny::selectizeInput(ns("filter"), "Filter",
                                         choices=c("None"="none",
                                                   # "Only n = nmax (categorical only)" = "nmax",
                                                   "Colorblind-friendly" = "cbf",
                                                   "Fair" = "fair",
                                                   "Good contrast ratio with white" = "crW",
                                                   "Good contrast ratio with black" = "crB"),
                                         selected="cbf")),
                shiny::column(3,
                    shinyWidgets::pickerInput(ns("series"), label="Palette series",
                                              choices=c4aSeries$series, selected=c4aSeries$series,
                                              options=list(maxOptions=10000, `actions-box`=TRUE, `live-search`=TRUE,
                                                           `live-search-normalize`=TRUE),
                                              multiple=TRUE))
            ),
            shiny::hr(),
            shiny::selectizeInput(ns("palette"), "Select palette", choices="matplotlib.pi_yg"),
            shiny::fluidRow(
                shiny::column(4, shiny::plotOutput(ns("pointPlot"), height="100px")),
                shiny::column(4, shiny::plotOutput(ns("linePlot"), height="100px")),
                shiny::column(4, shiny::plotOutput(ns("mapPlot"), height="100px"))
            ),
            shiny::hr(),
            shiny::fluidRow(
                shiny::column(3, shiny::checkboxInput(ns("continous"), "Show as continuous palette?")),
                shiny::column(3, shiny::sliderInput(ns("range"), "Range", min=0, max=1, value=c(0, 1), dragRange=TRUE))
            ),
            shiny::tableOutput(ns("colTable"))
    )
}

#' @rdname color-selection
#' @export
colSelectionServer <- function(input, output, session, result=reactiveVal()) {

    c4aSeries <- cols4all::c4a_series()
    palColors <- shiny::reactiveVal()
    palTbl <- shiny::reactive({
        filters <- input$filter
        if (filters == "none")
            filters <- character(0)
        if (substr(input$palType, 1, 1) == "b") m <- input$nColor else m <- input$nColor
        series <- input$series
        if (length(series) < 1)
            series <- "cols4all"
        for (aE in c("palette", "pointPlot", "linePlot", "mapPlot"))
            shinyjs::show(aE)
        tbl <- tryCatch(
            {
                cols4all:::prep_table(
                    type=input$palType, n=input$nColor, m=m,
                    continuous=input$continous, sort="name", series=series,
                    show.scores=FALSE, range=input$range,
                    columns=input$nColor, filters=filters, verbose=FALSE)
            },
            error=function(e) {
                shiny::showNotification("Error while generating table of palletes. Please check the parameters!")
                for (aE in c("palette", "pointPlot", "linePlot", "mapPlot"))
                    shinyjs::hide(aE)
                return(NULL)}
        )
        tbl

    })
    shiny::observe({
        retColors <- tryCatch(
            cols4all::c4a(palette=input$palette, n=input$nColor, type=input$type, range=input$range),
            error=function(e) {
                shiny::showNotification("Error while generating colors. Please check the parameters!")
                for (aE in c("palette", "pointPlot", "linePlot", "mapPlot"))
                    shinyjs::hide(aE)
                return(NULL)})
        palColors(retColors)
        result(list(palette=input$palette, n=input$nColor, type=input$type, range=input$range, series=input$series))
    })
    output$colTable <- function() {
        shiny::withProgress({
            tryCatch({cols4all:::plot_table(palTbl(), include.na = FALSE, cvd.sim="none", verbose=FALSE,
                                  text.format="hex", text.col = "same")},
                     error=function(e) {
                         shiny::showNotification("Error while plotting table of palletes. Please check the parameters!")
                         for (aE in c("palette", "pointPlot", "linePlot", "mapPlot"))
                             shinyjs::hide(aE)
                         return(NULL)})
        }, message = "Preparing color table...")

    }
    output$pointPlot <- shiny::renderPlot({
        cols4all:::c4a_plot_scatter(palColors(), size=1)
    })
    output$linePlot <- shiny::renderPlot({
        cols4all:::c4a_plot_lines(palColors(), lwd=2)
    })
    output$mapPlot <- shiny::renderPlot({
        cols4all:::c4a_plot_map(palColors())
    })
    shiny::observe({
        tbl <- palTbl()
        shiny::updateSelectizeInput(inputId="palette", choices=tbl$zn$fullname)
    }) |> shiny::bindEvent(palTbl())
}

if (interactive()) {
    colUi <- function(request) {
        shiny::fluidPage(
            colSelectionUi("test")
        )
    }

    colServer <- function(input, output, session) {
        shiny::moduleServer("test", colSelectionServer)
    }

    shiny::shinyApp(ui=colUi, server=colServer)
}


