modPlotLineUi <- function(id) {
    ns <- shiny::NS(id)

    bslib::layout_sidebar(
        shinyjs::useShinyjs(),
        sidebar=bslib::sidebar(
            id=ns("plot-lines"), width = "25%",
            bslib::accordion(
                open=c("Data source"), multiple=FALSE,
                bslib::accordion_panel(
                    title="Data source", icon=shiny::icon("folder-open"),
                    shiny::h4("Cases"),
                    shiny::selectInput(ns("ncVar1"), "Variable", choices=""),
                    shiny::selectizeInput(ns("tsIdx1"), "Time step", choices=""),
                    shiny::radioButtons(ns("agg1"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(
                        ns("ncNames1"), "NetCDF files / domains",
                        choices="", multiple=TRUE, search=TRUE, updateOn="close"
                    ),
                    shiny::hr(),
                    shiny::h4("Reference case"),
                    shiny::selectizeInput(ns("ncVar2"), "Variable", choices="", multiple=TRUE,
                                          options=list(maxItems=1)),
                    shiny::selectizeInput(ns("tsIdx2"), "Time step", choices=""),
                    shiny::radioButtons(ns("agg2"), "Aggregation method",
                                        choices=c("none", "min", "max", "mean"), inline=TRUE),
                    shinyWidgets::virtualSelectInput(ns("ncNames2"), "NetCDF files / domains",
                                                     choices="", multiple=TRUE, updateOn="close")
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
                    shiny::actionButton(ns("prepDl"), "Prepare data for downloading..."),
                    shiny::downloadButton(ns("dlMap"), "Download map data!")
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genMap"), label="Generate map...", width='100%'))),
            shiny::column(4, shiny::div(class="d-grid gap-2", bslib::input_task_button(
                id=ns("genCmpMap"), label="Generate comparing map...", width='100%')))
        ),
        bslib::card(
            height="75vh", full_screen=TRUE, id=ns("map2d-card"),
            shinycssloaders::withSpinner(
                image="img/working.gif",
                leaflet::leafletOutput(ns("map2d"), height="550px")),
            bslib::input_task_button(ns("getFeat"), "Get drawn features")
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
