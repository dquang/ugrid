#' Prepare a simple table of colors based on `cols4all::c4a_table`
#' @param filters Filter apply to table of palettes. Possible values are:
#' `cbf` for colorblind-friendly, `fair` for fairness, `crW` for sufficient contrast ratio with white
#' `crB` for sufficient contrast ratio with  black.
getC4aTable <- function(
    type=c("all", "cat", "seq", "div", "cyc"),
    n=NULL, m=NULL, sort="name", series="all",
    filters="none", range=NA, continuous=FALSE
) {

    type <- match.arg(type, several.ok=TRUE)
    seriesNull <- if (any(series == "all")) NULL else series
    nNA <- ifelse(length(n) == 0, NA, n)
    filterLogic <- list(cbf = "cbfriendly > 1",
                        fair = "fair == 'H'",
                        crW = "isTRUE(contrastWT)",
                        crB = "isTRUE(contrastBK)")
    filterLogic <- filterLogic[names(filterLogic) %in% filters]
    if (length(filterLogic) < 1) {
        filterLogic <- "TRUE"
    } else {
        filterLogic <- paste(filterLogic, collapse=" & ")
    }
    if (any(grepl("all", type)))
        type <- cols4all::c4a_types(series=seriesNull, as.data.frame=FALSE)
    type <- type[!grepl("^biv", type)]
    pals <- unlist(sapply(type, cols4all::c4a_palettes, series=seriesNull, USE.NAMES = FALSE))
    if (length(pals) < 1)
        return(NULL)
    scoreTbl <- lapply(pals, cols4all::c4a_scores, n=nNA) |>
        data.table::rbindlist(fill=TRUE)
    scoreTbl <- scoreTbl[eval(parse(text=filterLogic))]
    if (nrow(scoreTbl) < 1)
        return(NULL)
    palColors <- lapply(
        seq_along(scoreTbl$fullname),
        function(i) {cols4all::c4a(palette=scoreTbl[i, fullname], n=scoreTbl[i, n],
                                   range=range, nm_invalid="interpolate")}
    )
    palTbl <-  data.table::data.table(
        fullname = scoreTbl$fullname,
        palette = palColors
    )
    return(palTbl)
}


#' Generate HTML content for `shinyWidgets::picketInput`
#'
#' @param palTbl Table of palettes created by `getC4aTable`
#' @param continuous Logical value for displaying the colors as a color gradient
#' @param width Width of color blocks
#' @param minWidth,maxWidth Minimum and maximum widths of the color gradient
genPaletteContent <- function(
        palTbl, continuous=FALSE, reverse=FALSE,
        width="2em", minWidth="20em", maxWidth="40em") {

    if (!is.data.table(palTbl))
        return(NULL)
    if (isTRUE(reverse)) {
        palTbl[, palette := lapply(palette, rev)]
    }
    # css modified from cols4all
    css_col_norm <- paste0('border-radius: 0px; display: inline-block;',
                           'white-space: nowrap; overflow: auto;',
                           'text-overflow: ellipsis;text-align:left;width: ',
                           width,
                           ';height: 1.5em; font-size: 80%;text-align: c;')
    css_col_ramp <- paste0('font-family: monospace;border-radius: 0px; display: inline-block;',
                           'white-space: nowrap; overflow: auto; text-overflow: ellipsis;',
                           'text-align:left;background-position: right 50px;height: 1.5em; font-size: 80%; min-width: ',
                           minWidth, '; max-width: ', maxWidth,";"
                           )
    if (continuous ) {
        palTbl[, content := paste0(
                '<div style="font-family: monospace;text-align:left;">',
                fullname,
                '<br><span style="',
                css_col_ramp,
                'background-image: linear-gradient(to right, ',
                paste(unlist(palette), collapse = ","),
                ');text-align: c;" >&nbsp;</span></div>'
            ), by = .I]
    } else {
        palTbl[, blocks := paste0('<span style="',
                               css_col_norm, 'background-color: ', unlist(palette), '"></span>',
                               collapse = ""
                        ), by = .I]
        palTbl[, content := sprintf(
            '<div style="text-align:left;"><strong>%s</strong><br>%s</div>',
            fullname, blocks)]
    }
    return(palTbl[, c("fullname", "content")])
}

#' An modified version of `shinyWidgets::pickerInput`
#'
#' This is a specisialised version of `shinyWidgets::pickerInput` for choosing color palettes
#' @param inputId,label,... Parameters for `shinyWidgets::pickerInput`
#' @seealso [shinyWidgets::pickerInput()]
#' @param type,n,m,sort,series,filters,range,continuous  Parameters for `cols4all::c4a_table`
#' @param reverse Should color palettes be reversed?
#' @seealso [cols4all::c4a_table()]
#' @rdname colorPaletteInput
#' @export
colorPaletteInput <- function(
    inputId,
    label=NULL, n=5, filters="none", series=c("tableau", "brewer", "cols4all", "matplotlib"),
    type=c("cat"),
    range=NA, continuous=FALSE, reverse=FALSE,
    ...) {
    palTbl <- getC4aTable(
        type=type, n=n,
        continuous=continuous, filters=filters,
        series=series, range=range
    )
    if (!is.data.table(palTbl)) {
        choices=""
        choicesOpt=NULL
    } else {
        tbl <- genPaletteContent(palTbl, continuous=continuous, reverse=reverse)
        choices <- tbl$fullname
        choicesOpt=list(content=tbl$content)
    }
    ret <- shinyWidgets::pickerInput(
        inputId=inputId, label=label, choices=choices, choicesOpt=choicesOpt
    )
    return(ret)
}


#' Update a `pickerInput` with color palettes
#' @rdname colorPaletteInput
#' @export
updateColorPaletteInput <- function(
        session = getDefaultReactiveDomain(),
        inputId, palTbl,
        continuous=FALSE, reverse=FALSE,
        width=NULL,
        ...
) {
    if (!is.data.table(palTbl)) {
        return(NULL)
    } else {
        if (length(width) != 1) {
            if (!continuous) {
                width <- "2em"
            } else {
                lenName <- palTbl[, max(nchar(fullname), na.rm=TRUE)]
                width <- paste0(lenName, "em")
            }
        }
        tbl <- genPaletteContent(palTbl=palTbl, continuous=continuous, reverse=reverse,
                                 width=width, minWidth=width, maxWidth=width)
        if (length(tbl$fullname) > 0) {
            shinyWidgets::updatePickerInput(
                session=session, inputId=inputId,
                choices=tbl$fullname, choicesOpt=list(content=tbl$content),
                ...
            )
        }
    }
}


