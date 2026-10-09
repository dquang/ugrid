#' Prepare data for downloading.
#'
#' This function is exported for using in future calls
#'
#' @export
#' @keywords internal
prepareData4Download <- function(mdta1, mdta2=NULL, iline1=NULL, iline2=NULL,
                                 fids=NULL, pid=NULL, lgT1=NULL, lgT2=NULL,
                                 dsn=tempfile(pattern="map_data_", fileext=".gpkg")) {

    if (inherits(mdta1, "sf")) {
        sf::st_write(mdta1, dsn=dsn, layer=lgT1, append=FALSE)
        insertMeta(dsn=dsn, tableName=lgT1, fid=fids, pid=pid, bbox=sf::st_bbox(mdta1))
    }
    if (inherits(mdta2, "sf")) {
        sf::st_write(mdta2, dsn=dsn, layer=lgT2, append=TRUE)
        insertMeta(dsn=dsn, tableName=lgT2, fid=fids, pid=pid, bbox=sf::st_bbox(mdta2))
    }

    if (inherits(iline1, "sf"))
        sf::st_write(iline1, dsn=dsn, layer=paste0("Isoline - ", lgT1), append=TRUE)
    if (inherits(iline2, "sf"))
        sf::st_write(iline2, dsn=dsn, layer=paste0("Isoline - ", lgT2), append=TRUE)

    return(dsn)
}
