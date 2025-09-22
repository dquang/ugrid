#' Generate bbox of domains for a project
#'
#' Generate a table of 4-point coordinates of the domain boundaries for a project
#'
#' @param path Path to the project
#' @param pattern Search pattern for Ugrid-NetCDF files.
#' @import RNetCDF
#' @import data.table
genDomainBbox <- function(path, pattern=getOption("ugrid.pattern")) {

    if (!chkChr(pattern))
        pattern <- "\\.nc$"

    ncfiles <- sapply(path, list.files, pattern=pattern, ignore.case=TRUE, full.names=TRUE, USE.NAMES=FALSE) |>
        as.vector()
    nCores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    ret <- foreach::foreach(i=1:length(ncfiles), .combine=rbind) %dopar% {
        nc <- RNetCDF::open.nc(ncfiles[i])
        fInfo <- RNetCDF::file.inq.nc(nc)
        globalAtt <- sapply(seq.int(0, fInfo$ngatts - 1),
                            function(x) RNetCDF::att.get.nc(nc, variable="NC_GLOBAL", attribute=x))
        isUgrid <- any(grepl("UGRID", globalAtt, ignore.case = TRUE))
        if (isUgrid) {
            attTbl <- ugrid:::getAllAtts(nc=nc)
            nodeCoords <- attTbl[grepl("node_coordinates", name), val] |>
                strsplit(" ", fixed=TRUE) |> unlist()
            xRange <- RNetCDF::var.get.nc(nc, nodeCoords[1]) |> range()
            yRange <- RNetCDF::var.get.nc(nc, nodeCoords[2]) |> range()
            pid <- digest::sha1(ncfiles[i])
            pol <- data.frame(
                X = c(xRange[1], xRange[2], xRange[2], xRange[1]),
                Y = c(yRange[1], yRange[1], yRange[2], yRange[2]),
                polygon_id = rep(pid, 4),
                file_id = rep(basename(ncfiles[i]), 4)
            )
        } else {
            pol <- NULL
        }
        RNetCDF::close.nc(nc)
        return(pol)
    }

    return(ret)
}

genDomainPolygons <- function(meshes) {

    if (is(meshes, "list"))
        meshes <- list(meshes)
    nCores <- parallel::detectCores()
    doParallel::registerDoParallel(cores = parallel::detectCores() - 1)
    `%dopar%` <- foreach::`%dopar%`
    ret <- foreach::foreach(i=1:length(meshes), .combine=rbind) %dopar% {

        aM <- meshes[[i]]
        pol <- aM$buildFace2DPoly()
        aM$readDomainInfo()
        faceDm <- aM$getData4Face2D(aM$m2D$face$cell_domain_number)
        fids <- seq.int(1, length(faceDm))[faceDm == aM$md]
        pol <- pol[fids, ] |>
            sf::st_union() |> sf::st_as_sf()
        pol$path <- aM$path
        pol
    }

    return(ret)
}
