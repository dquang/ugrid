#' Find faces that are crossed by a line or intersected by points.
#'
#' @param x an object of points or lines
#' @param mesh a Ugrid-class object
#' @export
findFaces <- function(x, mesh) {

    stopifnot(inherits(x, "sf"))
    stopifnot(inherits(mesh, "Ugrid"))
    xtype <- sf::st_geometry_type(x)
    mesh$buildFace2DPoly()
    if (grepl("POINT", xtype)) {
        ret <- sf::st_intersects(x, mesh$m2D$face2D) |> unlist()
    } else if (grepl("LINE", xtype)) {
        ret <- sf::st_crosses(x, mesh$m2D$face2D) |> unlist()
    } else {
        ret <- NULL
    }

    return(ret)
}

calcFaceStations <- function(faces, mesh) {

    faces <- unlist(faces)
    stopifnot(length(faces) > 1 )
    pol <- mesh$buildFace2DPoly()
    pol <- pol[faces, ]
    faceCen <- sf::st_centroid(pol)
    ret <- sf::st_distance(faceCen[-nrow(faceCen), ], faceCen[-1, ], by_element = TRUE)
    ret <- c(0, ret)
    ret <- cumsum(ret)
    return(ret)
}

buildTemplateRaster <- function(faces, mesh, dx=500, dy=5) {
    faceSta <- calcFaceStations(faces=faces, mesh=mesh)
    depths <- mesh$getData4Any(variable="altitude", topo="m2D", at="interface")
    bbox <- c(xmin=min(faceSta), ymin=min(depths),
                     xmax=max(faceSta), ymax=max(depths))
    class(bbox) <- "bbox"
    ret <- stars::st_as_stars(bbox, dx=dx, dy=dy)
    return(ret)
}


#' @noRd
interpLayer <- function(faces, mesh, variable="mesh2d_sa1",
                        tsIdx=100, dx=500, dy=3){

    faceSta <- calcFaceStations(faces=faces, mesh=mesh)
    # depth of interfaces
    depths <- mesh$getData4Any("altitude", topo="m2D", "interface") |> as.vector()
    xRange <- seq(min(faceSta), max(faceSta), dx)
    yRange <- seq(min(depths), max(depths), dy)
    dta <- mesh$getData4Face2D(variable=variable)
    dta <- dta[, faces, tsIdx]
    dta2 <- as.data.table(t(dta))
    dta2[, x := faceSta / 1000]
    dta3 <- melt(dta2, id.vars="x", variable.name = "layer")
    # the number of layers is one less than the number of interfaces
    if (!variable %in% mesh$m2D$interface)
        depths <- depths[-1]
    dta3[, y := rep(depths, each=length(faces))]
    dta3 <- dta3[!is.na(value)]
    interpTbl <- akima::interp(x=dta3$x, y=dta3$y, z=dta3$value,
                               duplicate="mean", linear=FALSE)
    tbl <- expand.grid(x = interpTbl$x, y = interpTbl$y)
    tbl$z <- as.vector(interpTbl$z)
    tbl <- as.data.table(tbl)
    tbl[, z := nafill(z, type="nocb")]
    tbl[, z := nafill(z, type="locf")]

    return(tbl)
}

