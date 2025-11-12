test_that("readPli", {

    # pli file with cross sections of Elbe river
    file <- system.file("testdata/Obs_cross_sections01_crs.pli", package = "ugrid")
    objxy <- readPli(file, crs = "EPSG:25833")
    # expect_equal(class(obj)[1], "sf")
    expect_true(inherits(objxy, what = "sf"))
    expect_type(objxy, "list")
    expect_equal(dim(objxy), c(23,2))

    expect_equal(objxy[["crName"]][5], "OCS06_Schnackenburg")
    # extract coordinates and compare to original
    allcoords <- sf::st_coordinates(objxy)
    getc <- allcoords[which(allcoords[,"L1"]==5),c("X", "Y")]
    # coordinates of OCS06_Schnackenburg from file
    oric <- matrix(c(2.704275890674320E+005 , 2.698686592644166E+005, 5.882534470629050E+006,5.881906069325095E+006 ), nrow = 2, dimnames=list(c(), c("X", "Y")))
    expect_equal(getc, oric)

    expect_equal(as.character(sf::st_geometry_type(objxy)[5]), "LINESTRING")

    # read with unknown crs
    expect_warning(readPli(file, crs = "EPSG:12345"))

    file <- system.file("testdata/non_existing_file.pli", package = "ugrid")
    expect_null(suppressWarnings(readPli(file,crs = "EPSG:25833")))
    expect_warning(readPli(file,crs = "EPSG:25833"))
    # teste kooridnaten mit delta 10-6


    # pliz file
    file <- system.file("testdata/Buhnen_Deiche_v02_fxw.pliz", package="ugrid")
    objxyz <- readPli(file,  crs = "EPSG:25833")
    expect_equal(dim(objxyz), c(74,2))
    expect_true(inherits(objxyz, what = "sf"))

    allcoords <- sf::st_coordinates(objxyz)
    getc <- allcoords[which(allcoords[,"L1"]==which(objxyz[[1]]=="Nr_re_24:type=kribben")),c("X", "Y", "Z")]
    # coordinates of Nr_re_24:type=kribben from file
    oric <- matrix(c(2.486647558610054E+005,  2.486707301124556E+005, 5.894438098527372E+006,  5.894434326580151E+006,  1.161600000000000E+001,  1.257200000000000E+001), nrow = 2, dimnames=list(c(), c("X", "Y", "Z")))
    expect_equal(getc, oric)

})
