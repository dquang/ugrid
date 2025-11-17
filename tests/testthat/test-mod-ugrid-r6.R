# teste getVarName
test_that("Method getVarName", {
    #ugrid ncdf with 2D topo
    file <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    UgridObj <-  Ugrid$new(file)
    # df with all vars in nc
    # strictly this is a test for Ugrid$new
    # check data frame attributes
    expect_equal(dim(UgridObj$vars), c(23,16))
    expect_equal(colnames(UgridObj$vars), c("name", "id", "type", "ndims", "natts", "dim1", "dim2", "dim3", "dim1Name", "dim2Name", "dim3Name", "fill_value", "long_name", "standard_name", "unit", "hasTime" ))
    obj <-  "Start and end nodes of mesh edges"
    names(obj) <- "long_name"
    expect_equal(unlist(UgridObj$vars[3, "long_name"]),obj)

    # get Ugrid name of this variable
    expect_equal(UgridObj$getVarName(variable = "sea_surface_height", topo = "m2D", at = "face"), "mesh2d_s1")
    expect_equal(UgridObj$getVarName(variable = "altitude", topo = "m2D", at = "face"), "mesh2d_flowelem_bl")
    expect_equal(UgridObj$getVarName("sea_water_speed"), "mesh2d_ucmag")
    # time !!! not associated with any topo (whether 1D, 2D, or 3D)
    # therefore not valid (UgridObj$getVarName("time"), "time")
    # message/error/NULL
    expect_message(UgridObj$getVarName("empty", topo = "m2D", "face"))
    expect_error(UgridObj$getVarName("altitude", topo = "mD", "face"))
    expect_null( suppressMessages((UgridObj$getVarName("empty", topo = "m2D", "face"))))
})

test_that("Method getData4Any wrapper functions", {
    # ugrid ncdf 2D Topo
    file <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    UgridObj <-  Ugrid$new(file)
    expect_type((UgridObj$data2D), "list")
    expect_equal(length(UgridObj$data2D), 0)

    # At Faces
    UgridObj$getData4Face2D("altitude")
    ### hier sollte ein Unteschied zwischen dim/Nas mit onlyMain=TRUE entstehen!
    main <- UgridObj$getData4Face2D("sea_surface_height", onlyMain = TRUE)
    UgridObj$getData4Face2D("sea_surface_height")
    expect_equal(names(UgridObj$data2D$face), c("altitude","sea_surface_height"))
    expect_equal(dim(UgridObj$data2D$face$sea_surface_height), c(3621, 15))
    expect_equal(range(UgridObj$data2D$face$sea_surface_height, na.rm = TRUE), c(8.00000, 17.4104942089))
    length(which(is.na(UgridObj$data2D$face$sea_surface_height))) # 14677
    expect_equal(dim(main), c(2531,15))
    expect_equal(dim(UgridObj$data2D$face$altitude), NULL)
    expect_equal(length(UgridObj$data2D$face$altitude), 3621)

    # At Edge
    expect_type(UgridObj$data2D$edge, "NULL")
    UgridObj$getData4Edge2D("mesh2d_cftrt")
    expect_equal(names(UgridObj$data2D$edge), "mesh2d_cftrt")
    expect_equal(length(UgridObj$data2D), 2)

    # At Node
    UgridObj$getData4Node2D("altitude")
    expect_equal(names(UgridObj$data2D), c("face", "edge", "node"))
    expect_equal(names(UgridObj$data2D$node), "altitude")
    expect_equal(length(UgridObj$data2D$node[["altitude"]]), 2381)

    #ugrid ncdf with 1D topo
    file <- system.file("testdata/d3dfm/havel_1d2d/FlowFM_0007_cut_map.nc", package = "ugrid")
    UgridObj <-  Ugrid$new(file)
    UgridObj$data1D
    UgridObj$getData4Node1D("altitude")  #only na
    UgridObj$getData4Node1D("sea_surface_height")
    expect_equal(length(UgridObj$data1D$node),2)
    expect_equal(length(UgridObj$data1D$node$altitude), c(34))
    expect_equal(dim(UgridObj$data1D$node$sea_surface_height), c(34,11))

})
