# teste getVarName
test_that("Method getVarName", {
    #ugrid ncdf with 2D topo
    mapFile <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    mesh <-  Ugrid$new(mapFile)
    # df with all vars in nc
    # strictly this is a test for Ugrid$new
    # check data frame attributes
    expect_equal(dim(mesh$vars), c(23,16))
    expect_equal(colnames(mesh$vars),
                 c("name", "id", "type", "ndims", "natts",
                   "dim1", "dim2", "dim3", "dim1Name", "dim2Name",
                   "dim3Name", "fill_value", "long_name", "standard_name", "unit", "hasTime" ))
    obj <-  "Start and end nodes of mesh edges"
    names(obj) <- "long_name"
    expect_equal(unlist(mesh$vars[3, "long_name"]),obj)

    # get Ugrid name of this variable
    expect_equal(mesh$getVarName(variable = "sea_surface_height", topo = "m2D", at = "face"), "mesh2d_s1")
    expect_equal(mesh$getVarName(variable = "altitude", topo = "m2D", at = "face"), "mesh2d_flowelem_bl")
    expect_equal(mesh$getVarName("sea_water_speed"), "mesh2d_ucmag")
    # time !!! not associated with any topo (whether 1D, 2D, or 3D)
    # therefore not valid (mesh$getVarName("time"), "time")
    # message/error/NULL
    expect_message(mesh$getVarName("empty", topo = "m2D", "face"))
    expect_error(mesh$getVarName("altitude", topo = "mD", "face"))
    expect_null( suppressMessages((mesh$getVarName("empty", topo = "m2D", "face"))))
})

test_that("Method getData4Any wrapper functions", {
    # ugrid ncdf 2D Topo
    mapFile <- system.file("testdata/d3dfm/elbe2d/FlowFM_0001_map.nc", package = "ugrid")
    mesh <-  Ugrid$new(mapFile)
    expect_type(mesh$data2D, "list")
    expect_equal(length(mesh$data2D), 0)

    # At Faces
    mesh$getData4Face2D("altitude")
    ### hier sollte ein Unteschied zwischen dim/Nas mit onlyMain=TRUE entstehen!
    main <- mesh$getData4Face2D("sea_surface_height", onlyMain = TRUE)
    mesh$getData4Face2D("sea_surface_height")
    expect_equal(names(mesh$data2D$face), c("altitude","sea_surface_height"))
    expect_equal(dim(mesh$data2D$face$sea_surface_height), c(3621, 15))
    expect_equal(range(mesh$data2D$face$sea_surface_height, na.rm = TRUE), c(8.00000, 17.4104942089))
    length(which(is.na(mesh$data2D$face$sea_surface_height))) # 14677
    expect_equal(dim(main), c(2531, 15))
    expect_equal(dim(mesh$data2D$face$altitude), NULL)
    expect_equal(length(mesh$data2D$face$altitude), 3621)

    # At Edge
    expect_type(mesh$data2D$edge, "NULL")
    mesh$getData4Edge2D("mesh2d_cftrt")
    expect_equal(names(mesh$data2D$edge), "mesh2d_cftrt")
    expect_true(is.matrix(mesh$data2D$edge$mesh2d_cftrt))
    # At Node
    mesh$getData4Node2D("altitude")
    expect_equal(names(mesh$data2D$node), "altitude")
    expect_equal(length(mesh$data2D$node[["altitude"]]), 2381)

    #ugrid ncdf with 1D topo
    mapFile <- system.file("testdata/d3dfm/havel_1d2d/FlowFM_0007_cut_map.nc", package = "ugrid")
    mesh <-  Ugrid$new(mapFile)
    mesh$data1D
    mesh$getData4Node1D("altitude")  #only na
    mesh$getData4Node1D("sea_surface_height")
    expect_equal(length(mesh$data1D$node),2)
    expect_equal(length(mesh$data1D$node$altitude), c(34))
    expect_equal(dim(mesh$data1D$node$sea_surface_height), c(34,11))

})
