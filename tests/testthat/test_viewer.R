test_that("view3dCells works not correct", {
  cells <- data.frame(
    x = seq.int(10),
    y = seq.int(10),
    z = seq.int(10)
  )
  vc <- view3dCells(cells, "x", "y", "z", renderer = "none")
  expect_s4_class(vc[[1]], "threeJsGeometry")
  vc[[1]]$properties$radius <- 1
  vc[[1]]$properties$radiusTop <- 1
  vc[[1]]$properties$radiusBottom <- 1
  vc[[1]]$properties$size <- 1
  vc[[1]]$properties$headLength <- 1
  vc[[1]]$properties$headWidth <- .5
  vc[[1]]$properties$width <- 1
  vc[[1]]$properties$height <- 1
  vc[[1]]$properties$depth <- 1
  vc[[1]]$properties$thetaStart <- 0
  vc[[1]]$properties$thetaLength <- 1.5 * pi
  vc[[1]]$properties$tube <- .5
  vc[[1]]$properties$label <- "text"
  vc[[1]]$rotation <- c(0, pi/2, 0)
  for (type in availableGeometries) {
    if(type!='polygon'){
      message(type)
      vc[[1]]$type <- type
      threeJsViewer(vc)
      rglViewer(vc)
      rgl::close3d()
    }
  }
  vc[[1]]$rotation <- c(pi, 0, 0)
  for (type in availableGeometries) {
    if(type!='polygon'){
      message(type)
      vc[[1]]$type <- type
      threeJsViewer(vc)
      rglViewer(vc)
      rgl::close3d()
    }
  }
})
test_that("view3dStructure works not correct", {
  obj <- GRanges("1", IRanges(seq.int(10), width = 1),
    x = sample.int(10),
    y = sample.int(10),
    z = sample.int(10)
  )
  feature.gr <- GRanges("1", IRanges(c(3, 7), width = 3),
    label = c("gene1", "gene2"),
    col = c("red", "blue"),
    type = "gene"
  )
  vc <- view3dStructure(obj, feature.gr,
    renderer = "none",
    coor_mark_interval = 5, coor_tick_unit = 2
  )
  null <- lapply(vc, expect_s4_class, class = "threeJsGeometry")
  threeJsViewer(vc)
  rglViewer(vc, background = "white")
  rgl::close3d()
  obj2 <- obj
  obj2$x <- obj$x * 2
  obj2$y <- obj$y * 2
  obj2$z <- obj$z * 2
  vc2 <- view3dStructure(obj2, feature.gr,
                         renderer = "none",
                         coor_mark_interval = 5, coor_tick_unit = 2
  )
  vc2 <- lapply(vc2, function(.ele){
    .ele$side <- 'right'
    .ele
  })
  null <- lapply(vc2, expect_s4_class, class = "threeJsGeometry")
  threeJsViewer(vc, vc2, title = c('10', '20'))
})
