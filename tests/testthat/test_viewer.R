test_that('view3dCells works not correct', {
  cells <- data.frame(x=seq.int(10), 
                   y=seq.int(10),
                   z=seq.int(10))
  vc <- view3dCells(cells, 'x', 'y', 'z', renderer = 'none')
  expect_s4_class(vc[[1]], 'threeJsGeometry')
  threeJsViewer(vc)
  rglViewer(vc)
})
test_that('view3dStructure works not correct', {
  obj <- GRanges('1', IRanges(seq.int(10), width = 1),
                 x=seq.int(10), 
                 y=seq.int(10),
                 z=seq.int(10))
  feature.gr <- GRanges('1', IRanges(c(3, 7), width = 3),
                        label=c('gene1', 'gene2'),
                        col=c('red', 'blue'),
                        type='gene')
  vc <- view3dStructure(obj, feature.gr, renderer = 'none',
                        coor_mark_interval = 5, coor_tick_unit = 2)
  null <- lapply(vc, expect_s4_class, class = 'threeJsGeometry')
  threeJsViewer(vc)
  rglViewer(vc)
})
