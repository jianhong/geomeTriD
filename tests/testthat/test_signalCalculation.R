test_that("resampleDataByFun works not correct", function(){
  fromGR <- GRanges('chr1', IRanges(seq.int(10), width = 1), score=1)
  ## check for tiles
  targetGR <- GRanges('chr1', IRanges(seq(1, 9, by=2), width=2))
  rd <- resampleDataByFun(fromGR, targetGR, FUN = viewMeans)
  expect_equal(rd$score, rep(1, 5))
  expect_equal(granges(rd), targetGR)
  rd <- resampleDataByFun(fromGR, targetGR, FUN = viewSums)
  expect_equal(rd$score, rep(2, 5))
  expect_equal(granges(rd), targetGR)
  ## check for 1bp
  rd <- resampleDataByFun(fromGR, fromGR, FUN = viewMeans)
  expect_equal(rd, fromGR)
  ## check for bigger size
  targetGR <- GRanges('chr1', IRanges(1, width = 20))
  rd <- resampleDataByFun(fromGR, targetGR, FUN = viewMeans)
  expect_equal(rd$score, 1)
  rd <- resampleDataByFun(fromGR, targetGR, FUN = viewMeans)
  expect_equal(rd$score, 1)
  rd <- resampleDataByFun(fromGR, targetGR, FUN = viewSums)
  expect_equal(rd$score, 10)
})
test_that('smooth3dPoints works not correct', function(){
  obj <- GRanges('1', IRanges(seq.int(5)*10, width=10),
                 x=seq.int(5), y=seq.int(5), z=seq.int(5))
  sp <- smooth3dPoints(obj, 5)
  expect_equal(length(sp), 25)
  expect_no_error({sp <- smooth3dPoints(obj)})
  expect_equal(sp$x0[1], 1)
  expect_equal(sp$x1[50], 5)
})

test_that('create3dGenomicSignals', function(){
  GenoSig <- GRanges('chr1', IRanges(seq(1, 100, by=10), width=10),
                     score=seq.int(10))
  pos <- matrix(rnorm(303), ncol=3)*10
  pos <- cbind(x0=pos[seq.int(100), 1],
               x1=pos[seq.int(101)[-1], 1],
               y0=pos[seq.int(100), 2],
               y1=pos[seq.int(101)[-1], 2],
               z0=pos[seq.int(100), 3],
               z1=pos[seq.int(101)[-1], 3])
  targetObj <- GRanges('chr1', IRanges(seq.int(100), width=1))
  mcols(targetObj) <- pos
  ds <- create3dGenomicSignals(GenoSig, targetObj,
                               signalTransformFun = function(x) {
                                 log2(x + 1)
                               },
                               reverseGenomicSigs=FALSE,
                               type='segment',
                               lwd.maxGenomicSigs=8,
                               name='test',
                               tag='test')
  ## test positionTransformFun
  ds2 <- create3dGenomicSignals(GenoSig, targetObj,
                               signalTransformFun = function(x) {
                                 log2(x + 1)
                               },
                               positionTransformFun = function(x) {
                                 x$z0 <- x$z0+0.2
                                 x$z1 <- x$z1+0.2
                                 x
                               },
                               reverseGenomicSigs=FALSE,
                               type='segment',
                               lwd.maxGenomicSigs=8,
                               name='test',
                               tag='test')
  threeJsViewer(c(ds, ds2))
  null <- mapply(function(a, b){
    expect_equal(a$x, b$x)
    expect_equal(a$y, b$y)
    expect_equal(a$z+0.2, b$z)
  }, ds, ds2)
  ## test for all geometries
  types <- availableGeometries[
    !availableGeometries %in% c('arrow', 'line', 'label', 'text')]
  x <- lapply(types, 
    function(type){
      create3dGenomicSignals(GenoSig, targetObj,
                             signalTransformFun = function(x) {
                               log2(x + 1)
                             },
                             reverseGenomicSigs=FALSE,
                             type=type,
                             lwd.maxGenomicSigs=8,
                             radius=1,
                             radiusTop=1,
                             radiusBottom=1,
                             maxVal=1,
                             path=system.file('extdata', 'suzanne_buffergeometry.json',
                                              package=getPackageName()),
                             name=type,
                             tag=type)
    })
  names(x) <- types
  threeJsViewer(x)
})
