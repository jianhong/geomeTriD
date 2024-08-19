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
test_that('', function(){
  obj <- GRanges('1', IRanges(seq.int(5)*10, width=10),
                 x=seq.int(5), y=seq.int(5), z=seq.int(5))
  sp <- smooth3dPoints(obj, 5)
  expect_equal(length(sp), 25)
  expect_no_error({sp <- smooth3dPoints(obj)})
  expect_equal(sp$x0[1], 1)
  expect_equal(sp$x1[50], 5)
})
