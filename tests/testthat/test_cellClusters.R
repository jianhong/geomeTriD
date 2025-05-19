xyzs <- lapply(seq.int(20), function(i){
   matrix(sample.int(100, 60, replace = TRUE),
   nrow=20, dimnames=list(NULL, c('x', 'y', 'z')))
})
## add NA to randow row
xyzs <- mapply(xyzs, round(runif(20, max = 3)), FUN=function(xyz, N){
  if(N>0){
    NAs <- sample.int(20, N, replace = FALSE)
    xyz[NAs, ] <- NA
  }
  xyz
}, SIMPLIFY = FALSE)

test_that("rescalePointClouds works not correct", {
  res <- rescalePointClouds(xyzs)
  center <- vapply(res, colMeans, FUN.VALUE=numeric(3L), na.rm=TRUE)
  expect_true(all(center<1e-9))
  rg <- vapply(res, range, numeric(2L), na.rm=TRUE)
  expect_true(all(abs(colSums(rg))<0.5))
})

test_that("fill_NA works not correct", {
  res <- lapply(xyzs, fill_NA)
  idx <- lapply(res, function(.ele){
    which(is.na(.ele[, 'x']))
  })
  expect_true(all(lengths(idx)==0))
  xyz <- rbind(matrix(1, nrow = 1, ncol=3), matrix(NA, nrow=10, ncol=3))
  colnames(xyz) <- c('x', 'y', 'z')
  res <- fill_NA(xyz)
  expect_true(all(res==1))
})

test_that('cellClusters works not correct', {
  cc <- cellClusters(xyzs)
  expect_equal(length(cc$order), length(xyzs))
})
