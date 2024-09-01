A <- matrix(c(
  23, 178,
  66, 173,
  88, 187,
  119, 202,
  122, 229,
  170, 232,
  179, 199
), ncol = 2, byrow = TRUE)
B <- matrix(c(
  232, 38,
  208, 32,
  181, 31,
  155, 45,
  142, 33,
  121, 59,
  139, 69
), ncol = 2, byrow = TRUE)
pdist <- function(x, y) {
  sqrt(rowSums((x - y)^2))
}
test_that("kabsch works not correct", {
  B1 <- kabsch(B, A)
  vB <- mean(pdist(B1, A))
  expect_true(vB < 20)
  A1 <- kabsch(A, B)
  vA <- mean(pdist(A1, B))
  expect_equal(vA, vB, tolerance = 0.01)
})

test_that("alignCoor works not correct", {
  res <- alignCoor(x, x)
  expect_true(is(res, "GRanges"))
  d <- mean(pdist(as.matrix(mcols(res)), as.matrix(mcols(x))))
  expect_equal(d, 1e-3, tolerance = 1e-3)
})
