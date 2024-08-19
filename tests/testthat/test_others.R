test_that('prettyMark works not correct', {
  pM <- prettyMark(c(1000, 1e6, 1e9), 1e3)
  expect_equal(pM, c("1K", "1M", "1G"))
  pM <- prettyMark(c(1e3, 1e6, 1e9)+4e2, 1e3)
  expect_equal(pM, c("1K", "1M", "1G"))
  pM <- prettyMark(c(1e3, 1e6, 1e9)+5e2, 1e5)
  expect_equal(pM, c("1.5K", "1M", "1G"))
})
