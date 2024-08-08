test_that("checkGI works not correct", {
  expect_error(checkGI(x))
  expect_equal(checkGI(gi), gi)
  g <- head(gi)
  g$score <- NULL
  expect_error(checkGI(g))
})

test_that('checkSignalTransformFun works not correct', {
  expect_error(checkSignalTransformFun(1))
  expect_error(checkSignalTransformFun(list(1)))
  expect_no_error(checkSignalTransformFun(mean))
  expect_no_error(checkSignalTransformFun(list(c)))
})

test_that('parseFeature works not correct', {
  expect_error(parseFeature(gi))
  expect_error(parseFeature(x)) ## not label
  g <- head(x)
  g$label <- letters[seq_along(g)]
  expect_message({g <- parseFeature(g)})
  expect_true(all(c('col', 'type', 'cex', 'pch', 'size') %in%
                    colnames(mcols(g))))
  expect_true(is(g$size, 'unit'))
})

test_that('invertCol works not correct', {
  expect_equal(invertCol(1), '#FFFFFF')
  expect_equal(invertCol('white'), '#000000')
  expect_equal(invertCol('red'), "#00FFFF")
  expect_equal(invertCol('#FF00FF'), '#00FF00')
})
