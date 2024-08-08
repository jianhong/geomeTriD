test_that("overlapNodes works not correct", {
  from<-c(x=1, y=1, r=1)
  to<-c(x=1, y=1, r=1)
  expect_true(overlapNodes(from, to, 1))
  expect_true(overlapNodes(from, c(3, 3, 1), 1))
  expect_false(overlapNodes(from, c(4, 3, 1), 1))
})

test_that("mirrorP works not correct", {
  Q <- c(x=0.5, y=-.5)
  P <- c(x=0, y=0)
  R <- c(x=0, y=1)
  N <- 1
  Q1 <- mirrorP(Q, P, R, N)
  expect_equal(unname(-1*Q[1]), Q1[1])
  #check by eye, plot(rbind(Q, P, R, Q1), col=seq.int(4))
  Q2 <- mirrorP(Q, P, R, 2)
  expect_equal(Q2[1], -1.5)
  Q3 <- mirrorP(P, Q, Q1, N)
  #check by eye, plot(rbind(Q, Q1, P, Q3), col=seq.int(4))
  expect_equal(Q3[2], -1)
  expect_equal(Q3[1], 0)
})

