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

set.seed(1)
x1 <- runif(10)
y1 <- runif(10)
x2 <- runif(10)
y2 <- runif(10)
test_that("getSlope works not correct", {
  gs <- getSlope(x1, y1, x2, y2)
  expect_true(all(c('xm', 'ym', 'dx', 'dy', 'slope') %in% names(gs)))
  expect_true(all(lengths(gs)==length(x1)))
  expect_equal(gs$slope, (y2-y1)/(x2-x1))
})

test_that("calcSquareControlPoints works not correct", {
  scp <- calcSquareControlPoints(x1, y1, x2, y2, 1, 90, 1)
  expect_equal(length(scp$x), length(scp$y))
  expect_equal(length(scp$x), 2*length(scp$end))
  expect_equal(length(scp$end), length(x1))
})

test_that("calcControlPoints works not correct", {
  cp <- calcControlPoints(x1, y1, x2, y2, 1, 90, 1)
  expect_equal(length(cp$x), length(cp$y))
  expect_equal(length(cp$x), length(x1))
})

test_that("calcOrigin works not correct", {
  co <- calcOrigin(x1, y1, x2, y2, 0)
  expect_equal(length(co$x), length(co$y))
  expect_equal(length(co$x), length(x1))
})

test_that("interleave works not correct", {
  co <- interleave(ncp=1, ncurve=10, val=0.5, sval=1, eval=1,
                   e=sample(c(TRUE, FALSE), 20, replace = TRUE))
  expect_equal(length(co), 20)
})

test_that("safeEndPoint works not correct", {
  sep <- safeEndPoint(x=c(-1, 2), lim=c(0, 1), dx=0.05)
  expect_equal(sep, c(0.05, 0.95))
})

test_that("pointsDistance works not correct", {
  pd <- pointsDistance(p1=c(0, 0), p2=c(1, 1))
  expect_equal(pd, sqrt(2))
  pd <- pointsDistance(p1=c(0, 0), p2=c(0, 0))
  expect_equal(pd, 0)
})

test_that("rotatePoint works not correct", {
  rp <- rotatePoint(0, 0, 1, 1)
  expect_equal(rp, c(-1, 1))
  rp <- rotatePoint(0, 0, 0, 0)
  expect_equal(rp, c(0, 0))
  rp <- rotatePoint(0, 0, 0, 1)
  expect_equal(rp, c(-1, 0))
  rp <- rotatePoint(0, 0, -1, 1)
  expect_equal(rp, c(-1, -1))
})

test_that("breakPointByBin works not correct",{
  bpb <- breakPointByBin(x=c(0, 10), y=c(0, 10), start=0, end=10, seqn='1')
  expect_equal(bpb$x0, seq(0, 9))
  expect_equal(bpb$x1, seq(1, 10))
  expect_equal(bpb$y0, seq(0, 9))
  expect_equal(bpb$y1, seq(1, 10))
})

test_that("calTickPos works not correct", {
  feature.tick <- GRanges('1', IRanges(c(5, 10), width=1))
  curve_gr <- GRanges('1', IRanges(seq.int(12), width=1),
                      x0=seq.int(12),
                      y0=seq.int(12),
                      x1=seq.int(2, 13),
                      y1=seq.int(2, 13))
  tp <- calTickPos(feature.tick, curve_gr, arrowLen=unit(1, 'mm'))
  expect_equal(tp$x1, c(5, 10))
  expect_equal(tp$y1, tp$x1)
  expect_equal(tp$ol, c('1'=5, '2'=10))
})