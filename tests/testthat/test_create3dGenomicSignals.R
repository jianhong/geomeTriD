test_that("create3dGenomicSignals", function() {
  GenoSig <- GRanges("chr1", IRanges(seq(1, 100, by = 10), width = 10),
    score = seq.int(10)
  )
  pos <- matrix(rnorm(303), ncol = 3) * 10
  pos <- cbind(
    x0 = pos[seq.int(100), 1],
    x1 = pos[seq.int(101)[-1], 1],
    y0 = pos[seq.int(100), 2],
    y1 = pos[seq.int(101)[-1], 2],
    z0 = pos[seq.int(100), 3],
    z1 = pos[seq.int(101)[-1], 3]
  )
  targetObj <- GRanges("chr1", IRanges(seq.int(100), width = 1))
  mcols(targetObj) <- pos
  ds <- create3dGenomicSignals(GenoSig, targetObj,
    signalTransformFun = function(x) {
      log2(x + 1)
    },
    reverseGenomicSigs = FALSE,
    type = "segment",
    lwd.maxGenomicSigs = 8,
    name = "test",
    tag = "test"
  )
  ## test positionTransformFun
  ds2 <- create3dGenomicSignals(GenoSig, targetObj,
    signalTransformFun = function(x) {
      log2(x + 1)
    },
    positionTransformFun = function(x) {
      x$z0 <- x$z0 + 0.2
      x$z1 <- x$z1 + 0.2
      x
    },
    reverseGenomicSigs = FALSE,
    type = "segment",
    lwd.maxGenomicSigs = 8,
    name = "test",
    tag = "test"
  )
  threeJsViewer(c(ds, ds2))
  null <- mapply(function(a, b) {
    expect_equal(a$x, b$x)
    expect_equal(a$y, b$y)
    expect_equal(a$z + 0.2, b$z)
  }, ds, ds2)
  ## test for color
  GenoSig$color <- rep_len(seq.int(7), length(GenoSig))
  ## test for all geometries
  types <- availableGeometries[
    !availableGeometries %in% c("arrow", "line", "label", "text", "polygon")
  ]
  x <- lapply(
    types,
    function(type) {
      create3dGenomicSignals(GenoSig, targetObj,
        signalTransformFun = function(x) {
          log2(x + 1)
        },
        genomicScoreRange = c(0, 11),
        reverseGenomicSigs = FALSE,
        type = type,
        lwd.maxGenomicSigs = 8,
        radius = 1,
        radiusTop = 1,
        radiusBottom = 1,
        maxVal = 1,
        path = system.file("extdata", "suzanne_buffergeometry.json",
          package = getPackageName()
        ),
        name = type,
        tag = type
      )
    }
  )
  names(x) <- types
  threeJsViewer(x)
  ## test for Pairs/GInteractions
  ps <- Pairs(GenoSig, rev(GenoSig), score=GenoSig$score)
  gi <- GInteractions(GenoSig, rev(GenoSig), score=GenoSig$score)
  x <- create3dGenomicSignals(ps, targetObj, name='ps', tag='tag')
  threeJsViewer(x)
  x <- create3dGenomicSignals(gi, targetObj, name='gi', tag='tag')
  threeJsViewer(x)
})
