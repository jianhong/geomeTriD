#' create 3d Geometry by given genomic signals
#' @description
#' Create a 3d Geometry by given genomic signals for target 3d positions.
#' @param GenoSig The Genomic signals. An object of
#' \link[GenomicRanges:GRanges-class]{GRanges}, 
#' \link[S4Vectors:Pairs-class]{Pairs},
#' or \link[InteractionSet]{GInteractions} with scores or
#'  an object of \link[trackViewer:track]{track}.
#' @param targetObj The GRanges object with mcols x0, y0, z0, x1, y1, and z1
#' @param signalTransformFun The transformation function for genomic signals.
#' @param positionTransformFun The transformation function for the coordinates.
#' The function must have input as a data.frame with colnames
#'  x0, y0, z0, x1, y1, and z1. And it must have
#' output as same dimension data.frame.
#' @param reverseGenomicSigs Plot the genomic signals in reverse values.
#' @param type The Geometry type.See \link{threeJsGeometry}
#' @param color The color of the signal. If there is metadata 'color' in GenoSig
#' this parameter will be ignored.
#' @param tag The tag used to group geometries.
#' @param name The prefix for the name of the geometries.
#' @param rotation The rotations in the x, y and 
#' z axis in radians.
#' @param ... the parameters for each different type of geometries.
#' If type is 'segments', lwd.maxGenomicSigs (the maximal lwd of the line) is
#'  required.
#' If type is 'circle', radius (the radius of the circle) and
#'  the maxVal (the value for 2*pi) is
#'  required.
#' If type is 'sphere', 'dodecahedron', 'icosahedron', 'octahedron',
#' or 'tetrahedron', radius is
#'  required.
#' If type is 'box', 'capsule', 'cylinder', 'cone', or 'torus', if
#' the properties of correspond geometry is not set, they will be set to
#' the transformed score value.
#' If type is 'json', please refer the documentation about
#' BufferGeometryLoader at threejs.org
#' If input 'GenoSig' is an object of Pairs or GInteractions, the type will be
#' set to 'polygon' and topN is used to set how many top events will be plot.
#' @return \link{threeJsGeometry} objects or NULL
#' @importFrom utils getFromNamespace
#' @importFrom methods getPackageName
#' @importFrom S4Vectors Pairs
#' @importFrom InteractionSet GInteractions
#' @export
#' @examples
#' library(GenomicRanges)
#' GenoSig <- GRanges("chr1", IRanges(seq(1, 100, by = 10), width = 10),
#'   score = seq.int(10)
#' )
#' pos <- matrix(rnorm(303), ncol = 3)
#' pos <- cbind(
#'   x0 = pos[seq.int(100), 1],
#'   x1 = pos[seq.int(101)[-1], 1],
#'   y0 = pos[seq.int(100), 2],
#'   y1 = pos[seq.int(101)[-1], 2],
#'   z0 = pos[seq.int(100), 3],
#'   z1 = pos[seq.int(101)[-1], 3]
#' )
#' targetObj <- GRanges("chr1", IRanges(seq.int(100), width = 1))
#' mcols(targetObj) <- pos
#' ds <- create3dGenomicSignals(GenoSig, targetObj,
#'   signalTransformFun = function(x) {
#'     log2(x + 1)
#'   },
#'   reverseGenomicSigs = FALSE,
#'   type = "segment",
#'   lwd.maxGenomicSigs = 8,
#'   name = "test",
#'   tag = "test"
#' )
#' threeJsViewer(ds)
create3dGenomicSignals <- function(GenoSig, targetObj,
                                   signalTransformFun,
                                   positionTransformFun,
                                   reverseGenomicSigs,
                                   type = "segment",
                                   tag,
                                   name,
                                   color = c("gray30", "darkred"),
                                   rotation = c(0, 0, 0),
                                   ...) {
  if(missing(reverseGenomicSigs)) reverseGenomicSigs <- FALSE
  if(missing(signalTransformFun)) signalTransformFun <- c
  checkSmoothedGR(targetObj)
  checkSignalTransformFun(signalTransformFun)
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  stopifnot(length(color) > 0)
  if (!missing(positionTransformFun)) {
    stopifnot(is.function(positionTransformFun))
    pos <- positionTransformFun(
      as.data.frame(targetObj[, c(
        "x0", "y0", "z0",
        "x1", "y1", "z1"
      )])
    )
    if (!all(c(
      "x0", "y0", "z0",
      "x1", "y1", "z1"
    ) %in% colnames(pos))) {
      stop(
        "output of positionTransformFun must has colnames with",
        "x0, y0, z0, z1, y1, and z1"
      )
    }
    if (nrow(pos) != length(targetObj)) {
      stop("output of positionTransformFun must have the same nrow as input")
    }
    targetObj$x0 <- pos[, "x0", drop = TRUE]
    targetObj$y0 <- pos[, "y0", drop = TRUE]
    targetObj$z0 <- pos[, "z0", drop = TRUE]
    targetObj$x1 <- pos[, "x1", drop = TRUE]
    targetObj$y1 <- pos[, "y1", drop = TRUE]
    targetObj$z1 <- pos[, "z1", drop = TRUE]
    rm(pos)
  }
  seqn <- as.character(seqnames(targetObj)[1])
  if (is(GenoSig, "track")) {
    if (GenoSig$format == "WIG") {
      GenoSig <- parseWIG(
        trackScore = GenoSig,
        chrom = seqn,
        from = start(range(targetObj)),
        to = end(range(targetObj))
      )
    }
    if (length(GenoSig$style$color)) {
      if (!all(GenoSig$style$color == "black")) {
        color <- GenoSig$style$color
      }
    }
    if(length(GenoSig$dat2)){
      GenoSig <- Pairs(GenoSig$dat, GenoSig$dat2, score=GenoSig$dat$score)
    }else{
      GenoSig <- GenoSig$dat
    }
  }
  stopifnot(inherits(GenoSig, c("GRanges", "Pairs", "GInteractions")))
  stopifnot("score" %in% colnames(mcols(GenoSig)))
  stopifnot(length(GenoSig)>0)
  mcols(GenoSig)$score <- signalTransformFun(mcols(GenoSig)$score)
  if(is(GenoSig, 'GRanges')){
    GenoSig <- resampleDataByFun(GenoSig, targetObj,
                                 dropZERO = !(reverseGenomicSigs[1]),
                                 na.rm = TRUE
    )
  }else{
    if(type!='segment'){
      type <- 'polygon'
    }
    GenoSig <- getPosByTargetForPairs(GenoSig, targetObj, type, color, ...)
    rotation = c(0, 0, 0)
  }
  checkSignalGeometryType(type, ...)
  FUN <- paste0(
    "create",
    toupper(substr(type, start = 1, stop = 1)),
    tolower(substr(type, start = 2, stop = nchar(type))),
    "Geometry"
  )
  FUN <- getFromNamespace(FUN, ns = getPackageName())
  geo <- FUN(
    GenoSig = GenoSig,
    revGenoSig = reverseGenomicSigs,
    name = name,
    color = color,
    tag = tag,
    rotation = rotation,
    ...
  )
  return(geo)
}

# get Positions for pairs
#' @importFrom S4Vectors first second queryHits subjectHits
getPosByTargetForPairs <- function(queryObj, targetObj,
                                   type, color,
                                   topN = 100,
                                   ...){
  stopifnot(inherits(queryObj, c('Pairs', 'GInteractions')))
  checkSmoothedGR(targetObj)
  if(length(queryObj)>topN[length(topN)]){
    if(topN[length(topN)]==100){
      message('Only top ', topN, ' event will be kept.',
            ' Increase the topN to keep more.')
    }
    score <- mcols(queryObj)$score
    score <- sort(score, decreasing = TRUE)
    score <- score[topN]
    if(length(topN)>1){
      queryObj <- queryObj[mcols(queryObj)$score %in% score]
    }else{
      queryObj <- queryObj[mcols(queryObj)$score>=score]
    }
  }
  if(length(mcols(queryObj)$color)!=length(queryObj)){
    if(length(queryObj)==length(color)){
      mcols(queryObj)$color <- color
    }
  }
  f <- first(queryObj)
  s <- second(queryObj)
  stopifnot('Only fixed bin size is supported for pairs.'=
              all(c(width(f), width(s))==width(f)[1]))
  stopifnot('Only fixed bin size is supported for pairs.'=
              all(width(targetObj)-width(targetObj)[1]<width(targetObj)[1]/10))
  ol_f <- findOverlaps(f, targetObj)
  ol_s <- findOverlaps(s, targetObj)
  f_points <- split(subjectHits(ol_f), queryHits(ol_f))
  s_points <- split(subjectHits(ol_s), queryHits(ol_s))
  inRangePoints <- intersect(names(f_points), names(s_points))
  points <- mapply(function(f, s){
    l <- min(length(f), length(s))
    data.frame(f=f[seq.int(l)], s=s[seq.int(l)])
  }, f_points[inRangePoints], s_points[inRangePoints],
  SIMPLIFY = FALSE)
  points <- cbind(source=rep(inRangePoints,
                             vapply(points, nrow, numeric(1L))),
                  do.call(rbind, points))
  getXYZ <- function(gr, postfix='0'){
    res <- getXYZmean(gr)
    mcols(res) <- mcols(res)[, c('x', 'y', 'z')]
    colnames(mcols(res)) <- paste0(colnames(mcols(res)), postfix)
    return(res)
  }
  if(type=='segment'){
    res <- getXYZ(targetObj[points$f], '0')
    mc2 <- getXYZ(targetObj[points$s], '1')
    mcols(res) <- cbind(
      mcols(res), mcols(mc2),
      score=mcols(queryObj)$score[as.numeric(points$source)], 
      index=as.numeric(points$source))
  }else{
    if(width(targetObj)[1]<=width(f)[1]){
      res <- targetObj[points$f]
      mc2 <- mcols(targetObj[points$s])
      colnames(mc2) <- paste(colnames(mc2), 2, sep='_')
      mcols(res) <- cbind(
        mcols(res), mc2,
        score=mcols(queryObj)$score[as.numeric(points$source)], 
        index=as.numeric(points$source))
    } else{
      stop('Only support for the high resolution signals for segment. ',
           'Try to increase the width of input interactions.')
    }
  }
  if(length(mcols(queryObj)$color)){
    res$color <- mcols(queryObj)$color[res$index]
  }
  return(res)
}
# map score to segments lwd
createSegmentGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    lwd.maxGenomicSigs = 8, alpha = 1, length.out,
    ...) {
  if(lwd.maxGenomicSigs<2){
    warning('Small lwd.maxGenomicSigs will lead to some errors when plot it.')
  }
  if(missing(length.out)){
    if(lwd.maxGenomicSigs<3){
      length.out <- 5
    }else{
      length.out <- 2*round(lwd.maxGenomicSigs - 1)
    }
  }
  lwdRange <- seq(0, lwd.maxGenomicSigs, length.out = length.out+1)[-1]
  genomicSigScoreRange <- quantile(GenoSig$score,
    probs = c(.1, .99)
  )
  if (genomicSigScoreRange[1] == genomicSigScoreRange[2]) {
    genomicSigScoreRange <- range(GenoSig$score)
  }
  if (genomicSigScoreRange[1] != genomicSigScoreRange[2]) {
    genomicSigBreaks <- c(
      -1,
      seq(genomicSigScoreRange[1],
        genomicSigScoreRange[2],
        length.out = length.out
      ),
      max(GenoSig$score) + 1
    )
    genomicSiglabels <- seq_along(genomicSigBreaks)[-length(genomicSigBreaks)]
    if (revGenoSig) {
      genomicSiglabels <- rev(genomicSiglabels)
      color <- rev(color)
    }
    GenoSig$lwd <- lwdRange[as.numeric(as.character(
      cut(GenoSig$score,
        breaks = genomicSigBreaks,
        labels = genomicSiglabels
      )
    ))]
    genomicSigLwd <- sort(unique(GenoSig$lwd))
    if (length(color) > 1) {
      color <-
        colorRampPalette(colors = color)(
          length(genomicSiglabels))
      names(color) <- genomicSiglabels
    } else {
      color <- rep(
        color,
        length(genomicSigLwd)
      )
      names(color) <- genomicSigLwd
    }
    genomic_signal <- lapply(genomicSigLwd, function(lwd) {
      idx <- which(GenoSig$lwd == lwd)
      threeJsGeometry(
        x = as.numeric(t(as.matrix(mcols(GenoSig)[idx, c("x0", "x1"),
          drop = FALSE
        ]))),
        y = as.numeric(t(as.matrix(mcols(GenoSig)[idx, c("y0", "y1"),
          drop = FALSE
        ]))),
        z = as.numeric(t(as.matrix(mcols(GenoSig)[idx, c("z0", "z1"),
          drop = FALSE
        ]))),
        type = "segment",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else color[lwd],
        tag = tag,
        rotation = rotation,
        properties = list(
          size = lwd,
          alpha = alpha
        )
      )
    })
    names(genomic_signal) <- paste0(name, "_lwd_", genomicSigLwd)
    genomic_signal
  } else {
    NULL
  }
}

getXYZmean <- function(GenoSig) {
  GenoSig$x <- rowMeans(as.matrix(mcols(GenoSig)[, c("x0", "x1"),
    drop = FALSE
  ]))
  GenoSig$y <- rowMeans(as.matrix(mcols(GenoSig)[, c("y0", "y1"),
    drop = FALSE
  ]))
  GenoSig$z <- rowMeans(as.matrix(mcols(GenoSig)[, c("z0", "z1"),
    drop = FALSE
  ]))
  return(GenoSig)
}

extendParam <- function(param, l) {
  if (length(param) != l) {
    param <- rep(param, l)[seq.int(l)]
  }
  return(param)
}

# map score to thetaLength
createCircleGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    radius = 8, maxVal = 1, thetaStart = 0,
    ...) {
  GenoSig$score <- 2 * pi * GenoSig$score / maxVal[1]
  if (revGenoSig) GenoSig$score <- 2 * pi - GenoSig$score
  radius <- extendParam(radius, length(GenoSig))
  thetaStart <- extendParam(thetaStart, length(GenoSig))
  GenoSig <- getXYZmean(GenoSig)
  ## If the GenoSig$score can be categorized,
  genomicSigPct <- unique(GenoSig$score)
  if (length(genomicSigPct) < min(length(GenoSig) - 2, 10)) {
    genomic_signal <- mapply(
      genomicSigPct, radius, thetaStart,
      FUN = function(pct, r, ts) {
        idx <- which(GenoSig$score == pct)
        threeJsGeometry(
          x = GenoSig$x[idx],
          y = GenoSig$y[idx],
          z = GenoSig$z[idx],
          type = "circle",
          colors = if(length(GenoSig$color)) GenoSig$color[idx] else color,
          tag = tag,
          rotation = rotation,
          properties = list(
            radius = r,
            thetaStart = ts,
            thetaLength = pct
          )
        )
      }, SIMPLIFY = FALSE
    )
    names(genomic_signal) <-
      names(genomic_signal) <- paste0(name, "_circle_", genomicSigPct)
  } else {
    genomic_signal <- mapply(
      seq_along(GenoSig), radius, thetaStart,
      FUN = function(idx, r, ts) {
        threeJsGeometry(
          x = GenoSig$x[idx],
          y = GenoSig$y[idx],
          z = GenoSig$z[idx],
          type = "circle",
          colors = if(length(GenoSig$color)) GenoSig$color[idx] else color,
          tag = tag,
          properties = list(
            radius = r,
            thetaStart = ts,
            thetaLength = GenoSig$score[idx]
          )
        )
      }, SIMPLIFY = FALSE
    )
    names(genomic_signal) <- names(GenoSig)
    if (length(names(genomic_signal)) != length(genomic_signal)) {
      names(genomic_signal) <-
        paste0(name, "_circle_", seq_along(genomic_signal))
    }
  }
  return(genomic_signal)
}

# map color to score
#' @importFrom grDevices colorRampPalette
mapScore2Color <- function(GenoSig, color) {
  if (length(color) != length(GenoSig)) {
    breaks <- range(GenoSig$score)
    if (length(color) == 1) {
      color <- c("white", color)
    }
    if (breaks[1] == breaks[2]) {
      color <- color[2]
    } else {
      breaks <- seq(breaks[1], breaks[2], length.out = 100)
      breaks[length(breaks)] <- breaks[length(breaks)] + 1
      color <- colorRampPalette(colors = color)(100)
      color <- color[findInterval(GenoSig$score, breaks)]
    }
  }
  return(color)
}

createSphereGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    radius = 8, type = "sphere",
    ...) {
  color <- mapScore2Color(GenoSig, color)
  GenoSig <- getXYZmean(GenoSig)
  if (length(radius) == length(GenoSig)) {
    # pre calculated radius
    genomic_signal <- mapply(function(idx, .radius, .col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = type,
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else .col,
        tag = tag,
        rotation = rotation,
        properties = list(
          radius = .radius
        )
      )
    }, seq_along(GenoSig), radius, color)
    names(genomic_signal) <-
      paste0(name, "_", type, "_", seq_along(genomic_signal))
  } else {
    genomic_signal <- threeJsGeometry(
      x = GenoSig$x,
      y = GenoSig$y,
      z = GenoSig$z,
      type = type,
      colors = if(length(GenoSig$color)) GenoSig$color else color,
      tag = tag,
      rotation = rotation,
      properties = list(
        radius = radius[1]
      )
    )
    genomic_signal <- list(genomic_signal)
    names(genomic_signal) <- name
  }

  return(genomic_signal)
}


createBoxGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    width, height, depth,
    ...) {
  if (missing(width) || missing(height) || missing(depth)) {
    if (missing(width)) {
      width <- GenoSig$score
    }
    if (missing(height)) {
      height <- GenoSig$score
    }
    if (missing(depth)) {
      depth <- GenoSig$score
    }
  }
  GenoSig <- getXYZmean(GenoSig)
  color <- mapScore2Color(GenoSig, color)
  wid <- unique(width)
  hgt <- unique(height)
  dpt <- unique(depth)
  if (length(wid) == 1 && length(hgt) == 1 && length(dpt) == 1) {
    ## if the size are identical
    ## map color
    genomic_signal <- list(
      threeJsGeometry(
        x = GenoSig$x,
        y = GenoSig$y,
        z = GenoSig$z,
        type = "box",
        colors = if(length(GenoSig$color)) GenoSig$color else color,
        tag = tag,
        rotation = rotation,
        properties = list(
          width = wid,
          height = hgt,
          depth = dpt
        )
      )
    )
    names(genomic_signal) <- name
  } else {
    genomic_signal <- mapply(function(idx, w, h, d, col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = "box",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else col,
        tag = tag,
        rotation = rotation,
        properties = list(
          width = w,
          height = h,
          depth = d
        )
      )
    }, seq_along(GenoSig), width, height, depth, color)
    names(genomic_signal) <-
      paste0(name, "_box_", seq_along(genomic_signal))
  }
  return(genomic_signal)
}

createCapsuleGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    height, radius,
    ...) {
  if (missing(height) || missing(radius)) {
    if (missing(height)) {
      height <- GenoSig$score
    }
    if (missing(radius)) {
      radius <- GenoSig$score
    }
  }
  GenoSig <- getXYZmean(GenoSig)
  color <- mapScore2Color(GenoSig, color)
  hgt <- unique(height)
  r <- unique(radius)
  if (length(hgt) == 1 && length(r) == 1) {
    ## if the size are identical
    ## map color
    genomic_signal <- list(
      threeJsGeometry(
        x = GenoSig$x,
        y = GenoSig$y,
        z = GenoSig$z,
        type = "capsule",
        colors = if(length(GenoSig$color)) GenoSig$color else color,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = hgt,
          radius = r
        )
      )
    )
    names(genomic_signal) <- name
  } else {
    genomic_signal <- mapply(function(idx, h, d, col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = "capsule",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else col,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = h,
          radius = d
        )
      )
    }, seq_along(GenoSig), height, radius, color)
    names(genomic_signal) <-
      paste0(name, "_capsule_", seq_along(genomic_signal))
  }
  return(genomic_signal)
}

createCylinderGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    height, radiusTop, radiusBottom,
    ...) {
  if (missing(height) || missing(radiusTop) || missing(radiusBottom)) {
    if (missing(height)) {
      height <- GenoSig$score
    }
    if (missing(radiusTop)) {
      radiusTop <- GenoSig$score
    }
    if (missing(radiusBottom)) {
      radiusBottom <- GenoSig$score
    }
  }
  GenoSig <- getXYZmean(GenoSig)
  color <- mapScore2Color(GenoSig, color)
  hgt <- unique(height)
  rt <- unique(radiusTop)
  rb <- unique(radiusBottom)
  if (length(hgt) == 1 && length(rt) == 1 && length(rb) == 1) {
    ## if the size are identical
    ## map color
    genomic_signal <- list(
      threeJsGeometry(
        x = GenoSig$x,
        y = GenoSig$y,
        z = GenoSig$z,
        type = "cylinder",
        colors = if(length(GenoSig$color)) GenoSig$color else color,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = hgt,
          radiusTop = rt,
          radiusBottom = rb
        )
      )
    )
    names(genomic_signal) <- name
  } else {
    genomic_signal <- mapply(function(idx, h, rt, rb, col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = "cylinder",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else col,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = h,
          radiusTop = rt,
          radiusBottom = rb
        )
      )
    }, seq_along(GenoSig), height, radiusTop, radiusBottom, color)
    names(genomic_signal) <-
      paste0(name, "_cylinder_", seq_along(genomic_signal))
  }
  return(genomic_signal)
}

createConeGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    height, radius,
    ...) {
  if (missing(height) || missing(radius)) {
    if (missing(height)) {
      height <- GenoSig$score
    }
    if (missing(radius)) {
      radius <- GenoSig$score
    }
  }
  GenoSig <- getXYZmean(GenoSig)
  color <- mapScore2Color(GenoSig, color)
  hgt <- unique(height)
  r <- unique(radius)
  if (length(hgt) == 1 && length(r) == 1) {
    ## if the size are identical
    ## map color
    genomic_signal <- list(
      threeJsGeometry(
        x = GenoSig$x,
        y = GenoSig$y,
        z = GenoSig$z,
        type = "cone",
        colors = if(length(GenoSig$color)) GenoSig$color else color,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = hgt,
          radius = r
        )
      )
    )
    names(genomic_signal) <- name
  } else {
    genomic_signal <- mapply(function(idx, h, d, col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = "cone",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else col,
        tag = tag,
        rotation = rotation,
        properties = list(
          height = h,
          radius = d
        )
      )
    }, seq_along(GenoSig), height, radius, color)
    names(genomic_signal) <-
      paste0(name, "_cone_", seq_along(genomic_signal))
  }
  return(genomic_signal)
}

createDodecahedronGeometry <- function(...) {
  createSphereGeometry(..., type = "dodecahedron")
}
createIcosahedronGeometry <- function(...) {
  createSphereGeometry(..., type = "icosahedron")
}
createOctahedronGeometry <- function(...) {
  createSphereGeometry(..., type = "octahedron")
}
createTetrahedronGeometry <- function(...) {
  createSphereGeometry(..., type = "tetrahedron")
}

createTorusGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    tube, radius,
    ...) {
  if (missing(tube) || missing(radius)) {
    if (missing(tube)) {
      tube <- GenoSig$score
    }
    if (missing(radius)) {
      radius <- GenoSig$score
    }
  }
  GenoSig <- getXYZmean(GenoSig)
  color <- mapScore2Color(GenoSig, color)
  hgt <- unique(tube)
  r <- unique(radius)
  if (length(hgt) == 1 && length(r) == 1) {
    ## if the size are identical
    ## map color
    genomic_signal <- list(
      threeJsGeometry(
        x = GenoSig$x,
        y = GenoSig$y,
        z = GenoSig$z,
        type = "torus",
        colors = if(length(GenoSig$color)) GenoSig$color else color,
        tag = tag,
        rotation = rotation,
        properties = list(
          tube = hgt,
          radius = r
        )
      )
    )
    names(genomic_signal) <- name
  } else {
    genomic_signal <- mapply(function(idx, h, d, col) {
      threeJsGeometry(
        x = GenoSig$x[idx],
        y = GenoSig$y[idx],
        z = GenoSig$z[idx],
        type = "torus",
        colors = if(length(GenoSig$color)) GenoSig$color[idx] else col,
        tag = tag,
        rotation = rotation,
        properties = list(
          tube = h,
          radius = d
        )
      )
    }, seq_along(GenoSig), tube, radius, color)
    names(genomic_signal) <-
      paste0(name, "_torus_", seq_along(genomic_signal))
  }
  return(genomic_signal)
}

#' @importFrom rjson fromJSON
createJsonGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    path,
    ...) {
  json <- fromJSON(file = path)
  stopifnot(
    "json file must contain metadata and data" =
      all(c("metadata", "data") %in% names(json))
  )
  GenoSig <- getXYZmean(GenoSig)
  genomic_signal <- list(
    threeJsGeometry(
      x = GenoSig$x,
      y = GenoSig$y,
      z = GenoSig$z,
      type = "json",
      colors = if(length(GenoSig$color)) GenoSig$color else 
        mapScore2Color(GenoSig, color),
      tag = tag,
      rotation = rotation,
      properties = list(
        json = json
      )
    )
  )
  names(genomic_signal) <- name
  return(genomic_signal)
}

# create segments for two points
tileSegments <- function(x1, y1, z1, x2, y2, z2, step=5){
  seq0 <- function(from, to, step){
    seq(from, to, length.out=step+2)[-c(1, step+2)]
  }
  return(list(
    x = do.call(rbind,
                mapply(seq0, x1, x2, step, SIMPLIFY = FALSE)),
    y = do.call(rbind,
                mapply(seq0, y1, y2, step, SIMPLIFY = FALSE)),
    z = do.call(rbind,
                mapply(seq0, z1, z2, step, SIMPLIFY = FALSE))
  ))
}
# get index group
getIndicesGroups <- function(idx, jj){
  .ele <- do.call(cbind, rep(list(idx), jj))
  as.integer(.ele)
}
# get indices for given len of points
getIndices <- function(ii, jj, index){
  iii <- ii-1
  jjj <- jj-1
  indices <- matrix(nrow=3, ncol = 2*iii*jjj)
  idx <- 1
  for(i in seq.int(iii)){
    for(j in seq.int(jjj)){
      a <- (j-1)*ii + i +1
      b <- (j-1)*ii + i
      c <- j*ii + i
      d <- j*ii + i + 1
      # two faces per rect
      indices[, idx ] <- c(a, b, d)
      indices[, idx+1 ] <- c(b, c, d)
      idx <- idx + 2
    }
  }
  ## remove the indices between group
  indicesGroups <- getIndicesGroups(index, jj)
  indicesKeep <- indicesGroups[indices]
  dim(indicesKeep) <- dim(indices)
  indicesKeep <- indicesKeep[2, , drop=TRUE] == 
    indicesKeep[1, , drop=TRUE] &
    indicesKeep[3, , drop=TRUE] == 
    indicesKeep[1, , drop=TRUE]
  return(indices[, indicesKeep, drop=FALSE])
}
mapScore2Alpha <- function(score, default=0.1){
  ra <- range(score, na.rm=TRUE)
  if(ra[2]==ra[1]){
    return(rep(default, length(score)))
  }
  if(any(is.infinite(ra))){
    return(rep(min, length(score)))
  }
  if(ra[2]>ra[1]){
    return((score-ra[1])/diff(ra)/2+default)
  }
  return(rep(default, length(score)))
}
# for interaction data
createPolygonGeometry <- function(
    GenoSig, revGenoSig,
    name, color, tag, rotation,
    ...) {
  genomic_signal <- data.frame(
    GenoSig$x0, GenoSig$x1, GenoSig$x1_2, GenoSig$x0_2,
    GenoSig$y0, GenoSig$y1, GenoSig$y1_2, GenoSig$y0_2,
    GenoSig$z0, GenoSig$z1, GenoSig$z1_2, GenoSig$z0_2,
    if(length(GenoSig$color)) GenoSig$color else mapScore2Color(GenoSig, color),
    mapScore2Alpha(GenoSig$score))
  colnames(genomic_signal) <- c("x1", "x2", "x3", "x4",
                                "y1", "y2", "y3", "y4",
                                "z1", "z2", "z3", "z4",
                                "col", "alpha")
  
  p2p3 <- tileSegments(genomic_signal$x2,
                       genomic_signal$y2,
                       genomic_signal$z2,
                       genomic_signal$x3,
                       genomic_signal$y3,
                       genomic_signal$z3)
  p4p1 <- tileSegments(genomic_signal$x4,
                       genomic_signal$y4,
                       genomic_signal$z4,
                       genomic_signal$x1,
                       genomic_signal$y1,
                       genomic_signal$z1)
  x = cbind(genomic_signal$x1, genomic_signal$x2,
            p2p3$x, 
            genomic_signal$x3, genomic_signal$x4,
            p4p1$x,
            genomic_signal$x1)
  y = cbind(genomic_signal$y1, genomic_signal$y2,
            p2p3$y,
            genomic_signal$y3, genomic_signal$y4,
            p4p1$y,
            genomic_signal$y1)
  z = cbind(genomic_signal$z1, genomic_signal$z2,
            p2p3$z,
            genomic_signal$z3, genomic_signal$z4,
            p4p1$z,
            genomic_signal$z1)
  indices <- getIndices(nrow(x), ncol(x), GenoSig$index)
  idJ <- indices[3, , drop=TRUE]# one triangle, one color, alpha
  geo <- list(threeJsGeometry(
    x = as.numeric(x),
    y = as.numeric(y),
    z = as.numeric(z),
    type = 'polygon',
    colors = rep(genomic_signal$col, ncol(x))[idJ],
    tag = tag,
    rotation = rotation,
    properties = list(
      alpha=rep(genomic_signal$alpha, ncol(x))[idJ],
      indices=as.integer(indices)-1
    )
  ))
  names(geo) <- name
  return(geo)
}
