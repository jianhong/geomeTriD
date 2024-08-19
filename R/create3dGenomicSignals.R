#' create 3d Geometry by given genomic signals
#' @description
#' Create a 3d Geometry by given genomic signals for target 3d positions.
#' @param GenoSig The Genomic signals. An object of 
#' \link[GenomicRanges:GRanges-class]{GRanges} with scores or
#'  an object of \link[trackViewer:track]{track}.
#' @param targetObj The GRanges object with mcols x0, y0, z0, x1, y1, and z1
#' @param signalTransformFun The transformation function for genomic signals.
#' @param reverseGenomicSigs Plot the genomic signals in reverse values.
#' @param type The Geometry type.See \link{threeJsGeometry}
#' @param color The color of the signal.
#' @param tag The tag used to group geometries.
#' @param name The prefix for the name of the geometries.
#' @param ... the parameters for each different type of geometries.
#' If type is 'segments', lwd.maxGenomicSigs (the maximal lwd of the line) is
#'  required.
#' If type is 'circle', radius (the radius of the circle) and 
#'  the maxVal (the value for 2*pi) is
#'  required.
#' @return \link{threeJsGeometry} objects or NULL
#' 
create3dGenomicSignals <- function(GenoSig, targetObj,
                                signalTransformFun,
                                reverseGenomicSigs,
                                type = 'segment',
                                tag,
                                name,
                                color=c("gray30", "darkred"),
                                ...) {
  checkSmoothedGR(targetObj)
  checkSignalTransformFun(signalTransformFun)
  checkSignalGeometryType(type, ...)
  stopifnot(is.character(name))
  stopifnot(length(name)==1)
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
    GenoSig <- GenoSig$dat
  }
  stopifnot(is(GenoSig, "GRanges"))
  stopifnot("score" %in% colnames(mcols(GenoSig)))
  GenoSig <- resampleDataByFun(GenoSig, targetObj,
                               dropZERO = FALSE,
                               na.rm = TRUE)
  GenoSig$score <- signalTransformFun(GenoSig$score)
  FUN = switch(type,
               'segment' = createSegmentsGeometry,
               "circle" = createCircleGeometry)
  geo <- FUN(GenoSig = GenoSig,
             targetObj = targetObj,
             revGenoSig = reverseGenomicSigs,
             name = name,
             color = color,
             tag = tag,
             ...)
  return(geo)
}

createSegmentsGeometry <- function(
    GenoSig, targetObj, revGenoSig,
    name, color, tag,
    lwd.maxGenomicSigs=8, alpha = 1,
    ...){
  genomicSigScoreRange <- quantile(GenoSig$score,
                                   probs = c(.1, .99))
  if (genomicSigScoreRange[1] == genomicSigScoreRange[2]) {
    genomicSigScoreRange <- range(GenoSig$score)
  }
  if (genomicSigScoreRange[1] != genomicSigScoreRange[2]) {
    genomicSigBreaks <- c(
      -1,
      seq(genomicSigScoreRange[1],
          genomicSigScoreRange[2],
          length.out = lwd.maxGenomicSigs - 1
      ),
      max(GenoSig$score) + 1
    )
    genomicSiglabels <- seq_along(genomicSigBreaks)[-length(genomicSigBreaks)]
    if (revGenoSig) {
      genomicSiglabels <- rev(genomicSiglabels)
      color <- rev(color)
    }
    GenoSig$lwd <- as.numeric(as.character(
      cut(GenoSig$score,
          breaks = genomicSigBreaks,
          labels = genomicSiglabels
      )
    ))
    genomicSigLwd <- sort(unique(GenoSig$lwd))
    if (length(color) > 1) {
      color <-
        colorRampPalette(colors = color)(
          max(genomicSigLwd))
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
        x = as.numeric(t(as.matrix(mcols(targetObj)[idx, c("x0", "x1"),
                                              drop = FALSE
        ]))),
        y = as.numeric(t(as.matrix(mcols(targetObj)[idx, c("y0", "y1"),
                                              drop = FALSE
        ]))),
        z = as.numeric(t(as.matrix(mcols(targetObj)[idx, c("z0", "z1"),
                                              drop = FALSE
        ]))),
        type = "segment",
        colors = color[lwd],
        tag = tag,
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

createCircleGeometry <- function(
    GenoSig, targetObj, revGenoSig,
    name, color, tag,
    radius=8, maxVal=1, thetaStart=0,
    ...){
  GenoSig$score <- 2*pi*GenoSig$score/maxVal[1]
  if(length(radius)!=length(GenoSig)){
    radius <- rep(radius, length(GenoSig))[seq_along(GenoSig)]
  }
  if(length(thetaStart)!=length(GenoSig)){
    thetaStart <- rep(thetaStart, length(GenoSig))[seq_along(GenoSig)]
  }
  GenoSig$x <- rowMeans(as.matrix(mcols(targetObj)[, c("x0", "x1"),
                                                   drop = FALSE]))
  GenoSig$y <- rowMeans(as.matrix(mcols(targetObj)[, c("y0", "y1"),
                                                   drop = FALSE]))
  GenoSig$z <- rowMeans(as.matrix(mcols(targetObj)[, c("z0", "z1"),
                                                   drop = FALSE]))
  ## If the GenoSig$score can be categorized,
  genomicSigPct <- unique(GenoSig$score)
  if(length(genomicSigPct)<min(length(GenoSig)-2, 10)){
    genomic_signal <- mapply(
      genomicSigPct, radius, thetaStart,
      FUN = function(pct, r, ts){
        idx <- which(GenoSig$score==pct)
        threeJsGeometry(
          x = GenoSig$x[idx],
          y = GenoSig$y[idx],
          z = GenoSig$z[idx],
          type = "circle",
          colors = color,
          tag = tag,
          properties = list(
            radius = r,
            thetaStart = t,
            thetaLength = pct
          )
        )
      }, SIMPLIFY = FALSE)
    names(genomic_signal) <- 
      names(genomic_signal) <- paste0(name, '_circle_', genomicSigPct)
  }else{
    genomic_signal <- mapply(
      seq_along(GenoSig), radius, thetaStart,
      FUN = function(idx, r, ts){
        threeJsGeometry(
          x = GenoSig$x[idx],
          y = GenoSig$y[idx],
          z = GenoSig$z[idx],
          type = "circle",
          colors = color,
          tag = tag,
          properties = list(
            radius = r,
            thetaStart = t,
            thetaLength = GenoSig$score[idx]
          )
        )
      }, SIMPLIFY = FALSE)
    names(genomic_signal) <- names(GenoSig)
    if(length(names(genomic_signal))!=length(genomic_signal)){
      names(genomic_signal) <- paste0(name, '_circle_', seq_along(genomic_signal))
    }
  }
  return(genomic_signal)
}
