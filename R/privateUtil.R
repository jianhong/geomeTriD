orderedGR <- function(gr = GRanges()) {
  if (length(gr) > 0) {
    gr[order(as.character(seqnames(gr)), start(gr))]
  } else {
    gr
  }
}

#' @importFrom grDevices col2rgb rgb
invertCol <- function(col) {
  n <- names(col)
  col <- col2rgb(col, alpha = FALSE)
  int_col <- abs(255 - col)
  int_col <- apply(int_col, 2, function(.ele) {
    rgb(.ele[1], .ele[2], .ele[3], maxColorValue = 255)
  })
  int_col <- unlist(int_col)
  names(int_col) <- n
  int_col
}

col2hex <- function(col) {
  col <- col2rgb(col, alpha = FALSE)
  col <- apply(col, 2, function(.ele) {
    rgb(.ele[1], .ele[2], .ele[3], maxColorValue = 255)
  })
  unlist(col)
}

checkSignalTransformFun <- function(signalTransformFun) {
  if (length(signalTransformFun) > 1 || is.list(signalTransformFun)) {
    stopifnot(all(vapply(signalTransformFun, is.function, FUN.VALUE = logical(1L))))
  } else {
    stopifnot(is.function(signalTransformFun))
  }
}

checkSignalGeometries <- function(geometries) {
  stopifnot(
    "found not supported geometries" =
      all(geometries %in% availableGeometries)
  )
}

checkSignalGeometryType <- function(type, ...) {
  stopifnot(length(type) == 1)
  if (!type %in% availableGeometries) {
    stop("available geometries are ", availableGeometriesString())
  }
  args <- list(...)
  switch(type,
    "segment" = {
      stopifnot(
        "lwd.maxGenomicSigs is required for segment" =
          "lwd.maxGenomicSigs" %in% names(args)
      )
      stopifnot(
        "lwd.maxGenomicSigs should be a number" =
          is.numeric(args$lwd.maxGenomicSigs)
      )
    },
    "circle" = {
      stopifnot(
        "radius and maxVal are required for circle" =
          all(c("radius", "maxVal") %in% names(args))
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
      stopifnot(
        "maxVal should be a number" =
          is.numeric(args$maxVal)
      )
    },
    "sphere" = {
      stopifnot(
        "radius is required for sphere" =
          "radius" %in% names(args)
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
    },
    "box" = {
      if (!any(c("width", "height", "depth") %in% names(args))) {
        message("The box size will be set by score.")
      }
    },
    "capsule" = {
      if (!any(c("height", "radius") %in% names(args))) {
        message("The capsule size will be set by score.")
      }
    },
    "cylinder" = {
      if (!all(c("height", "radiusTop", "radiusBottom")
      %in% names(args))) {
        message("The cylinder size will be set by score.")
      }
    },
    "cone" = {
      if (!any(c("height", "radius") %in% names(args))) {
        message("The cone size will be set by score.")
      }
    },
    "dodecahedron" = {
      stopifnot(
        "radius is required for dodecahedron" =
          "radius" %in% names(args)
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
    },
    "icosahedron" = {
      stopifnot(
        "radius is required for icosahedron" =
          "radius" %in% names(args)
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
    },
    "json" = {
      stopifnot(
        "path is required for json" =
          "path" %in% names(args)
      )
      stopifnot(
        "The json file does not exist" =
          file.exists(args$path)
      )
    },
    "octahedron" = {
      stopifnot(
        "radius is required for octahedron" =
          "radius" %in% names(args)
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
    },
    "tetrahedron" = {
      stopifnot(
        "radius is required for tetrahedron" =
          "radius" %in% names(args)
      )
      stopifnot(
        "radius should be a number" =
          is.numeric(args$radius)
      )
    },
    "torus" = {
      if (!any(c("tube", "radius") %in% names(args))) {
        message("The torus size will be set by score.")
      }
    }
  )
}

#' @importFrom BiocGenerics width
#' @importFrom S4Vectors mcols<-
checkGI <- function(gi, fixedBin = FALSE) {
  stopifnot(is(gi, "GInteractions"))
  stopifnot("score" %in% colnames(mcols(gi)))
  if (fixedBin) {
    w <- unique(width(regions(gi)))
    stopifnot("The input gi is not bin based data." = length(w) == 1)
  }
  if (any(is.na(mcols(gi)$score))) {
    warning("There are NA values in the gi score. It will be removed.")
    gi <- gi[!is.na(mcols(gi)$score)]
  }
  if (any(is.infinite(mcols(gi)$score))) {
    warning(
      "There are infinite values in the gi score.",
      " It will be changed to .Machine$integer.max"
    )
    mcols(gi)$score[is.infinite(mcols(gi)$score)] <-
      sign(mcols(gi)$score[is.infinite(mcols(gi)$score)]) *
        .Machine$integer.max
  }
  return(gi)
}

checkSmoothedGR <- function(obj) {
  stopifnot(is(obj, "GRanges"))
  stopifnot(all(c("x0", "y0", "z0", "x1", "y1", "z1") %in%
    colnames(mcols(obj))))
}
parseFeature <- function(feature.gr, seqn) {
  if (!missing(feature.gr)) {
    stopifnot(is(feature.gr, "GRanges"))
    if (length(feature.gr) == 0) {
      return(as.character(seqn)[1])
    }
  } else {
    return(as.character(seqn)[1])
  }
  if (length(feature.gr$col) == 0) feature.gr$col <- rep("black", length(feature.gr))
  if (length(feature.gr$type) == 0) {
    message("Feature type is missing. Set as gene.")
    feature.gr$type <- rep("gene", length(feature.gr))
  }
  if (length(feature.gr$cex) == 0) feature.gr$cex <- rep(1, length(feature.gr))
  if (length(feature.gr$pch) == 0) {
    feature.gr$pch <- rep(11, length(feature.gr))
  }
  if (length(feature.gr$size) == 0) {
    if (length(feature.gr)) {
      feature.gr$size <- rep(unit(0.25, "char"), length(feature.gr))
    }
  }
  stopifnot(length(feature.gr$type) == length(feature.gr))
  stopifnot(length(feature.gr$label) == length(feature.gr))
  feature.gr
}

safe_resolution <- function(x, n) {
  id <- width(x) < n
  n <- rep(n, length(x))
  if (any(id)) {
    n[id] <- width(x)[id]
  }
  return(n)
}

rescalePoints <- function(obj, scale_factor) {
  stopifnot(length(scale_factor) == 1)
  stopifnot(is.numeric(scale_factor))
  mcols(obj) <- as.data.frame(mcols(obj)) * scale_factor
  return(obj)
}
