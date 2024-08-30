
addPoints <- function(pointscollection, x, y, s, e) {
  pointscollection[[length(pointscollection) + 1]] <- list(
    x = x, y = y, start = s, end = e
  )
  pointscollection
}

getStartConnectionPoints <- function(shapeX) {
  ## return the nearest pairs
  ab <- c(1, length(shapeX[[1]]$x))
  p <- lapply(shapeX, function(.ele) data.frame(x = .ele$x[ab], y = .ele$y[ab]))
  pD <- apply(p[[1]], 1, function(p1) {
    apply(p[[2]], 1, function(p2) pointsDistance(p1, p2))
  }, simplify = FALSE)
  pD <- unlist(pD)
  cn <- list(c(1, 1), c(1, 2), c(2, 1), c(2, 2))
  cn <- cn[[which(pD == min(pD))[1]]]
  return(ab[cn]) ## return the connect pair (from to)
}

checkConnectionPoints <- function(shapeX, a, close = TRUE) {
  ## check distance, which one will follow the distance of a, b points
  ## if the bps is long, select the far point,
  ## else select the close point
  ab0 <- c(1, length(shapeX[[1]]$x))
  ab <- c(a, 1)
  p1 <- c(shapeX[[1]]$x[1], shapeX[[1]]$y[1])
  p2 <- data.frame(x = shapeX[[2]]$x[ab0], y = shapeX[[2]]$y[ab0])
  pD <- apply(p2, 1, pointsDistance, p1 = p1)
  i <- which(pD == min(pD))[1]
  if (length(close) == 0) close <- FALSE
  if (is.na(close[1])) close <- FALSE
  if (!is.logical(close)) close <- FALSE
  if (close) {
    ab[2] <- ab0[i]
  } else {
    ab[2] <- ab0[-i]
  }
  return(ab)
}

safeEndPoint <- function(x, lim, dx) {
  if (any(x < lim[1])) x[x<lim[1]] <- lim[1] + dx
  if (any(x > lim[2])) x[x>lim[2]] <- lim[2] - dx
  x
}

plotStartEndPoints <- function(xy, idx, xlim, ylim, start, ...) {
  # if it is the end point, the start will be the y of start point
  ## plot a horizontal line to the edge
  x <- xy$x[idx]
  y <- xy$y[idx]
  
  if (idx == 1) {
    idx <- c(1, 2)
  } else {
    idx <- c(idx - 1, idx)
  }
  xs <- xy$x[idx]
  ys <- xy$y[idx]
  slope <- diff(ys) / diff(xs)
  b <- y - slope * x
  
  x0 <- ifelse(x > mean(xlim), xlim[2], xlim[1])
  y0 <- ifelse(y > mean(ylim), ylim[2], ylim[1])
  dx <- pointsDistance(c(x, y), c(x0, y))
  dy <- pointsDistance(c(x, y), c(x, y0))
  ddx <- diff(xlim) / 100
  ddy <- diff(ylim) / 100
  if (dx <= dy) {
    # cross y axis
    x1 <- ifelse(x > mean(xlim), xlim[2] - ddx, xlim[1] + ddx)
    if (slope == 0) {
      y1 <- y0 <- y
      if (!missing(start)) {
        if (start[1] == y) {
          y1 <- y0 <- y + ddy
        }
      }
    } else {
      y0 <- slope * x0 + b
      y1 <- slope * x1 + b
    }
    y0 <- safeEndPoint(y0, ylim, 0)
    y1 <- safeEndPoint(y1, ylim, ddy)
  } else {
    # cross x axis
    y1 <- ifelse(y > mean(ylim), ylim[2] - ddy, ylim[1] + ddy)
    x0 <- (y0 - b) / slope
    x1 <- (y1 - b) / slope
    x0 <- safeEndPoint(x0, xlim, 0)
    x1 <- safeEndPoint(x1, xlim, ddx)
  }
  
  if (!missing(start)) {
    return(list(x = c(x, x0), y = c(y, y0)))
  } else {
    return(list(x = c(x0, x), y = c(y0, y)))
  }
}

pointsDistance <- function(p1, p2) {
  stopifnot(is.numeric(p1))
  stopifnot(is.numeric(p2))
  sqrt(sum((p1[c(1, 2)] - p2[c(1, 2)])^2))
}
overlapNodes <- function(from, to, lwd) {
  pointsDistance(from, to) <= from[3] + to[3] + lwd
}
moveNodes <- function(from, to, lwd, reverse = FALSE) {
  # from, to, data structure: c(x, y, r)
  lwd <- lwd / 2
  center <- colMeans(rbind(from, to))[c(1, 2)]
  pd <- pointsDistance(from[c(1, 2)], center)
  if (pd != 0) {
    from <- from[c(1, 2)] - (from[c(1, 2)] - center) *
      (1 - (from[3] + lwd) / pd)
  } else {
    from <- from[c(1, 2)]
  }
  pd <- pointsDistance(to[c(1, 2)], center)
  if (pd != 0) {
    to <- to[c(1, 2)] - (to[c(1, 2)] - center) *
      (1 - (to[3] + lwd) / pd)
  } else {
    to <- to[c(1, 2)]
  }
  return(list(from = from, to = to))
}

reorderEdgeLinkBySize <- function(edgeL_link, nodeXYout) {
  if (nrow(edgeL_link) == 0) {
    return(edgeL_link)
  }
  size <- apply(edgeL_link, 1, function(.ele) {
    sum(nodeXYout[.ele[c(1, 2)], 3, drop = TRUE])
  })
  edgeL_link[order(size, decreasing = TRUE), , drop = FALSE]
}

fixXY <- function(nodeXY, vertex.size, edgeL_link, lwd = 10 / 300) {
  ## rearrange nodes to make sure they are not overlap
  ## and make the interaction of nodes direct touch each other
  if (nrow(edgeL_link) == 0) {
    return(nodeXY)
  }
  nodeXYout <- cbind(nodeXY, vertex.size)
  edgeL_link <- reorderEdgeLinkBySize(edgeL_link, nodeXYout)
  ## touch each other
  for (i in seq.int(nrow(edgeL_link))) {
    from <- nodeXYout[edgeL_link[i, "from"], ]
    to <- nodeXYout[edgeL_link[i, "to"], ]
    if (!overlapNodes(from, to, lwd = lwd)) {
      newCoor <- moveNodes(from, to, lwd = lwd)
      nodeXYout[edgeL_link[i, "from"], c(1, 2)] <- newCoor[["from"]]
      nodeXYout[edgeL_link[i, "to"], c(1, 2)] <- newCoor[["to"]]
    }
  }
  ## Do not touch each other
  for (i in seq.int(nrow(edgeL_link))) {
    from <- nodeXYout[edgeL_link[i, "from"], , drop = FALSE]
    to <- nodeXYout[edgeL_link[i, "to"], , drop = FALSE]
    if (overlapNodes(from, to, lwd = 0)) {
      newCoor <- moveNodes(from, to, lwd = lwd, reverse = TRUE)
      nodeXYout[edgeL_link[i, "from"], c(1, 2)] <- newCoor[["from"]]
      nodeXYout[edgeL_link[i, "to"], c(1, 2)] <- newCoor[["to"]]
    }
  }
  return(nodeXYout[, c(1, 2), drop = FALSE])
}

# 2D Points P=[x,y] and R=[x,y] are arbitrary points on line,
# Q=[x,y] is point for which we want to find reflection
# returns solution vector [x,y]
# Q' = Q + 2(I - d^2*v*t(v))(P - Q)
# Where column vector v=R-P is parallel to line but is not unit (like n),
# P and R are two arbitrary line points; the v*t(v) is outer product;
# I is identity matrix; the d=1/sqrt(vx*vx + vy*vy) is inverse length of v.
# N is the tension strength.
mirrorP <- function(Q, P, R, N) {
  I <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
  n <- R - P
  d2 <- 1 / sum(n^2)
  as.numeric(Q + 2 * N * (I - d2 * n %*% t(n)) %*% (P - Q))
}

bezier0 <- function(t, ctrNodes) {
  x <- 0
  y <- 0
  n <- nrow(ctrNodes) - 1
  for (i in seq.int(nrow(ctrNodes))) {
    index <- i - 1
    if (!index) {
      x <- x + ctrNodes[i, 1, drop = TRUE] * ((1 - t)^(n - index)) * (t^index)
      y <- y + ctrNodes[i, 2, drop = TRUE] * ((1 - t)^(n - index)) * (t^index)
    } else {
      x <- x + factorial(n) / factorial(index) / factorial(n - index) * ctrNodes[i, 1] *
        ((1 - t)^(n - index)) * (t^index)
      y <- y + factorial(n) / factorial(index) / factorial(n - index) * ctrNodes[i, 2] *
        ((1 - t)^(n - index)) * (t^index)
    }
  }
  return(c(x, y))
}

bezier <- function(coefs, center, r_coefs, r, evaluation = 100, w, method = 1) {
  stopifnot("Dimention of coefs need to be 2" = length(dim(coefs)) == 2)
  stopifnot("Dimention of center need to be 2" = length(dim(center)) == 2)
  # mirror center
  center <- colMeans(center)
  # N is the strength of the bezier curve
  # strength is determined by the distance from the center of (p1, p2)
  # to the sum of perimeter of both nodes
  if (method == 2) {
    if (r == 0) {
      N <- min(w, 10)
    } else {
      # w is the ratio of distance to both node width.
      # distance of both nodes center points
      N <- pointsDistance(
        coefs[1, , drop = TRUE],
        coefs[nrow(coefs), , drop = TRUE]
      )
      # distance of the center of nodes center points to sub-cluster center
      Nr <- sqrt((mean(coefs[, 1, drop = TRUE]) - center[1])^2 +
                   (mean(coefs[, 2, drop = TRUE]) - center[2])^2)
      # N/sum(2*pi*r_coefs) should be same as w
      N <- max(min(2 * w, r / Nr, 2 * N / sum(2 * pi * r_coefs), na.rm = TRUE), 1.25)
    }
    N <- 2 * N # 4 points in new algorithm need 2 times strengh.
  } else {
    if (r == 0) {
      N <- 2
    } else {
      Nr <- sqrt((mean(coefs[, 1, drop = TRUE]) - center[1])^2 +
                   (mean(coefs[, 2, drop = TRUE]) - center[2])^2)
      N <- r / Nr
    }
  }
  center_mirror <- mirrorP(center,
                           coefs[1, , drop = TRUE],
                           coefs[nrow(coefs), , drop = TRUE],
                           N = N
  )
  half <- floor(nrow(coefs) / 2)
  ctrNodes <- rbind(
    coefs[seq.int(half), , drop = FALSE],
    center_mirror,
    coefs[seq.int(nrow(coefs))[-seq.int(half)], ,
          drop = FALSE
    ]
  )
  xy <- lapply((seq.int(101) - 1) / 100, bezier0, ctrNodes = ctrNodes)
  xy <- do.call(rbind, xy)
  list(x = xy[, 1, drop = TRUE], y = xy[, 2, drop = TRUE])
}

getMirrorPoint <- function(ns, a, w) {
  n <- length(ns$x)
  b <- ifelse(a == 1, 10, n - 10)
  c <- rotatePoint(ns$x[a], ns$y[a], ns$x[b], ns$y[b])
  mirrorP(c(ns$x[b], ns$y[b]), c(ns$x[a], ns$y[a]), c, N = max(log2(w), 1))
}

getPointInNodeCircle <- function(ns, a, c1, r) {
  t2xy <- function(rx, t, x0, y0) list(x = rx * cos(t) + x0, y = rx * sin(t) + y0)
  t <- atan((ns$y[a] - c1[2]) / (ns$x[a] - c1[1]))
  ap <- t2xy(r, t + ifelse(ns$x[a] - c1[1] >= 0, 0, pi), c1[1], c1[2])
  c(ap$x[1], ap$y[1])
}

curveMaker <- function(ns1, ns2, ab, cx, cy, r, w, evaluation = 100,
                       method = 1,
                       ...) {
  x1 <- ns1$x[ab[1]]
  y1 <- ns1$y[ab[1]]
  x2 <- ns2$x[ab[2]]
  y2 <- ns2$y[ab[2]]
  c1 <- c(cx[1], cy[1])
  if (length(r) == 1) {
    c2 <- c1
  } else {
    c2 <- c(cx[2], cy[2])
  }
  if (pointsDistance(c(x1, y1), c1) < r[1]) {
    # find the extension point on the circle
    ep1 <- getPointInNodeCircle(ns1, ab[1], c1, r[1])
  } else {
    ep1 <- getMirrorPoint(ns1, ab[1], w)
  }
  r2 <- ifelse(length(r) == 1, r[1], r[2])
  if (pointsDistance(c(x2, y2), c2) < r2) {
    ep2 <- getPointInNodeCircle(ns2, ab[2], c2, r2)
  } else {
    ep2 <- getMirrorPoint(ns2, ab[2], w)
  }
  if (method[1] == 2) {
    coefs <- c(
      x1, y1,
      ep1[1], ep1[2],
      ep2[1], ep2[2],
      x2, y2
    )
  } else {
    coefs <- c(
      x1, y1,
      x2, y2
    )
  }
  coefs <- matrix(coefs, ncol = 2, byrow = TRUE)
  xy <- bezier(
    coefs = coefs,
    center = matrix(c(cx, cy), ncol = 2),
    r_coefs = c(
      pointsDistance(c(x1, y1), c1),
      pointsDistance(c(x2, y2), c2)
    ),
    r = ifelse(length(r) == 1, r, 0), w = w,
    evaluation = evaluation,
    method = method[1]
  )
  return(xy)
}

archPlot <- function(x, y, r, angle = 300, init.angle = 0, length.out = 100, bck = FALSE) {
  if (any(is.na(init.angle))) {
    init.angle[is.na(init.angle)] <- 0
  }
  gamma <- 2 * pi * angle * c(1, 2) / 360 + init.angle * pi / 180
  t2xy <- function(rx, t, x0, y0) list(x = rx * cos(t) + x0, y = rx * sin(t) + y0)
  P <- t2xy(r, seq.int(gamma[1], gamma[2], length.out = length.out), x, y)
  # The first 10 points and last 10 points use the mirror points
  if (length.out > 60 && !bck) {
    rr <- 15
    N <- .6
    for (i in seq.int(rr)) {
      xy <- mirrorP(
        Q = c(P$x[i], P$y[i]),
        P = c(P$x[rr + 1], P$y[rr + 1]),
        R = c(P$x[rr + 2], P$y[rr + 2]),
        N = N
      )
      P$x[i] <- xy[1]
      P$y[i] <- xy[2]
    }
    ll <- length.out - rr
    for (i in seq.int(length.out)[-seq.int(ll)]) {
      xy <- mirrorP(
        Q = c(P$x[i], P$y[i]),
        P = c(P$x[ll - 1], P$y[ll - 1]),
        R = c(P$x[ll - 2], P$y[ll - 2]),
        N = N
      )
      P$x[i] <- xy[1]
      P$y[i] <- xy[2]
    }
  }
  return(P)
}

getCutPos <- function(x, breaks, n) {
  x[x <= min(breaks)] <- min(breaks) + .1
  x[x > max(breaks)] <- max(breaks)
  at <- as.numeric(cut(x,
                       breaks = breaks,
                       labels = seq.int(n)
  ))
  return(at)
}

getSrt <- function(x, y) {
  ## length of x, y must be 2
  srt <- ifelse(diff(x)[1] == 0, 0, 180 / pi * atan(diff(y)[1] / diff(x)[1]))
  if (is.na(srt)[1]) srt <- 0
  return(srt)
}

rotatePoint <- function(x1, y1, x2, y2) {
  c(-(y2 - y1) + x1, x2 - x1 + y1)
}

prettyMark <- function(x, u) {
  ## x: markers
  ## u: intervals
  div <- findInterval(x, c(0, 1e3, 1e6, 1e9, 1e12))
  un <- findInterval(u, c(0, 1e3, 1e6, 1e9, 1e12))
  digit <- u / 10^(3 * (un - 1))
  paste0(
    round(x / 10^(3 * (div - 1)), ifelse(digit == 1, 0, 1)),
    c("", "K", "M", "G", "T")[div]
  )
}

#' @importFrom BiocGenerics strand<-
#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges coverage Views ranges
resampleDataByFun <- function(fromGR, targetGR, FUN = viewMeans, dropZERO = TRUE,
                              ...) {
  ## resample the range
  strand(targetGR) <- "*"
  seqn <- as.character(seqnames(targetGR))[1]
  stopifnot(
    "Only one seqlevel is supported." =
      all(as.character(seqnames(targetGR)) ==
            seqn)
  )
  stopifnot("score" %in% colnames(mcols(fromGR)))
  strand(fromGR) <- "*"
  fromGR <- subsetByOverlaps(fromGR, targetGR)
  cvg <- coverage(fromGR, weight = fromGR$score)
  x <- cvg[[seqn]]
  .vw <- Views(x, start = ranges(targetGR))
  targetGR$score <- FUN(.vw, ...)
  targetGR$score[is.na(targetGR$score)] <- 0
  if (dropZERO) targetGR <- targetGR[targetGR$score != 0]
  orderedGR(targetGR)
}

breakPointByBin <- function(x, y, start, end, seqn) {
  if (length(x) > 2) {
    n <- length(x)
  } else {
    n <- 11
    x <- seq(x[1], x[2], length.out = n)
    y <- seq(y[1], y[2], length.out = n)
  }
  rg <- seq(start, end, length.out = n)
  GRanges(seqn, IRanges(start = rg[-n], end = rg[-1]),
          x0 = x[-n], x1 = x[-1],
          y0 = y[-n], y1 = y[-1]
  )
}

#' @importFrom IRanges findOverlaps
#' @importFrom BiocGenerics width
#' @importFrom S4Vectors queryHits subjectHits
findOlPos <- function(query, subject) {
  stopifnot(all(width(query) == 1))
  ol <- findOverlaps(query, subject, type = "within")
  ol <- split(subjectHits(ol), queryHits(ol))
  ol <- vapply(ol, function(.ol) {
    .ol[ceiling(length(.ol) / 2)]
  }, numeric(1L))
  ol[order(as.numeric(names(ol)))]
}

#' @importFrom BiocGenerics start width
calTickPos <- function(feature.tick, curve_gr, arrowLen, kd = 2, rate = 72) {
  stopifnot(is(arrowLen, "unit"))
  ol <- findOlPos(feature.tick, curve_gr)
  a <- start(feature.tick)[as.numeric(names(ol))]
  ratio <- (a - start(curve_gr)[ol]) / width(curve_gr[ol])
  x1 <- curve_gr$x0[ol] + (curve_gr$x1 - curve_gr$x0)[ol] * ratio
  y1 <- curve_gr$y0[ol] + (curve_gr$y1 - curve_gr$y0)[ol] * ratio
  if (kd == 3) {
    z1 <- curve_gr$z0[ol] + (curve_gr$z1 - curve_gr$z0)[ol] * ratio
  }
  srt <- atan((curve_gr$y1 - curve_gr$y0)[ol] / (curve_gr$x1 - curve_gr$x0)[ol]) + pi / 2
  srt[is.na(srt)] <- 0
  tickLen <- grid::convertUnit(arrowLen,
                               unitTo = "native", valueOnly = TRUE
  ) / rate
  if (kd == 2) {
    x2 <- x1 + tickLen * cos(srt)
    y2 <- y1 + tickLen * sin(srt)
    x3 <- x1 + 2 * tickLen * cos(srt)
    y3 <- y1 + 2 * tickLen * sin(srt)
  } else {
    # when kd==3
    return(
      list(
        x1 = x1, y1 = y1, z1 = z1,
        x2 = x1, y2 = y1, z2 = z1 + tickLen / 2,
        x3 = x1, y3 = y1, z3 = z1 + tickLen,
        srt = srt,
        id = as.numeric(names(ol)),
        ol = ol
      )
    )
  }
  list(
    x1 = x1, y1 = y1, x2 = x2, y2 = y2,
    x3 = x3, y3 = y3,
    srt = srt,
    id = as.numeric(names(ol)),
    ol = ol
  )
}

#' @importFrom IRanges countOverlaps
#' @importFrom BiocGenerics start<- width<- end<- strand
#' start end
#' @importFrom utils head
calGenePos <- function(fgf, curve_gr, arrowLen, kd = 2, rate = 72) {
  if(!is(fgf, 'GRanges')){
    return(NULL)
  }
  ## subsetByOverlaps will not work for unit metadata
  olcnt <- countOverlaps(fgf, curve_gr, ignore.strand = TRUE)
  if (sum(olcnt > 0) == 0) {
    return(NULL)
  }
  fgf <- fgf[olcnt > 0]
  fgf <- sort(fgf)
  s <- e <- fgf
  rg <- range(curve_gr)
  missing_start <- start(s) < start(rg)
  missing_end <- end(e) > end(rg)
  start(s)[missing_start] <- start(rg) + 1
  width(s) <- 1
  end(e)[missing_end] <- end(rg) - 1
  start(e) <- end(e)
  ol_s <- calTickPos(s, curve_gr, arrowLen, kd = kd, rate = rate)
  ol_e <- calTickPos(e, curve_gr, arrowLen, kd = kd, rate = rate)
  keep <- intersect(ol_s$id, ol_e$id) ## will delete some small element
  ol_s <- lapply(ol_s, function(.ele) {
    .ele[ol_s$id %in% keep]
  })
  ol_e <- lapply(ol_e, function(.ele) {
    .ele[ol_e$id %in% keep]
  })
  fgf <- fgf[keep]
  via_points <- mapply(seq, ol_s$ol, ol_e$ol, SIMPLIFY = FALSE)
  via_points <- lapply(via_points, function(.ele) {
    .ele[-unique(c(1, length(.ele)))]
  })
  via_points <- lapply(via_points, function(.ele) {
    curve_gr[.ele]
  })
  x1 <- mapply(function(p1, ps, pn) {
    if (length(ps) > 0) {
      c(p1, ps$x0, ps$x1[length(ps)], pn)
    } else {
      c(p1, pn)
    }
  }, ol_s$x1, via_points, ol_e$x1, SIMPLIFY = FALSE)
  y1 <- mapply(function(p1, ps, pn) {
    if (length(ps) > 0) {
      c(p1, ps$y0, ps$y1[length(ps)], pn)
    } else {
      c(p1, pn)
    }
  }, ol_s$y1, via_points, ol_e$y1, SIMPLIFY = FALSE)
  if (kd == 3) {
    z1 <- mapply(function(p1, ps, pn) {
      if (length(ps) > 0) {
        c(p1, ps$z0, ps$z1[length(ps)], pn)
      } else {
        c(p1, pn)
      }
    }, ol_s$z1, via_points, ol_e$z1, SIMPLIFY = FALSE)
  }
  # start points
  neg_strand <- as.character(strand(fgf)) == "-"
  x2 <- ifelse(neg_strand,
               ol_e$x1, ol_s$x1
  )
  y2 <- ifelse(neg_strand,
               ol_e$y1, ol_s$y1
  )
  x3 <- ifelse(neg_strand,
               ol_e$x3, ol_s$x3
  )
  y3 <- ifelse(neg_strand,
               ol_e$y3, ol_s$y3
  )
  if (kd == 3) {
    z2 <- ifelse(neg_strand,
                 ol_e$z1, ol_s$z1
    )
    z3 <- ifelse(neg_strand,
                 ol_e$z3, ol_s$z3
    )
  }
  getFirst2EleDiff <- function(.ele, isNeg) {
    if (isNeg) {
      .ele[length(.ele) - 1] - .ele[length(.ele)]
    } else {
      .ele[2] - .ele[1]
    }
  }
  x0diff <- mapply(getFirst2EleDiff, x1, neg_strand)
  y0diff <- mapply(getFirst2EleDiff, y1, neg_strand)
  srt <- atan(y0diff / x0diff) + ifelse(x0diff < 0, pi, 0)
  srt[is.na(srt)] <- 0
  if (kd == 3) {
    x4 <- mapply(x1, neg_strand, FUN = function(.ele, ns) {
      if (ns) .ele <- rev(.ele)
      head(.ele, n = 10)
    }, SIMPLIFY = FALSE)
    y4 <- mapply(y1, neg_strand, FUN = function(.ele, ns) {
      if (ns) .ele <- rev(.ele)
      head(.ele, n = 10)
    }, SIMPLIFY = FALSE)
    z4 <- mapply(rep, z3, lengths(x4), SIMPLIFY = FALSE)
    return(
      list(
        xs = x1, ys = y1, zs = z1,
        x1 = x2, y1 = y2, z1 = z2,
        x2 = x3, y2 = y3, z2 = z3,
        x3 = x4, y3 = y4, z3 = z4,
        srt = srt,
        fgf = fgf,
        x0diff = x0diff,
        missing_start = ifelse(neg_strand, missing_end, missing_start)
      )
    )
  }
  aLvalue <- abs(convertUnit(arrowLen, unitTo = "native", valueOnly = TRUE)) /
    rate * 5
  x4 <- x3 + aLvalue * cos(srt)
  y4 <- y3 + aLvalue * sin(srt)
  list(
    xs = x1, ys = y1,
    x1 = x2, y1 = y2,
    x2 = x3, y2 = y3,
    x3 = x4, y3 = y4,
    srt = srt,
    fgf = fgf,
    x0diff = x0diff,
    missing_start = ifelse(neg_strand, missing_end, missing_start)
  )
}

#' @importFrom grid convertHeight grobHeight convertWidth grobWidth grobX grobY
objWidth <- function(xlim, ...) {
  convertWidth(grobWidth(...),
               unitTo = "npc", valueOnly = TRUE
  ) * diff(xlim)
}
objHeight <- function(ylim, ...) {
  convertHeight(grobHeight(...),
                unitTo = "npc", valueOnly = TRUE
  ) * diff(ylim)
}
objX <- function(theta, ...) {
  convertWidth(grobX(..., theta = theta),
               unitTo = "npc", valueOnly = TRUE
  )
}
objY <- function(theta, ...) {
  convertHeight(grobY(..., theta = theta),
                unitTo = "npc", valueOnly = TRUE
  )
}

#' @importFrom grid convertX convertWidth grobWidth convertHeight grobHeight
getFourPoints <- function(obj, x_r, y_r) {
  x <- convertX(obj$x, unitTo = "native", valueOnly = TRUE)
  y <- convertX(obj$y, unitTo = "native", valueOnly = TRUE)
  w <- convertWidth(grobWidth(obj), unitTo = "native", valueOnly = TRUE) / 2
  h <- convertHeight(grobHeight(obj), unitTo = "native", valueOnly = TRUE) / 2
  left <- findInterval(x - w, x_r)
  right <- findInterval(x + w, x_r)
  bottom <- findInterval(y - w, y_r)
  top <- findInterval(y + w, y_r)
  return(list(left = left, right = right, bottom = bottom, top = top))
}
safeSeq <- function(range, length) {
  x <- seq(range[1], range[2], length.out = length)
  d <- diff(x)[1] / 100
  x[1] <- x[1] - d
  x[length(x)] <- x[length(x)] + d
  return(x)
}
#' @importFrom grid convertX convertY
getObjsPos <- function(objs, xlim, ylim, res_row, res_col, resolution = 10) {
  x_r <- safeSeq(xlim, res_row)
  y_r <- safeSeq(ylim, res_col)
  if (is(objs, "grob")) {
    objs <- list(objs)
  }
  coors <- lapply(objs, function(obj) {
    if (inherits(obj, c("text", "rect", "polygon"))) {
      rect <- getFourPoints(obj, x_r, y_r)
      if (any(is.na(c(rect$left, rect$right, rect$top, rect$bottom)))) {
        ## points contain NA values
        return(data.frame(x = NULL, y = NULL))
      }
      return(data.frame(
        x = rep(seq(rect$left, rect$right),
                each = rect$top - rect$bottom + 1
        ),
        y = rep(
          seq(rect$bottom, rect$top),
          rect$right - rect$left + 1
        )
      ))
    }
    if (is(obj, "lines")) {
      obj.x <- convertX(obj$x, unitTo = "native", valueOnly = TRUE)
      obj.y <- convertY(obj$y, unitTo = "native", valueOnly = TRUE)
      obj.x <- findInterval(obj.x, x_r)
      obj.y <- findInterval(obj.y, y_r)
      if (any(is.na(obj.x)) || any(is.na(obj.y))) {
        ## points contain NA values
        return(data.frame(x = NULL, y = NULL))
      }
      return(data.frame(
        x = seq.int(obj.x[1], obj.x[2], length.out = resolution),
        y = seq.int(obj.y[1], obj.y[2], length.out = resolution)
      ))
    }
    ## others not support
    return(data.frame(x = NULL, y = NULL))
  })
  coors <- unique(do.call(rbind, coors))
  coors[coors < 1] <- 1
  coors$x[coors$x > res_row] <- res_row
  coors$y[coors$y > res_col] <- res_col
  unique(coors)
}

getIndexInMatrix <- function(coors, nrow) {
  sort(coors$x + (coors$y - 1) * nrow)
}
## store the points occupied by objects
addObjCoor <- function(objCoor, objs, xlim, ylim) {
  ## check object coor and set it to 1
  coors <- getObjsPos(objs, xlim, ylim, nrow(objCoor), ncol(objCoor))
  objCoor[getIndexInMatrix(coors, nrow(objCoor))] <- 1
  objCoor
}
getOverlapCoor <- function(objCoor, objs, xlim, ylim, force = 6) {
  ##  check object coor
  coors <- getObjsPos(objs, xlim, ylim, nrow(objCoor), ncol(objCoor))
  ## check if 1/force percent of the position is 1
  isOccupied <- objCoor[getIndexInMatrix(coors, nrow(objCoor))]
  out <- length(isOccupied) / sum(isOccupied) < force
  if (!is.logical(out)) out <- FALSE
  if (is.na(out)) out <- FALSE
  return(out)
}
## TODO fix the algorithm by the center of the cluster.
safeObjCoor <- function(objCoor, obj, x, y, xlim, ylim, logic = TRUE, force = 6) {
  ol <- getOverlapCoor(objCoor, obj, xlim, ylim, force = force)
  if (logic) {
    return(ol)
  }
  if (is.na(x) || is.na(y)) {
    return(c(x, y))
  }
  x_r <- safeSeq(xlim, nrow(objCoor))
  y_r <- safeSeq(ylim, ncol(objCoor))
  x0 <- x
  y0 <- y
  new_obj <- obj
  for (i in seq.int(force)) {
    if (!ol) {
      return(c(x, y))
    } else {
      xat <- findInterval(x, x_r)
      yat <- findInterval(y, y_r)
      ## get the maximal 0s in all direction
      rect <- getFourPoints(new_obj, x_r, y_r)
      x_w <- ceiling((rect$right - rect$left + 1) / 2)
      y_h <- ceiling((rect$top - rect$bottom + 1) / 2)
      all_pos <- list(
        "left" = list(
          left = max(rect$left - x_w, 1),
          right = max(rect$right - x_w, 1),
          top = rect$top, bottom = rect$bottom
        ),
        "topleft" = list(
          left = max(rect$left - x_w, 1),
          right = max(rect$right - x_w, 1),
          top = min(rect$top + y_h, ncol(objCoor)),
          bottom = min(rect$bottom + y_h, ncol(objCoor))
        ),
        "top" = list(
          left = rect$left, right = rect$right,
          top = min(rect$top + y_h, ncol(objCoor)),
          bottom = min(rect$bottom + y_h, ncol(objCoor))
        ),
        "topright" = list(
          left = min(rect$left + x_w, nrow(objCoor)),
          right = min(rect$right + x_w, nrow(objCoor)),
          top = min(rect$top + y_h, ncol(objCoor)),
          bottom = min(rect$bottom + y_h, ncol(objCoor))
        ),
        "right" = list(
          left = min(rect$left + x_w, nrow(objCoor)),
          right = min(rect$right + x_w, nrow(objCoor)),
          top = rect$top, bottom = rect$bottom
        ),
        "rightbottom" = list(
          left = min(rect$left + x_w, nrow(objCoor)),
          right = min(rect$right + x_w, nrow(objCoor)),
          top = max(rect$top - y_h, 1),
          bottom = max(rect$bottom - y_h, 1)
        ),
        "bottom" = list(
          left = rect$left, right = rect$right,
          top = max(rect$top - y_h, 1),
          bottom = max(rect$bottom - y_h, 1)
        ),
        "leftbottom" = list(
          left = max(rect$left - x_w, 1),
          right = max(rect$right - x_w, 1),
          top = max(rect$top - y_h, 1),
          bottom = max(rect$bottom - y_h, 1)
        )
      )
      all_pos_empty_count <- vapply(all_pos, function(.pos) {
        sum(as.numeric(objCoor[
          seq(.pos$left, .pos$right),
          seq(.pos$bottom, .pos$top)
        ] == 1))
      }, numeric(1L))
      newAt <- all_pos[[which.min(all_pos_empty_count)[1]]]
      newXat <- ceiling((newAt$right + newAt$left + 1) / 2)
      newYat <- ceiling((newAt$top + newAt$bottom + 1) / 2)
      if (is.na(newXat) || is.na(newYat)) {
        return(c(x0, y0))
      }
      x <- ifelse(newXat == 1, xlim[1],
                  ifelse(newXat >= length(x_r), xlim[2], x_r[newXat])
      )
      y <- ifelse(newYat == 1, ylim[1],
                  ifelse(newYat >= length(y_r), ylim[2], y_r[newYat])
      )
      new_obj$x <- unit(x, units = "native")
      new_obj$y <- unit(y, units = "native")
      ol <- getOverlapCoor(objCoor, new_obj, xlim, ylim, force = force)
    }
  }
  return(c(x, y))
}
#' @importFrom grid textGrob grid.segments gpar grid.lines linesGrob grid.text
#' grid.draw arrow grid.points
plotBouquet <- function(pP, fgf, genomicSigs, signalTransformFun,
                        lwd.backbone, col.backbone,
                        lwd.maxGenomicSigs,
                        reverseGenomicSigs,
                        col.backbone_background,
                        alpha.backbone_background,
                        lwd.gene,
                        coor_mark_interval = 1e5,
                        col.coor = "black",
                        show_coor = TRUE,
                        coor_tick_unit = 1e3,
                        label_gene = TRUE,
                        col.tension_line = "black",
                        lwd.tension_line = 1,
                        safe_text_force = 6,
                        arrowLen, rate = 9, kd = 2,
                        xlim, ylim) {
  stopifnot(is.function(signalTransformFun))
  ## split the canvas by safe_text_force parameter
  res_row <- ceiling(abs(diff(xlim)) / objWidth(xlim, textGrob("w"))) * safe_text_force
  res_col <- ceiling(abs(diff(ylim)) / objHeight(xlim, textGrob("f"))) * safe_text_force
  objCoor <- matrix(0, nrow = res_row, ncol = res_col)
  if(length(fgf)>0){
    if(is.character(fgf)){
      seqn <- fgf
    }else if(is(fgf, 'GRanges')){
      seqn <- as.character(seqnames(fgf[1]))
    }else{
      stop('can not get seqname.')
    }
  }else{
    stop('can not get seqname.')
  }
  
  curve_gr <- lapply(pP, function(.ele) {
    .ele$seqn <- seqn
    do.call(breakPointByBin, .ele)
  })
  curve_gr <- unlist(GRangesList(curve_gr))
  missing_genomicSigs <- FALSE
  if (missing(genomicSigs)) {
    missing_genomicSigs <- TRUE
  } else {
    if (length(genomicSigs) == 0) {
      missing_genomicSigs <- TRUE
    }
  }
  if (!missing_genomicSigs) {
    if (is(genomicSigs, "track")) {
      if (genomicSigs$format == "WIG") {
        genomicSigs <- parseWIG(
          trackScore = genomicSigs,
          chrom = seqn,
          from = start(range(curve_gr)),
          to = end(range(curve_gr))
        )
      }
      genomicSigs <- genomicSigs$dat
    }
    stopifnot(is(genomicSigs, "GRanges"))
    stopifnot("score" %in% colnames(mcols(genomicSigs)))
    genomicSigs <- resampleDataByFun(genomicSigs, curve_gr,
                                     dropZERO = FALSE,
                                     na.rm = TRUE
    )
    genomicSigScoreRange <- quantile(signalTransformFun(genomicSigs$score),
                                     probs = c(.1, .99)
    )
    if (genomicSigScoreRange[1] == genomicSigScoreRange[2]) {
      genomicSigScoreRange <- range(signalTransformFun(genomicSigs$score))
    }
    if (genomicSigScoreRange[1] != genomicSigScoreRange[2]) {
      genomicSigBreaks <- c(
        -1,
        seq(genomicSigScoreRange[1],
            genomicSigScoreRange[2],
            length.out = lwd.maxGenomicSigs - 1
        ),
        max(signalTransformFun(genomicSigs$score)) + 1
      )
      genomicSiglabels <- seq_along(genomicSigBreaks)[-length(genomicSigBreaks)]
      if (reverseGenomicSigs) {
        genomicSiglabels <- rev(genomicSiglabels)
      }
      genomicSigs$lwd <- as.numeric(as.character(
        cut(signalTransformFun(genomicSigs$score),
            breaks = genomicSigBreaks,
            labels = genomicSiglabels
        )
      ))
      ## add genomic signals
      grid.segments(curve_gr$x0, curve_gr$y0,
                    curve_gr$x1, curve_gr$y1,
                    default.units = "native",
                    gp = gpar(
                      lwd = lwd.backbone + genomicSigs$lwd,
                      col = col.backbone_background,
                      alpha = alpha.backbone_background,
                      lineend = 1
                    )
      )
    }
  } else {
    grid.lines(c(curve_gr$x0, curve_gr$x1[length(curve_gr)]),
               c(curve_gr$y0, curve_gr$y1[length(curve_gr)]),
               default.units = "native",
               gp = gpar(
                 lwd = lwd.maxGenomicSigs / 2,
                 lty = 1,
                 col = col.backbone_background
               )
    )
  }
  ## add backbone
  grid.lines(c(curve_gr$x0, curve_gr$x1[length(curve_gr)]),
             c(curve_gr$y0, curve_gr$y1[length(curve_gr)]),
             default.units = "native",
             gp = gpar(
               lwd = lwd.backbone,
               lty = 1,
               col = col.backbone
             )
  )
  ## add backbone to avoidance list
  tgs <- mapply(function(x0, x1, y0, y1) {
    linesGrob(
      x = c(x0, x1),
      y = c(y0, y1),
      default.units = "native",
      gp = gpar(
        lwd = lwd.backbone,
        lty = 1
      )
    )
  }, curve_gr$x0, curve_gr$x1, curve_gr$y0, curve_gr$y1, SIMPLIFY = FALSE)
  objCoor <- addObjCoor(objCoor, tgs, xlim, ylim)
  
  ## add genomic coordinates
  if (show_coor) {
    r_tick <- range(curve_gr)
    end(r_tick) <- ceiling(end(r_tick) / coor_tick_unit) * coor_tick_unit
    start(r_tick) <- floor(start(r_tick) / coor_tick_unit) * coor_tick_unit
    strand(r_tick) <- "*"
    feature.tick <- GenomicRanges::slidingWindows(r_tick, width = 1, step = coor_tick_unit)[[1]]
    feature.tick$col <- col.tension_line
    tick.xy <- calTickPos(feature.tick, curve_gr, arrowLen, rate = rate, kd = kd)
    grid.segments(tick.xy$x1, tick.xy$y1,
                  tick.xy$x2, tick.xy$y2,
                  default.units = "native",
                  gp = gpar(
                    col = col.tension_line,
                    lwd = lwd.tension_line
                  )
    )
    if (coor_mark_interval) {
      feature.tick.mark <- feature.tick[tick.xy$id]
      mark <- start(feature.tick.mark) / coor_mark_interval
      keep <- which(mark == round(mark) & !is.na(tick.xy$x3) & !is.na(tick.xy$y3))
      if (length(keep) > 0) {
        grid.segments(tick.xy$x1[keep], tick.xy$y1[keep],
                      tick.xy$x3[keep], tick.xy$y3[keep],
                      default.units = "native",
                      gp = gpar(
                        col = col.coor,
                        lwd = lwd.tension_line
                      )
        )
        for (k in keep) {
          lab <- prettyMark(
            start(feature.tick.mark[k]),
            coor_mark_interval
          )
          tg <- textGrob(
            label = lab,
            x = tick.xy$x3[k], y = tick.xy$y3[k],
            default.units = "native",
            gp = gpar(col = col.coor),
            just = c(.5, -.2),
            rot = 180 * tick.xy$srt[k] / pi - 90
          )
          lab.xy <- safeObjCoor(objCoor,
                                x = tick.xy$x3[k],
                                y = tick.xy$y3[k],
                                obj = tg,
                                xlim = xlim,
                                ylim = ylim,
                                logic = FALSE,
                                force = safe_text_force
          )
          if ((!is.na(lab.xy[1]) && lab.xy[1] != tick.xy$x3[k]) ||
              (!is.na(lab.xy[2]) && lab.xy[2] != tick.xy$y3[k])) {
            tg <- grid.text(
              label = lab,
              x = lab.xy[1], y = lab.xy[2],
              default.units = "native",
              just = c(.5, -.2),
              rot = 180 * tick.xy$srt[k] / pi - 90,
              gp = gpar(col = col.coor)
            )
            grid.segments(tick.xy$x3[k], tick.xy$y3[k],
                          lab.xy[1],
                          lab.xy[2],
                          default.units = "native",
                          gp = gpar(
                            col = col.coor,
                            lwd = .5,
                            lty = 4
                          )
            )
          } else {
            grid.draw(tg)
          }
          objCoor <- addObjCoor(objCoor, tg, xlim, ylim)
        }
      }
    }
  }
  ## add gene annotation
  genePos <- calGenePos(fgf, curve_gr, arrowLen, rate = rate, kd = kd)
  if (length(genePos) > 0) {
    null <- mapply(function(x, y, col, lwd) {
      grid.lines(x, y,
                 default.units = "native",
                 gp = gpar(col = col, lwd = lwd)
      )
    }, x = genePos$xs, y = genePos$ys, col = genePos$fgf$col, lwd = lwd.gene)
    grid.segments(
      x0 = genePos$x1, y0 = genePos$y1,
      x1 = genePos$x2, y1 = genePos$y2,
      default.units = "native",
      gp = gpar(col = genePos$fgf$col)
    )
    isGene <- genePos$fgf$type %in% "gene" & !genePos$missing_start
    if (any(isGene)) {
      grid.segments(
        x0 = genePos$x2[isGene],
        x1 = genePos$x3[isGene],
        y0 = genePos$y2[isGene],
        y1 = genePos$y3[isGene],
        default.units = "native",
        gp = gpar(
          col = genePos$fgf$col[isGene],
          fill = genePos$fgf$col[isGene]
        ),
        arrow = arrow(
          angle = 15,
          type = "closed",
          length = arrowLen
        )
      )
    }
    notGene <- (!genePos$fgf$type %in% "gene") & (!genePos$missing_start)
    if (any(notGene)) {
      grid.points(
        x = genePos$x1[notGene],
        y = genePos$y1[notGene],
        pch = genePos$fgf$pch[notGene],
        size = genePos$fgf$size[notGene],
        default.units = "native",
        gp = gpar(
          col = genePos$fgf$col[notGene],
          fill = genePos$fgf$col[notGene]
        )
      )
    }
    if (label_gene) {
      for (k in seq_along(genePos$fgf)) {
        if (is.na(genePos$x2[k])) next
        if (is.na(genePos$fgf$label[k])) next
        if (genePos$fgf$label[k] == "") next
        vadj <- -.2
        hadj <- 0
        srt <- 180 * genePos$srt[k] / pi
        if (srt > 90 && srt < 270) {
          srt <- srt - 180
          hadj <- 1
        }
        tg <- textGrob(
          label = genePos$fgf$label[k],
          x = genePos$x2[k], y = genePos$y2[k],
          default.units = "native",
          just = c(hadj, vadj),
          rot = srt,
          gp = gpar(
            col = genePos$fgf$col[k],
            cex = genePos$fgf$cex
          )
        )
        lab.xy <- safeObjCoor(objCoor,
                              x = genePos$x2[k],
                              y = genePos$y2[k],
                              obj = tg,
                              xlim = xlim,
                              ylim = ylim,
                              logic = FALSE,
                              force = safe_text_force
        )
        if (!is.na(lab.xy[1]) && lab.xy[1] != genePos$x2[k]) {
          tg <- grid.text(
            label = genePos$fgf$label[k],
            x = lab.xy[1], y = lab.xy[2],
            default.units = "native",
            just = c(hadj, vadj),
            gp = gpar(
              col = genePos$fgf$col[k],
              cex = genePos$fgf$cex
            )
          )
          grid.segments(genePos$x2[k], genePos$y2[k],
                        lab.xy[1],
                        lab.xy[2],
                        default.units = "native",
                        gp = gpar(
                          col = genePos$fgf$col[k],
                          lwd = .5,
                          lty = 4
                        )
          )
        } else {
          grid.draw(tg)
        }
        objCoor <- addObjCoor(objCoor, tg, xlim, ylim)
      }
    }
  }
  return(objCoor)
}