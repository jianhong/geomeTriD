#' plot GInteractions
#' @description plot graph for GInteractions
#' @param gi An object of \link[InteractionSet:GInteractions-class]{GInteractions}
#' @param range The region to plot. an object of \link[GenomicRanges:GRanges-class]{GRanges}
#' @param feature.gr The annotation features to be added. An object of \link[GenomicRanges:GRanges-class]{GRanges}.
#' @param genomicSigs The genomic signals. An object of \link[GenomicRanges:GRanges-class]{GRanges} with scores or an object of \link[trackViewer:track]{track}.
#' @param signalTransformFun The transformation function for genomic signals.
#' @param label_region Label the region node or not.
#' @param show_edges Plot the interaction edges or not.
#' @param show_cluster Plot the cluster background or not.
#' @param reverseGenomicSigs Plot the Genomic signals in reverse values.
#' @param coor_tick_unit The bps for every ticks. Default is 1K.
#' @param coor_mark_interval The coordinates marker interval. Numeric(1). Set to 0
#' to turn it off. The default value 1e5 means show coordinates every 0.1M bp.
#' @param show_coor Show coordinates or not.
#' @param label_gene Show gene symbol or not.
#' @param lwd.backbone,lwd.gene,lwd.nodeCircle,lwd.edge,lwd.tension_line,lwd.maxGenomicSigs Line width for the
#' linker, gene, interaction node circle, the dashed line of interaction edges, the tension line and the maximal reversed genomic signal.
#' @param col.backbone,col.backbone_background,col.nodeCircle,col.edge,col.tension_line,col.coor Color
#' for the DNA chain, the compact DNA chain, the node circle, the linker, the tension line and the coordinates marker.
#' @param alpha.backbone_background Alpha channel for transparency of backbone background.
#' @param length.arrow Length of the edges of the arrow head (in inches).
#' @param safe_text_force The loops to avoid the text overlapping.
#' @param method Plot method. Could be 1 or 2.
#' @param doReduce Reduce the GInteractions or not.
#' @param ... Parameter will be passed to \link[igraph:layout_with_fr]{layout_with_fr}.
#' @return  A invisible list with the key points of the plot.
#' @importClassesFrom InteractionSet GInteractions
#' @importMethodsFrom InteractionSet regions anchorIds
#' @importFrom GenomicRanges GRanges GRangesList
#' @importFrom S4Vectors mcols first second
#' @importFrom BiocGenerics sort start<- end<- strand<- width start end
#' @importFrom GenomeInfoDb seqnames
#' @importFrom igraph graph_from_data_frame components layout_with_fr
#' norm_coords V
#' @importFrom plotrix arctext
#' @importFrom stats plogis quantile median
#' @importFrom graphics curve lines par points segments strheight text arrows
#' polygon
#' @importFrom scales rescale
#' @importFrom IRanges IRanges subsetByOverlaps viewMeans countOverlaps reduce
#' findOverlaps distance
#' @importFrom trackViewer parseWIG
#' @importFrom grid unit grid.newpage viewport pushViewport popViewport
#'  stringWidth convertWidth convertHeight grid.circle gpar grid.segments
#'  grid.points grid.text
#' @export
#' @examples
#' library(InteractionSet)
#' gi <- readRDS(system.file("extdata", "gi.rds", package = "trackViewer"))
#' range <- GRanges("chr2", IRanges(234500000, 235000000))
#' library(TxDb.Hsapiens.UCSC.hg19.knownGene)
#' library(org.Hs.eg.db)
#' feature.gr <- genes(TxDb.Hsapiens.UCSC.hg19.knownGene)
#' feature.gr <- subsetByOverlaps(feature.gr, range(regions(gi)))
#' symbols <- mget(feature.gr$gene_id, org.Hs.egSYMBOL, ifnotfound = NA)
#' feature.gr$label[lengths(symbols) == 1] <- unlist(symbols[lengths(symbols) == 1])
#' feature.gr$col <- sample(1:7, length(feature.gr), replace = TRUE)
#' feature.gr$type <- sample(c("cRE", "gene"),
#'   length(feature.gr),
#'   replace = TRUE,
#'   prob = c(0.1, 0.9)
#' )
#' feature.gr$pch <- rep(NA, length(feature.gr))
#' feature.gr$pch[feature.gr$type == "cRE"] <- 11
#' loopBouquetPlot(gi, range, feature.gr)
loopBouquetPlot <- function(gi, range, feature.gr, genomicSigs,
                            signalTransformFun = function(x) {
                              log2(x + 1)
                            },
                            label_region = FALSE, show_edges = TRUE,
                            show_cluster = TRUE,
                            lwd.backbone = 2, col.backbone = "gray",
                            lwd.maxGenomicSigs = 8, reverseGenomicSigs = TRUE,
                            col.backbone_background = "gray70",
                            alpha.backbone_background = 0.5,
                            lwd.gene = 2,
                            lwd.nodeCircle = 1, col.nodeCircle = "#DDDDDD25",
                            lwd.edge = 2, col.edge = "gray80",
                            coor_mark_interval = 1e5, col.coor = "black",
                            show_coor = TRUE,
                            coor_tick_unit = 1e3,
                            label_gene = TRUE,
                            col.tension_line = "black",
                            lwd.tension_line = 1,
                            length.arrow = NULL,
                            safe_text_force = 3,
                            method = 1,
                            doReduce = FALSE,
                            ...) {
  gi <- checkGI(gi)
  stopifnot(is.function(signalTransformFun))
  stopifnot(is.numeric(coor_mark_interval))
  stopifnot(length(coor_mark_interval) == 1)
  if (!missing(range)) {
    stopifnot(is(range, "GRanges"))
    stopifnot(
      "coor_tick_unit is too small." =
        width(range(range))[1] / coor_tick_unit < 1000
    )
    seqn <- as.character(seqnames(range)[1])
    gi <- subsetByOverlaps(gi, ranges = range, ignore.strand = TRUE)
    ol1 <- countOverlaps(first(gi), range)
    ol2 <- countOverlaps(second(gi), range)
    gi <- gi[ol1 > 0 & ol2 > 0]
  } else {
    seqn <- as.character(seqnames(first(gi))[1])
  }
  stopifnot("No interaction data available." = length(gi) > 0)
  gi <- sort(gi)
  if (doReduce) {
    gi <- reduce(gi, ignore.strand = TRUE)
  }
  reg <- regions(gi)
  stopifnot("No interactions detected" = length(reg) > 2)
  names(reg) <- seq_along(reg)
  if (!all(as.character(seqnames(reg)) == as.character(seqnames(reg))[1])) {
    warning("All interaction must within one chromosome.
            Interchromosomal interactions will be dropped.")
    gi <- gi[seqnames(first(gi)) == seqn & seqnames(second(gi)) == seqn]
  }
  ol <- findOverlaps(reg, drop.self = TRUE, drop.redundant = TRUE, minoverlap = 2)
  if (length(ol) > 0) {
    warning("There are overlaps in the input regions. Do reduce now.")
    gi <- reduce(gi)
    reg <- regions(gi)
    stopifnot("No interactions detected" = length(reg) > 2)
    names(reg) <- seq_along(reg)
  }
  feature.gr <- parseFeature(feature.gr = feature.gr, seqn = seqn)

  nodes <- unique(as.character(sort(c(
    anchorIds(gi, type = "first"),
    anchorIds(gi, type = "second")
  ))))
  if (length(nodes) == 0) {
    stop("No interaction data available.")
  }
  ## add genomic coordinates to edge
  d0 <- distance(
    reg[nodes[-length(nodes)]],
    reg[nodes[-1]]
  )
  d0.ups.dws <- width(reg[nodes[-length(nodes)]]) + width(reg[nodes[-1]])
  d <- log10(d0 + 1) + 1
  irq <- quantile(d, probs = c(.25, .75), na.rm = TRUE)
  edgeL_coor <- data.frame(
    from = nodes[-length(nodes)],
    to = nodes[-1],
    weight = max(d) / d
  )
  edgeL_link <- data.frame(
    from = as.character(anchorIds(gi, type = "first")),
    to = as.character(anchorIds(gi, type = "second")),
    weight = gi$score + 2 * diff(irq) + irq[2]
  )

  edgeL <- rbind(edgeL_coor, edgeL_link)
  m_w_reg <- min(width(reg[nodes]))
  nodes <- data.frame(
    names = nodes,
    size = (width(reg[nodes])) / ifelse(m_w_reg == 0, 1, m_w_reg)
  )
  gL <- graph_from_data_frame(d = edgeL_link, directed = FALSE, vertices = nodes)
  cl <- igraph::components(gL, mode = "weak")
  sGnodes <- split(names(cl$membership), cl$membership)
  g <- graph_from_data_frame(d = edgeL, directed = FALSE, vertices = nodes)
  layout <- layout_with_fr(g, ...) ## only layout_with_fr and layout_with_kk work OK
  stopifnot("3 dim is not supported yet" = ncol(layout) == 2)
  nodeXY <- norm_coords(layout, -1, 1, -1, 1)
  rownames(nodeXY) <- nodes$names
  colnames(nodeXY) <- c("X", "Y")
  vertex.factor <- 72
  vertex.size <- 1 / vertex.factor * V(g)$size
  vertex.size[is.na(vertex.size)] <- 1 / vertex.factor
  nodeXY <- fixXY(nodeXY, vertex.size, edgeL_link, lwd = lwd.backbone / 300)
  maxv <- max(vertex.size)
  xlim <- range(nodeXY[, 1, drop = TRUE])
  ylim <- range(nodeXY[, 2, drop = TRUE])
  d_xlim <- diff(xlim) / ifelse(length(sGnodes) > 1, 5, 2)
  d_ylim <- diff(ylim) / ifelse(length(sGnodes) > 1, 5, 2)
  xlim <- c(xlim[1] - d_xlim - maxv, xlim[2] + d_xlim + maxv)
  ylim <- c(ylim[1] - d_ylim - maxv, ylim[2] + d_ylim + maxv)

  nodesSize <- nodes$size

  clusterCenter <- lapply(sGnodes, function(.ele) {
    X <- nodeXY[.ele, "X", drop = TRUE]
    Y <- nodeXY[.ele, "Y", drop = TRUE]
    list(
      x = mean(X), y = mean(Y),
      r = sqrt((diff(range(X) / 2) + median(vertex.size))^2 +
        (diff(range(Y) / 2) + median(vertex.size))^2),
      nodes = .ele
    )
  })

  opar <- par(mar = rep(0, 4) + .1)
  plotPoints <- list()

  grid.newpage()
  vp <- viewport(
    default.units = "native",
    width = unit(min(1, diff(xlim) / diff(ylim)), "snpc"), # aspect ratio preserved
    height = unit(min(1, diff(ylim) / diff(xlim)), "snpc"),
    xscale = xlim, yscale = ylim
  )
  pushViewport(vp)
  on.exit({
    popViewport()
    par(opar)
  })
  if (!is.numeric(length.arrow)) {
    sH <- stringWidth("0")
    arrowLen <- grid::convertUnit(grid::stringHeight("0"), unitTo = "inches")
  } else {
    arrowLen <- length.arrow[1]
  }
  rate <- max(abs(
    c(
      convertWidth(unit(diff(xlim), "native"),
        unitTo = "inch", valueOnly = TRUE
      ),
      convertHeight(unit(diff(ylim), "native"),
        unitTo = "inch", valueOnly = TRUE
      )
    )
  )) /
    convertUnit(arrowLen, unitTo = "inches", valueOnly = TRUE)

  ## make sure the init.angle is facing to next node
  ## make the init.angle is facing outside of the clusterCenter
  init.angle <- lapply(clusterCenter, function(.ele) {
    dX <- nodeXY[.ele$nodes, "X", drop = TRUE] - .ele$x
    dY <- nodeXY[.ele$nodes, "Y", drop = TRUE] - .ele$y
    180 * atan(dY / dX) / pi + ifelse(dX >= 0, 90, -90)
  })
  init.angle <- c(unlist(unname(init.angle))[nodes$names], 0)
  nodeShape <- list() ## the curves connected the nodes
  for (i in seq.int(nrow(nodeXY))) {
    nodeShape[[i]] <- archPlot(nodeXY[i, "X"], nodeXY[i, "Y"], r = nodesSize[i] / vertex.factor, init.angle = init.angle[i])
  }

  getNodesClusters <- function(...) {
    which(vapply(sGnodes, function(.ele) any(... %in% .ele), logical(1L)))
  }
  last_a <- 1
  for (i in seq.int(length(nodeShape))) {
    ## plot the curve connected the nodes
    if (i < length(nodeShape)) {
      ## check the connection points
      if (i == 1) { ## mark the start point
        reg0 <- reg[rownames(nodeXY)[i]]
        this_reg_gap <- GRanges(seqnames(reg0), IRanges(
          end = start(reg0),
          width = 10 * coor_tick_unit
        ))
        if (start(this_reg_gap) < 1) start(this_reg_gap) <- 1
        idx <- c(i, i + 1)
        ab <- getStartConnectionPoints(nodeShape[idx])
        idx <- ifelse(ab[1] == 1, length(nodeShape[[1]]$x), 1)
        thisXY <- plotStartEndPoints(nodeShape[[i]], idx,
          xlim = xlim, ylim = ylim
        )
        plotPoints <- addPoints(
          plotPoints,
          thisXY$x, thisXY$y,
          start(this_reg_gap),
          end(this_reg_gap)
        )
      } else {
        idx <- seq(i, min(i + 2, length(nodeShape)))
        ab <- checkConnectionPoints(nodeShape[c(i, i + 1)], last_a, d[i] > median(d))
      }
      ## The curve will be plot by the center of the subcluster
      ## or the center of the two nodes
      cn <- getNodesClusters(nodes$names[c(i, i + 1)])
      if (length(cn) == 1) {
        cn <- clusterCenter[[cn]]
      } else {
        cn <- lapply(clusterCenter[cn], function(.ele) {
          c(x = .ele$x, y = .ele$y, r = .ele$r)
        })
        cn <- as.data.frame(do.call(rbind, cn))
      }
      reg0 <- reg[rownames(nodeXY)[i]]
      reg1 <- reg[rownames(nodeXY)[i + 1]]
      if (ab[1] == 1) {
        plotPoints <- addPoints(
          plotPoints,
          rev(nodeShape[[i]]$x),
          rev(nodeShape[[i]]$y),
          start(reg0),
          end(reg0)
        )
      } else {
        plotPoints <- addPoints(
          plotPoints,
          nodeShape[[i]]$x,
          nodeShape[[i]]$y,
          start(reg0),
          end(reg0)
        )
      }
      thisXY <- curveMaker(nodeShape[[i]],
        nodeShape[[i + 1]],
        ab,
        cn$x, cn$y,
        r = cn$r,
        w = d0[i] / d0.ups.dws[i],
        evaluation = floor(100 * edgeL_coor$weight[i]),
        method = method
      )
      plotPoints <- addPoints(
        plotPoints,
        thisXY$x, thisXY$y,
        end(reg0),
        start(reg1)
      )
      last_a <- ifelse(ab[2] == 1, length(nodeShape[[i]]$x), 1)
    }
    if (i == length(nodeShape)) { ## mark the end point
      reg0 <- reg[rownames(nodeXY)[i]]
      this_reg_gap <- GRanges(seqnames(reg0), IRanges(end(reg0),
        width = 10 * coor_tick_unit
      ))
      if (last_a == 1) {
        plotPoints <- addPoints(
          plotPoints,
          rev(nodeShape[[i]]$x),
          rev(nodeShape[[i]]$y),
          start(reg0),
          end(reg0)
        )
      } else {
        plotPoints <- addPoints(
          plotPoints,
          nodeShape[[i]]$x,
          nodeShape[[i]]$y,
          start(reg0),
          end(reg0)
        )
      }
      thisXY <- plotStartEndPoints(nodeShape[[i]], last_a,
        xlim = xlim, ylim = ylim,
        start = nodeShape[[i]]$y[1]
      )
      plotPoints <- addPoints(
        plotPoints,
        thisXY$x, thisXY$y,
        start(this_reg_gap),
        end(this_reg_gap)
      )
    }
  }
  ## plot the bacground circle
  if (show_cluster) {
    for (i in seq_along(clusterCenter)) {
      grid.circle(clusterCenter[[i]]$x, clusterCenter[[i]]$y,
        clusterCenter[[i]]$r,
        default.units = "native",
        gp = gpar(
          fill = col.nodeCircle,
          lty = 4, lwd = lwd.nodeCircle
        )
      )
    }
  }
  ## plot the edge lines
  if (show_edges) {
    grid.segments(nodeXY[as.character(edgeL_link$from), "X", drop = TRUE],
      nodeXY[as.character(edgeL_link$from), "Y", drop = TRUE],
      nodeXY[as.character(edgeL_link$to), "X", drop = TRUE],
      nodeXY[as.character(edgeL_link$to), "Y", drop = TRUE],
      default.units = "native",
      gp = gpar(lwd = lwd.edge, lty = 2, col = col.edge)
    )
  }
  ## plot the bouquet
  objCoor <- plotBouquet(
    pP = plotPoints,
    fgf = feature.gr,
    genomicSigs = genomicSigs,
    signalTransformFun = signalTransformFun,
    lwd.backbone = lwd.backbone,
    col.backbone = col.backbone,
    lwd.maxGenomicSigs = lwd.maxGenomicSigs,
    reverseGenomicSigs = reverseGenomicSigs,
    col.backbone_background = col.backbone_background,
    alpha.backbone_background = alpha.backbone_background,
    lwd.gene = lwd.gene,
    coor_mark_interval = coor_mark_interval,
    col.coor = col.coor,
    show_coor = show_coor,
    coor_tick_unit = coor_tick_unit,
    label_gene = label_gene,
    col.tension_line = col.tension_line,
    lwd.tension_line = lwd.tension_line,
    safe_text_force = safe_text_force,
    arrowLen = arrowLen, rate = rate,
    xlim = xlim, ylim = ylim
  )
  stopifnot(is(arrowLen, "unit"))
  if (label_region) {
    ## plot the node dot
    grid.points(nodeXY[, "X"],
      nodeXY[, "Y"],
      pch = 16,
      default.units = "native",
      gp = gpar(
        col = "white",
        cex = nodesSize / strheight("0")
      )
    )
    ## label the node
    grid.text(nodeXY[, "X"], nodeXY[, "Y"],
      default.units = "native",
      label = rownames(nodeXY)
    )
  }

  return(invisible(list(
    plotPoints = plotPoints, feature.gr = feature.gr,
    xlim = xlim, ylim = ylim,
    nodeXY = nodeXY, edgeL_link = edgeL_link,
    clusterCenter = clusterCenter,
    objCoor = objCoor,
    vp = vp
  )))
}
