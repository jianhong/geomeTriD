#' Plot GRanges xyz data in 2d or 3d
#' @description
#' Plot GRanges xyz data with grid or rgl package.
#' @param obj GRanges object with mcols x, y, and/or z
#' @param feature.gr The annotation features to be added. An object of \link[GenomicRanges:GRanges-class]{GRanges}.
#' @param genomicSigs The Genomic signals. An object of \link[GenomicRanges:GRanges-class]{GRanges} with scores or an object of \link[trackViewer:track]{track}.
#' @param region A GRanges object with the region to be plot.
#' @param signalTransformFun The transformation function for genomic signals.
#' @param renderer The renderer of the 3D plots. Could be rgl or threejs.
#' The threejs will create a htmlwidgets. If 'none' is set, a list of object
#' will be returned.
#' @param show_coor Plot ticks in the line to show the DNA compact tension.
#' @param reverseGenomicSigs Plot the genomic signals in reverse values.
#' @param k The dimension of plot. 2: 2d, 3: 3d.
#' @param coor_tick_unit The bps for every ticks. Default is 1K.
#' @param coor_mark_interval The coordinates marker interval. Numeric(1). Set to 0
#' to turn it off. The default value 1e5 means show coordinates every 0.1M bp.
#' @param label_gene Show gene symbol or not.
#' @param lwd.backbone,lwd.gene,lwd.tension_line,lwd.maxGenomicSigs Line width for the
#' linker, gene, interaction node circle, the dashed line of interaction edges, the tension line and the maximal reversed genomic signal.
#' @param col.backbone,col.backbone_background,col.tension_line,col.coor Color
#' for the DNA chain, the compact DNA chain, the node circle, the linker, the tension line and the coordinates marker.
#' @param alpha.backbone_background Alpha channel for transparency of backbone background.
#' @param length.arrow Length of the edges of the arrow head (in inches).
#' @param safe_text_force The loops to avoid the text overlapping.
#' @param square A logical value that controls whether control points for the curve are created city-block fashion or obliquely. See \link[grid]{grid.curve}.
#' @param cluster3Dpoints A logical value that controls whether cluster the points
#' in 3D. It will be ignored when k=2. 
#' @param ... Parameters for \link{create3dGenomicSignals}.
#' @return Coordinates for 2d or a list of threeJsGeometry objects or a
#' htmlwidget.
#' @importFrom stats quantile
#' @importFrom trackViewer parseWIG
#' @importFrom GenomeInfoDb seqnames
#' @importFrom BiocGenerics start<- end<- strand<-
#' @importFrom utils tail
#' @importFrom grDevices colorRampPalette
#' @importFrom grid convertUnit grid.newpage viewport pushViewport popViewport
#' convertWidth convertHeight xsplineGrob grobCoords convertX convertY is.unit
#' @importFrom GenomicRanges GRangesList
#' @export
#' @examples
#' obj <- readRDS(system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds",
#'   package = "geomeTriD"
#' ))
#' feature.gr <- readRDS(system.file("extdata", "4DNFI1UEG1HD.feature.gr.rds",
#'   package = "geomeTriD"
#' ))
#' tjg <- view3dStructure(obj,
#'   k = 3, feature.gr = feature.gr, renderer = "none",
#'   length.arrow = grid::unit(0.000006, "native")
#' )
view3dStructure <- function(obj, feature.gr,
                            genomicSigs,
                            region,
                            signalTransformFun = function(x) {
                              log2(x + 1)
                            },
                            k = 3,
                            renderer = c("rgl", "threejs", "none"),
                            lwd.backbone = 2, col.backbone = "gray",
                            lwd.maxGenomicSigs = 8,
                            reverseGenomicSigs = TRUE,
                            col.backbone_background =
                              if (k == 2) "gray70" else c("gray30", "darkred"),
                            alpha.backbone_background = 0.5,
                            lwd.gene = 3,
                            coor_mark_interval = 5e5, col.coor = "black",
                            show_coor = TRUE,
                            coor_tick_unit = 5e4,
                            label_gene = TRUE,
                            col.tension_line = "black",
                            lwd.tension_line = 1,
                            length.arrow = unit(abs(diff(obj$x)) / 20, "native"),
                            safe_text_force = 3,
                            square = TRUE,
                            cluster3Dpoints = FALSE,
                            ...) {
  stopifnot(is(obj, "GRanges"))
  if(!missing(region)){
    stopifnot(is(region, 'GRanges'))
    obj <- subsetByOverlaps(obj, region)
  }
  checkSignalTransformFun(signalTransformFun)
  stopifnot(is.numeric(k))
  stopifnot(k == 2 || k == 3)
  singleChr <- all(as.character(seqnames(obj)) == as.character(seqnames(obj)[1]))
  if(k==2){
    if(!singleChr){
      stop("Only work for data in a single chromsome.")
    }
  }
  stopifnot(all(c("x", "y") %in% colnames(mcols(obj))))
  if (k == 3) {
    stopifnot("z" %in% colnames(mcols(obj)))
  }
  renderer <- match.arg(renderer)
  stopifnot(is.logical(cluster3Dpoints))
  stopifnot(length(cluster3Dpoints)==1)
  seqn <- as.character(seqnames(obj)[1])
  feature.gr <- parseFeature(
    feature.gr = feature.gr,
    seqn = seqn
  )
  xlim <- range(obj$x, na.rm = TRUE)
  ylim <- range(obj$y, na.rm = TRUE)
  if (length(obj$z) > 0) {
    zlim <- range(obj$z, na.rm = TRUE)
  } else {
    zlim <- c(-1, 1) ## will not use
  }
  d_xlim <- diff(xlim) / 10
  d_ylim <- diff(ylim) / 10
  d_zlim <- diff(zlim) / 10
  xlim <- c(xlim[1] - d_xlim, xlim[2] + d_xlim)
  ylim <- c(ylim[1] - d_ylim, ylim[2] + d_ylim)
  zlim <- c(zlim[1] - d_zlim, zlim[2] + d_zlim)
  scale_factor <- 10^floor(log10(diff(range(c(xlim, ylim, zlim)))))
  ## fix the arrow length
  if (!is.numeric(length.arrow)) {
    arrowLen <- grid::convertUnit(grid::stringHeight("0"),
      unitTo = "inch",
      valueOnly = FALSE
    )
  } else {
    arrowLen <- length.arrow[1]
    stopifnot(is(arrowLen, "unit"))
  }

  if (k == 2) {
    ## plot 2D
    grid.newpage()
    vp <- viewport(default.units = "native", xscale = xlim, yscale = ylim)
    pushViewport(vp)
    on.exit({
      popViewport()
    })
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

    ncurve <- length(obj) - 1
    x1 <- obj$x[-length(obj$x)]
    x2 <- obj$x[-1]
    y1 <- obj$y[-length(obj$y)]
    y2 <- obj$y[-1]
    if (square) {
      cps <- calcSquareControlPoints(
        x1, y1, x2, y2,
        curvature = 1, angle = 90, ncp = 1
      )
      shape <- interleave(
        ncp = 1,
        ncurve = ncurve,
        val = 0.5,
        sval = 1,
        eval = 1,
        e = cps$end
      )
      ncp <- 2
    } else {
      cps <- calcControlPoints(
        x1, y1, x2, y2,
        curvature = 1, angle = 90, ncp = 1
      )
      ncp <- 1
      shape <- rep(0.5, ncurve)
    }
    idset <- seq.int(ncurve)
    splineGrob <- xsplineGrob(c(x1, cps$x, x2),
      c(y1, cps$y, y2),
      id = c(idset, rep(idset, each = ncp), idset),
      default.units = "native",
      shape = c(rep(0, ncurve), shape, rep(0, ncurve)),
      arrow = NULL, open = TRUE, name = "xspline"
    )
    coor <- grobCoords(splineGrob)
    coor <- lapply(coor, function(.ele) {
      list(
        x = convertX(unit(.ele$x, "inch"),
          unitTo = "native", valueOnly = TRUE
        ),
        y = convertY(unit(.ele$y, "inch"),
          unitTo = "native", valueOnly = TRUE
        )
      )
    })
    midPoints <- ceiling((start(obj) + end(obj)) / 2)
    midPoints <- c(
      start(obj)[1],
      midPoints[-c(1, length(midPoints))],
      end(obj)[length(obj)]
    )
    pP <- mapply(coor, midPoints[-length(midPoints)] + 1, midPoints[-1],
      FUN = function(xys, startP, endP) {
        list(x = xys$x, y = xys$y, start = startP, end = endP)
      }, SIMPLIFY = FALSE
    )
    objCoor <- plotBouquet(
      pP = pP,
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
  } else {
    ## plot 3D
    # recenter the x,y,z
    xcenter <- mean(xlim, na.rm=TRUE)
    ycenter <- mean(ylim, na.rm=TRUE)
    zcenter <- mean(zlim, na.rm=TRUE)
    obj$x <- obj$x - xcenter
    obj$y <- obj$y - ycenter
    obj$z <- obj$z - zcenter
    resizeFactor <- 5 / max(abs(obj$x), abs(obj$y), abs(obj$z), na.rm = TRUE) ## change to around -5:5
    obj$x <- obj$x * resizeFactor
    obj$y <- obj$y * resizeFactor
    obj$z <- obj$z * resizeFactor
    
    dots <- list(...)
    ## add point cluster annotation
    if(cluster3Dpoints){
      eps <- ifelse("eps" %in% names(dots), dots$eps, 'auto')
      quite <- ifelse('quite' %in% names(dots), dots$quite, TRUE)
      if(eps!='auto') eps <- eps * resizeFactor
      obj.cp <- obj
      mcols(obj.cp) <- mcols(obj)[, c('x', 'y', 'z')]
      obj.cp <- fill_NA(obj.cp)
      clusters <- pointCluster(obj.cp, eps = eps, resizeFactor=resizeFactor, quite = quite)
      rm(obj.cp)
      pc <- clusterAnno(obj, clusters)
    }
    ## spline smooth for each bin with 30 points
    if('resolution' %in% names(dots)){
      resolution <- dots$resolution
    }else{
      resolution <- 30
    }
    tryCatch({
      if(singleChr){
        obj <- smooth3dPoints(obj, resolution)
      }else{
        obj <- lapply(split(obj, as.character(seqnames(obj))),
                      smooth3dPoints, resolution=resolution)
      }
    }, error=function(e){
      stop('Can not smooth the 3D model. Please try to add parameter resolution=1')
    })
    
    geometries <- list() ## list to save all geometries to plot
    if(singleChr){
      geometries$backbone <- threeJsGeometry(
        x = c(obj$x0, obj$x1[length(obj)]),
        y = c(obj$y0, obj$y1[length(obj)]),
        z = c(obj$z0, obj$z1[length(obj)]),
        colors = col.backbone,
        type = "line",
        tag = "backbone",
        properties = list(
          size = lwd.backbone,
          target = unname(as.character(ranges(obj))),
          seqn = seqn,
          resizeFactor = resizeFactor
        )
      )
    }else{
      if(length(col.backbone)>length(obj)){
        this.col.backbone <- col.backbone[seq_along(obj)]
      }else{
        this.col.backbone <- col.backbone
      }
      if(length(lwd.backbone)>length(obj)){
        this.lwd.backbone <- lwd.backbone[seq_along(obj)]
      }else{
        this.lwd.backbone <- lwd.backbone
      }
      if(all(names(obj) %in% names(this.col.backbone))){
        this.col.backbone <- this.col.backbone[names(obj)]
      }
      if(all(names(obj) %in% names(this.lwd.backbone))){
        this.lwd.backbone <- this.lwd.backbone[names(obj)]
      }
      backbone <- mapply(function(.obj, .col, .lwd, .seqn){
        threeJsGeometry(
          x = c(.obj$x0, .obj$x1[length(.obj)]),
          y = c(.obj$y0, .obj$y1[length(.obj)]),
          z = c(.obj$z0, .obj$z1[length(.obj)]),
          colors = .col,
          type = "line",
          tag = "backbone",
          properties = list(
            size = .lwd,
            target = unname(as.character(ranges(.obj))),
            seqn = .seqn,
            resizeFactor = resizeFactor
          )
        )
      }, obj, this.col.backbone, this.lwd.backbone, names(obj))
      names(backbone) <- paste0('backbone', names(obj))
      geometries <- c(geometries, backbone)
      obj <- unlist(GRangesList(obj))
    }
    
    ## obj is the GRanges with p0 and p1 (x,y,z) coordinates
    if(cluster3Dpoints){
      type <- ifelse('clusterType' %in% names(dots),
                     ifelse(dots$clusterType %in% c('sphere', 'segment'), 
                            dots$clusterType, 'sphere'), 'sphere')
      pc_geometries <-
        createPointClusterGeometries(pc, obj,
                                     resizeFactor = resizeFactor, type=type)
    }

    rate <- 50

    ## genomicSigs
    missing_genomicSigs <- FALSE
    if (missing(genomicSigs)) {
      missing_genomicSigs <- TRUE
    } else {
      if (length(genomicSigs) == 0) {
        missing_genomicSigs <- TRUE
      }
    }
    if (!missing_genomicSigs) {
      if (!is.list(genomicSigs)) {
        genomicSigs <- list("genomic_signal" = genomicSigs)
      }
      if (length(names(genomicSigs)) != length(genomicSigs)) {
        names(genomicSigs) <- paste0("Signal_", seq_along(genomicSigs))
      }
      if (!is.list(signalTransformFun)) {
        signalTransformFun <- list(signalTransformFun)
      }
      if (length(signalTransformFun) != length(genomicSigs)) {
        signalTransformFun <- rep_len(
          signalTransformFun,
          length(genomicSigs)
        )
      }
      if (!is.list(col.backbone_background)){
        col.backbone_background <- list(col.backbone_background)
      }
      if(length(col.backbone_background) != length(genomicSigs)){
        col.backbone_background <- rep_len(
          col.backbone_background, length(genomicSigs)
        )
      }
      genomic_signal_geometry <- mapply(
        create3dGenomicSignals,
        GenoSig = genomicSigs,
        targetObj = rep(list(obj), length(genomicSigs)),
        signalTransformFun = signalTransformFun,
        reverseGenomicSigs = reverseGenomicSigs,
        type = "segment",
        tag = names(genomicSigs),
        name = names(genomicSigs),
        color = col.backbone_background,
        lwd.maxGenomicSigs = lwd.maxGenomicSigs,
        alpha = alpha.backbone_background,
        resizeFactor = resizeFactor,
        ...,
        SIMPLIFY = FALSE
      )
      names(genomic_signal_geometry) <- NULL
      geometries <- c(
        geometries,
        unlist(genomic_signal_geometry[
          lengths(genomic_signal_geometry) > 0
        ])
      )
      geometries <- geometries[lengths(geometries) > 0]
    }
    ## add genomic coordinates
    if (show_coor & singleChr) {
      r_tick <- range(obj)
      end(r_tick) <- ceiling(end(r_tick) / coor_tick_unit) * coor_tick_unit
      start(r_tick) <- floor(start(r_tick) / coor_tick_unit) * coor_tick_unit
      strand(r_tick) <- "*"
      feature.tick <- GenomicRanges::slidingWindows(r_tick, width = 1, step = coor_tick_unit)[[1]]
      feature.tick$col <- col.tension_line
      tick.xy <- calTickPos(feature.tick, obj, arrowLen, rate = rate, kd = k)
      tick.xy_matrix <- do.call(rbind, tick.xy)
      geometries$tick_minor <- threeJsGeometry(
        x = as.numeric(tick.xy_matrix[c("x1", "x2"), ]),
        y = as.numeric(tick.xy_matrix[c("y1", "y2"), ]),
        z = as.numeric(tick.xy_matrix[c("z1", "z2"), ]),
        type = "segment",
        colors = col.tension_line,
        tag = "tick_minor",
        properties = list(size = lwd.tension_line, resizeFactor = resizeFactor)
      )

      if (coor_mark_interval) {
        feature.tick.mark <- feature.tick[tick.xy$id]
        mark <- start(feature.tick.mark) / coor_mark_interval
        keep <- which(mark == round(mark) & !is.na(tick.xy$x3) &
          !is.na(tick.xy$y3) & !is.na(tick.xy$z3))
        if (length(keep) > 0) {
          geometries$tick_major <- threeJsGeometry(
            x = as.numeric(tick.xy_matrix[c("x1", "x3"), keep]),
            y = as.numeric(tick.xy_matrix[c("y1", "y3"), keep]),
            z = as.numeric(tick.xy_matrix[c("z1", "z3"), keep]),
            type = "segment",
            colors = col.tension_line,
            tag = "tick_major",
            properties = list(size = lwd.tension_line,
                              resizeFactor = resizeFactor)
          )
          coor_text <- prettyMark(
            start(feature.tick.mark),
            coor_mark_interval
          )
          tick_labels <- lapply(keep, function(idx) {
            threeJsGeometry(
              x = tick.xy$x3[idx],
              y = tick.xy$y3[idx],
              z = tick.xy$z3[idx],
              type = "label",
              colors = col.coor,
              tag = "tick_labels",
              properties = list(
                label = coor_text[idx],
                size = .1,
                depth = .02,
                pos = 3,
                resizeFactor = resizeFactor
              )
            )
          })

          names(tick_labels) <- coor_text[keep]
          geometries <- c(geometries, tick_labels)
        }
      }
    }

    ## add gene annotation
    if(is(feature.gr, 'GRanges')){
      feature.gr$type <- tolower(feature.gr$type)
      feature.gr.s <- split(feature.gr, feature.gr$type)
      feature.gr.s <- feature.gr.s[lengths(feature.gr.s)>0]
      names(feature.gr.s) <- NULL
      feature_geometries <- lapply(feature.gr.s, function(this.feature.gr){
        genePos <- calGenePos(this.feature.gr, obj,
                              arrowLen, rate = rate, kd = 3)
        feature_type <- as.character(this.feature.gr$type[1])
        feature_tag <- ifelse(feature_type=='gene', 'gene_body', feature_type)
        if (length(genePos) > 0) {
          gene_body_geometries <- lapply(seq_along(genePos$xs), function(idx) {
            threeJsGeometry(
              x = genePos$xs[[idx]],
              y = genePos$ys[[idx]],
              z = genePos$zs[[idx]],
              colors = genePos$fgf$col[idx],
              type = "line",
              tag = feature_tag,
              properties = list(
                size = ifelse(is.numeric(genePos$fgf$lwd[idx]), genePos$fgf$lwd[idx], lwd.gene),
                target = unname(genePos$fgf$label[idx]),
                resizeFactor = resizeFactor
              )
            )
          })
          names(gene_body_geometries) <- 
            paste0(feature_tag, "_", genePos$fgf$label)
          this_geometries <- gene_body_geometries
          
          if(feature_type=='gene'){
            ## add a vertical line at the TSS.
            this_geometries$tss_vl <- threeJsGeometry(
              x = as.numeric(rbind(genePos$x1, genePos$x2)),
              y = as.numeric(rbind(genePos$y1, genePos$y2)),
              z = as.numeric(rbind(genePos$z1, genePos$z2)),
              colors = genePos$fgf$col,
              type = "segment",
              tag = "tss_labels",
              properties = list(
                size = lwd.gene / 2,
                resizeFactor = resizeFactor
              )
            )
            isGene <- tolower(genePos$fgf$type) %in% "gene" &
              !genePos$missing_start
            if (any(isGene)) {
              ## add arrow
              tss_arrow <- lapply(which(isGene), function(idx) {
                threeJsGeometry(
                  x = c(genePos$x2[idx], tail(genePos$x3[[idx]], n = 1)),
                  y = c(genePos$y2[idx], tail(genePos$y3[[idx]], n = 1)),
                  z = c(genePos$z2[idx], tail(genePos$z3[[idx]], n = 1)),
                  type = "arrow",
                  colors = genePos$fgf$col[idx],
                  tag = "tss_labels",
                  properties = list(
                    headLength = as.numeric(arrowLen) * .2,
                    headWidth = as.numeric(arrowLen) * .2,
                    size = lwd.gene / 2,
                    target = unname(genePos$fgf$label[idx]),
                    resizeFactor = resizeFactor
                  )
                )
              })
              names(tss_arrow) <- paste0("arrow_", genePos$fgf$label[isGene])
              this_geometries <- c(this_geometries, tss_arrow)
            }
          }else{
            notGene <- (!tolower(genePos$fgf$type) %in% 
                          c("gene", "compartment", "tad")) &
              (!genePos$missing_start)
            if (any(notGene)) {
              notGeneRadius <- genePos$fgf$pch[notGene]
              if (length(notGeneRadius) != sum(notGene)) {
                notGeneRadius <- genePos$fgf$size[notGene]
                if (length(genePos$fgf$size[notGene]) == sum(notGene)) {
                  if (!all(vapply(notGeneRadius, is.unit, logical(1L)))) {
                    stop("The size is not in unit format.")
                  }
                  notGeneRadius <- convertUnit(notGeneRadius,
                                               unitTo = "inch",
                                               valueOnly = TRUE
                  ) / 5
                } else {
                  notGeneRadius <- 1
                }
              }else{
                notGeneRadius <- notGeneRadius/36
              }
              if (length(unique(notGeneRadius)) == 1) {
                this_geometries$cRE <- threeJsGeometry(
                  x = genePos$x2[notGene],
                  y = genePos$y2[notGene],
                  z = genePos$z2[notGene],
                  type = "tetrahedron",
                  colors = genePos$fgf$col[notGene],
                  tag = feature_tag,
                  properties = list(
                    radius = notGeneRadius[1],
                    resizeFactor = resizeFactor
                  )
                )
              } else {
                cis_fgf <- unique(notGeneRadius)
                geometries_cRE <- lapply(cis_fgf, function(.r) {
                  idx <- which(notGeneRadius == .r)
                  threeJsGeometry(
                    x = genePos$x2[notGene][idx],
                    y = genePos$y2[notGene][idx],
                    z = genePos$z2[notGene][idx],
                    type = "tetrahedron",
                    colors = genePos$fgf$col[notGene][idx],
                    tag = feature_tag,
                    properties = list(
                      radius = .r,
                      resizeFactor = resizeFactor
                    )
                  )
                })
                names(geometries_cRE) <- paste0(feature_tag, "_", cis_fgf)
                this_geometries <- c(this_geometries, geometries_cRE)
              }
            }
          }
          
          if (label_gene && any(!is.na(genePos$fgf$label))) {
            gene_labels_geometries <- lapply(
              seq_along(genePos$fgf$label)[!is.na(genePos$fgf$label)],
              function(idx) {
                threeJsGeometry(
                  x = genePos$x2[idx],
                  y = genePos$y2[idx],
                  z = (genePos$z2[idx] + genePos$z1[idx]) / 2,
                  type = "label", #' text',
                  colors = genePos$fgf$col[idx],
                  tag = paste0(feature_type, "_labels"),
                  properties = list(
                    label = unname(genePos$fgf$label[idx]),
                    size = 0.1,
                    depth = 0.02,
                    pos = 4,
                    resizeFactor = resizeFactor
                  )
                )
              }
            )
            names(gene_labels_geometries) <-
              paste0(feature_type, "_label_",
                     genePos$fgf$label[!is.na(genePos$fgf$label)])
            this_geometries <- c(this_geometries, gene_labels_geometries)
          }
          return(this_geometries)
        }
        return(NULL)
      })
      geometries <- c(
        geometries,
        unlist(feature_geometries[
          lengths(feature_geometries) > 0
        ])
      )
    }
    ## add point clusters
    if(cluster3Dpoints){
      if(length(pc_geometries)>0){
        geometries <- c(geometries, pc_geometries)
      } 
    }
    
    geometries <- geometries[lengths(geometries) > 0]
    if (renderer == "rgl") {
      return(rglViewer(geometries))
    } else {
      if (renderer == "threejs") {
        return(threeJsViewer(geometries))
      } else {
        return(geometries)
      }
    }
  }
}
