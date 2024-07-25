#' Plot GRanges xyz data in 2d or 3d
#' @description
#' Plot GRanges xyz data with grid or rgl package.
#' @param obj GRanges object with mcols x, y, and/or z
#' @param k The dimension of plot. 2: 2d, 3: 3d.
#' @param feature.gr The annotation features to be added. An object of \link[GenomicRanges:GRanges-class]{GRanges}.
#' @param genomicSigs The Genomic signals. An object of \link[GenomicRanges:GRanges-class]{GRanges} with scores or an object of \link[trackViewer:track]{track}.
#' @param renderer The renderer of the 3D plots. Could be rgl or threejs.
#' The threejs will create a htmlwidgets. If 'none' is set, a list of object
#' will be returned.
#' @param show_coor Plot ticks in the line to show the DNA compact tension.
#' @param reverseGenomicSigs Plot the genomic signals in reverse values.
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
#' @param ... Not used.
#' @return Coordinates for 2d or a list of threeJsGeometry objects or a 
#' htmlwidget.
#' @importFrom stats quantile
#' @importFrom trackViewer parseWIG
#' @importFrom IRanges tile
#' @importFrom GenomeInfoDb seqnames 
#' @importFrom BiocGenerics start<- end<- strand<-
#' @importFrom utils tail
#' @importFrom grDevices colorRampPalette
#' @importFrom grid convertUnit grid.newpage viewport pushViewport popViewport 
#' convertWidth convertHeight xsplineGrob grobCoords convertX convertY
#' @export
#' @examples
#' obj <- readRDS(system.file('extdata', '4DNFI1UEG1HD.chr21.FLAMINGO.res.rds',
#'     package='geomeTriD'))
#' feature.gr <- readRDS(system.file('extdata', '4DNFI1UEG1HD.feature.gr.rds',
#'     package='geomeTriD'))
#' tjg <- view3dStructure(obj, k=3, feature.gr=feature.gr, renderer='none',
#'    length.arrow=grid::unit(0.000006, 'native'))
view3dStructure <- function(obj, k=3, feature.gr,
                            genomicSigs,
                            renderer = c('rgl', 'threejs', 'none'),
                            lwd.backbone = 2, col.backbone = 'gray',
                            lwd.maxGenomicSigs = 8, reverseGenomicSigs = TRUE,
                            col.backbone_background = 
                              if(k==2) 'gray70' else c('gray30', 'darkred'),
                            alpha.backbone_background = 0.5,
                            lwd.gene = 3,
                            coor_mark_interval = 5e5, col.coor = "black",
                            show_coor = TRUE,
                            coor_tick_unit = 5e4,
                            label_gene = TRUE,
                            col.tension_line = 'black',
                            lwd.tension_line = 1,
                            length.arrow = NULL,
                            safe_text_force = 3,
                            square = TRUE,
                            ...){
  stopifnot(is(obj, 'GRanges'))
  stopifnot(
    'Only work for data in a single chromsome.'=
      all(as.character(seqnames(obj))==as.character(seqnames(obj)[1])))
  seqn <- as.character(seqnames(obj)[1])
  stopifnot(is.numeric(k))
  stopifnot(k==2 || k==3)
  stopifnot(all(c('x', 'y') %in% colnames(mcols(obj))))
  if(k==3){
    stopifnot('z' %in% colnames(mcols(obj)))
  }
  renderer <- match.arg(renderer)
  feature.gr <- parseFeature(feature.gr=feature.gr)
  xlim <- range(obj$x)
  ylim <- range(obj$y)
  zlim <- range(obj$z)
  d_xlim <- diff(xlim)/10
  d_ylim <- diff(ylim)/10
  d_zlim <- diff(zlim)/10
  xlim <- c(xlim[1] - d_xlim, xlim[2] + d_xlim)
  ylim <- c(ylim[1] - d_ylim, ylim[2] + d_ylim)
  zlim <- c(zlim[1] - d_zlim, zlim[2] + d_zlim)
  scale_factor <- 10^floor(log10(diff(range(c(xlim, ylim, zlim)))))
  if(k==2){
    ## plot 2D
    grid.newpage()
    vp <- viewport(default.units = 'native', xscale=xlim, yscale=ylim)
    pushViewport(vp)
    on.exit({
      popViewport()
    })
    ## fix the arrow length
    if(!is.numeric(length.arrow)){
      arrowLen <- grid::convertUnit(grid::stringHeight("0"),
                                    unitTo = "inch",
                                    valueOnly = FALSE)
    }else{
      arrowLen <- length.arrow[1]
      stopifnot(is(arrowLen, 'unit'))
    }
    rate <- max(abs(
      c(convertWidth(unit(diff(xlim), 'native'),
                     unitTo = "inch", valueOnly = TRUE),
        convertHeight(unit(diff(ylim), 'native'),
                      unitTo = "inch", valueOnly = TRUE))))/
      convertUnit(arrowLen, unitTo = "inches", valueOnly = TRUE)
    
    ncurve <- length(obj) - 1
    x1 <- obj$x[-length(obj$x)]
    x2 <- obj$x[-1]
    y1 <- obj$y[-length(obj$y)]
    y2 <- obj$y[-1]
    if(square){
      cps <- calcSquareControlPoints(
        x1, y1, x2, y2,
        curvature = 1, angle = 90, ncp = 1)
      shape <- interleave(ncp = 1,
                          ncurve = ncurve,
                          val = 0.5,
                          sval = 1, 
                          eval = 1,
                          e = cps$end)
      ncp <- 2
    }else{
      cps <- calcControlPoints(
        x1, y1, x2, y2,
        curvature = 1, angle = 90, ncp = 1)
      ncp <- 1
      shape <- rep(0.5, ncurve)
    }
    idset <- seq.int(ncurve)
    splineGrob <- xsplineGrob(c(x1, cps$x, x2), 
                              c(y1, cps$y, y2),
                              id = c(idset, rep(idset,  each = ncp), idset),
                              default.units = "native", 
                              shape = c(rep(0, ncurve), shape, rep(0, ncurve)), 
                              arrow = NULL, open = TRUE, name = "xspline")
    coor <- grobCoords(splineGrob)
    coor <- lapply(coor, function(.ele){
      list(x = convertX(unit(.ele$x, 'inch'),
                        unitTo = 'native', valueOnly = TRUE),
           y = convertY(unit(.ele$y, 'inch'),
                        unitTo = 'native', valueOnly = TRUE))
    })
    midPoints <- ceiling((start(obj) + end(obj))/2)
    midPoints <- c(start(obj)[1],
                   midPoints[-c(1, length(midPoints))],
                   end(obj)[length(obj)])
    pP <- mapply(coor, midPoints[-length(midPoints)]+1, midPoints[-1],
                 FUN=function(xys, startP, endP){
                   list(x=xys$x, y=xys$y, start=startP, end=endP)
                 }, SIMPLIFY = FALSE)
    objCoor <- plotBouquet(pP=pP,
                           fgf=feature.gr,
                           genomicSigs=genomicSigs,
                           lwd.backbone=lwd.backbone,
                           col.backbone=col.backbone,
                           lwd.maxGenomicSigs = lwd.maxGenomicSigs,
                           reverseGenomicSigs = reverseGenomicSigs,
                           col.backbone_background=col.backbone_background,
                           alpha.backbone_background=alpha.backbone_background,
                           lwd.gene=lwd.gene,
                           coor_mark_interval=coor_mark_interval,
                           col.coor=col.coor,
                           show_coor = show_coor,
                           coor_tick_unit = coor_tick_unit,
                           label_gene = label_gene,
                           col.tension_line = col.tension_line,
                           lwd.tension_line = lwd.tension_line,
                           safe_text_force = safe_text_force,
                           arrowLen = arrowLen, rate = rate,
                           xlim=xlim, ylim=ylim)
  }else{
    ## plot 3D
    # recenter the x,y,z
    xcenter <- mean(xlim)
    ycenter <- mean(ylim)
    zcenter <- mean(zlim)
    obj$x <- obj$x - xcenter
    obj$y <- obj$y - ycenter
    obj$z <- obj$z - zcenter
    mf <- 5*scale_factor/max(abs(obj$x), abs(obj$y), abs(obj$z)) ## change to around -5:5
    obj$x <- obj$x * mf
    obj$y <- obj$y * mf
    obj$z <- obj$z * mf
    t <- seq_along(obj)
    ## spline smooth for each bin with 30 points
    resolusion <- 30
    tt <- seq(1, length(obj), len = resolusion*length(obj)+1)
    sdata <- lapply(colnames(mcols(obj)), function(j){
      splinefun(t, mcols(obj)[, j, drop=TRUE])(tt)
    })
    sdata <- do.call(cbind, sdata)
    sdata <- data.frame(sdata)
    colnames(sdata) <- colnames(mcols(obj))
    obj <- tile(obj, n=resolusion)
    obj <- unlist(obj)
    stopifnot('Unexpected happend.'=length(obj)+1==nrow(sdata))
    sdata0 <- sdata[-nrow(sdata), , drop=FALSE]
    sdata1 <- sdata[-1, , drop=FALSE]
    colnames(sdata0) <- paste0(colnames(sdata), '0')
    colnames(sdata1) <- paste0(colnames(sdata), '1')
    mcols(obj) <- cbind(sdata0, sdata1)
    ## obj is the GRanges with p0 and p1 (x,y,z) coordinates
    
    geometries <- list() ## list to save all geometries to plot
    geometries$backbone <- threeJsGeometry(
      x = c(obj$x0, obj$x1[length(obj)])/scale_factor,
      y = c(obj$y0, obj$y1[length(obj)])/scale_factor,
      z = c(obj$z0, obj$z1[length(obj)])/scale_factor,
      colors = col.backbone,
      type = 'line',
      tag = 'backbone',
      properties = list(size = lwd.backbone)
    )
    
    ## fix the arrow length
    if(!is.numeric(length.arrow)){
      arrowLen <- grid::convertUnit(grid::stringHeight("0"),
                                    unitTo = "inch",
                                    valueOnly = FALSE)
    }else{
      arrowLen <- length.arrow[1]
      stopifnot(is(arrowLen, 'unit'))
    }
    rate <- 50
    
    ## genomicSigs
    missing_genomicSigs <- FALSE
    if(missing(genomicSigs)){
      missing_genomicSigs <- TRUE
    }else{
      if(length(genomicSigs)==0){
        missing_genomicSigs <- TRUE
      }
    }
    if(!missing_genomicSigs){
      if(!is.list(genomicSigs)){
        genomicSigs <- list('genomic_signal'=genomicSigs)
      }
      if(length(names(genomicSigs))!=length(genomicSigs)){
        names(genomicSigs) <- paste0('Signal_', seq_along(genomicSigs))
      }
      genomic_signal_geometry <- mapply(function(GenoSig, .name, revGenoSig){
        this.col.backbone_background <- col.backbone_background
        if(is(GenoSig, 'track')){
          if(GenoSig$format=='WIG'){
            GenoSig <- parseWIG(trackScore=GenoSig,
                                chrom=seqn,
                                from=start(range(obj)),
                                to=end(range(obj)))
          }
          if(length(GenoSig@style@color)){
            if(!all(GenoSig@style@color=="black")){
              this.col.backbone_background <- GenoSig@style@color
            }
          }
          GenoSig <- GenoSig$dat
        }
        stopifnot(is(GenoSig, 'GRanges'))
        stopifnot('score' %in% colnames(mcols(GenoSig)))
        GenoSig <- resampleDataByFun(GenoSig, obj, dropZERO = FALSE,
                                     na.rm = TRUE)
        genomicSigScoreRange <- quantile(log2(GenoSig$score+1),
                                      probs = c(.1, .99))
        if(genomicSigScoreRange[1]==genomicSigScoreRange[2]){
          genomicSigScoreRange <- range(log2(GenoSig$score+1))
        }
        if(genomicSigScoreRange[1]!=genomicSigScoreRange[2]){
          genomicSigBreaks <- c(-1,
                             seq(genomicSigScoreRange[1],
                                 genomicSigScoreRange[2],
                                 length.out = lwd.maxGenomicSigs-1),
                             max(log2(GenoSig$score+1))+1)
          genomicSiglabels <- seq_along(genomicSigBreaks)[-length(genomicSigBreaks)]
          if(revGenoSig){
            genomicSiglabels <- rev(genomicSiglabels)
            this.col.backbone_background <- rev(this.col.backbone_background)
          }
          GenoSig$lwd <- as.numeric(as.character(
            cut(log2(GenoSig$score+1),
                breaks=genomicSigBreaks,
                labels = genomicSiglabels)))
          genomicSigLwd <- sort(unique(GenoSig$lwd))
          if(length(this.col.backbone_background)>1){
            this.col.backbone_background <-
              colorRampPalette(colors=this.col.backbone_background)(
                max(genomicSigLwd))
            names(this.col.backbone_background) <- genomicSiglabels
          }else{
            this.col.backbone_background <- rep(this.col.backbone_background,
                                           length(genomicSigLwd))
            names(this.col.backbone_background) = genomicSigLwd
          }
          genomic_signal <- lapply(genomicSigLwd, function(lwd){
            idx <- which(GenoSig$lwd==lwd)
            threeJsGeometry(
              x = as.numeric(t(as.matrix(mcols(obj)[idx, c('x0', 'x1')])))/
                scale_factor,
              y = as.numeric(t(as.matrix(mcols(obj)[idx, c('y0', 'y1')])))/
                scale_factor,
              z = as.numeric(t(as.matrix(mcols(obj)[idx, c('z0', 'z1')])))/
                scale_factor,
              type = 'segment',
              colors = this.col.backbone_background[lwd],
              tag=.name,
              properties = list(size=lwd,
                                alpha= alpha.backbone_background)
            )
          })
          names(genomic_signal) <- paste0(.name, '_lwd_', genomicSigLwd)
          genomic_signal
        }else{
          NULL
        }
      }, genomicSigs, names(genomicSigs), reverseGenomicSigs, SIMPLIFY = FALSE)
      geometries <- c(geometries,
                      unlist(genomic_signal_geometry[
                        lengths(genomic_signal_geometry)>0]))
      geometries <- geometries[lengths(geometries)>0]
    }
    
    ## add genomic coordinates
    if(show_coor){
      r_tick <- range(obj)
      end(r_tick) <- ceiling(end(r_tick)/coor_tick_unit)*coor_tick_unit
      start(r_tick) <- floor(start(r_tick)/coor_tick_unit)*coor_tick_unit
      strand(r_tick) <- "*"
      feature.tick <- GenomicRanges::slidingWindows(r_tick, width = 1, step=coor_tick_unit)[[1]]
      feature.tick$col <- col.tension_line
      tick.xy <- calTickPos(feature.tick, obj, arrowLen, rate=rate, kd=k)
      tick.xy_matrix <- do.call(rbind, tick.xy)
      geometries$tick_minor <- threeJsGeometry(
        x = as.numeric(tick.xy_matrix[c('x1', 'x2'), ])/scale_factor,
        y = as.numeric(tick.xy_matrix[c('y1', 'y2'), ])/scale_factor,
        z = as.numeric(tick.xy_matrix[c('z1', 'z2'), ])/scale_factor,
        type = 'segment',
        colors = col.tension_line,
        tag='tick_minor',
        properties = list(size=lwd.tension_line)
      )
      
      if(coor_mark_interval){
        feature.tick.mark <- feature.tick[tick.xy$id]
        mark <- start(feature.tick.mark)/coor_mark_interval
        keep <- which(mark==round(mark) & !is.na(tick.xy$x3) & 
                        !is.na(tick.xy$y3) & !is.na(tick.xy$z3))
        if(length(keep)>0){
          geometries$tick_major <- threeJsGeometry(
            x = as.numeric(tick.xy_matrix[c('x1', 'x3'), keep])/scale_factor,
            y = as.numeric(tick.xy_matrix[c('y1', 'y3'), keep])/scale_factor,
            z = as.numeric(tick.xy_matrix[c('z1', 'z3'), keep])/scale_factor,
            type = 'segment',
            colors = col.tension_line,
            tag='tick_major',
            properties = list(size=lwd.tension_line)
          )
          coor_text <- prettyMark(start(feature.tick.mark[keep]),
                                  coor_mark_interval)
          tick_labels <- lapply(seq_along(tick.xy$x3[keep]), function(idx){
            threeJsGeometry(
              x = tick.xy$x3[idx]/scale_factor,
              y = tick.xy$y3[idx]/scale_factor,
              z = tick.xy$z3[idx]/scale_factor,
              type = 'label',
              colors = col.coor,
              tag='tick_labels',
              properties = list(label = coor_text[idx],
                                size = .1,
                                depth = .02,
                                pos=3)
            )
          })
          
          names(tick_labels) <- coor_text
          geometries <- c(geometries, tick_labels)
        }
      }
    }
    
    ## add gene annotation
    genePos <- calGenePos(feature.gr, obj, arrowLen, rate=rate, kd=3)
    if(length(genePos)>0){
      gene_body_geometries <- lapply(seq_along(genePos$xs), function(idx){
        threeJsGeometry(
          x=genePos$xs[[idx]]/scale_factor,
          y=genePos$ys[[idx]]/scale_factor,
          z=genePos$zs[[idx]]/scale_factor,
          colors = genePos$fgf$col[idx],
          type = 'line',
          tag = 'gene_body',
          properties = list(
            size = lwd.gene
          )
        )
      })
      names(gene_body_geometries) <- paste0('gene_body_', genePos$fgf$label)
      geometries <- c(geometries, gene_body_geometries)
      
      ## add a vertical line at the TSS.
      geometries$tss_vl <- threeJsGeometry(
        x=as.numeric(rbind(genePos$x1, genePos$x2))/scale_factor,
        y=as.numeric(rbind(genePos$y1, genePos$y2))/scale_factor,
        z=as.numeric(rbind(genePos$z1, genePos$z2))/scale_factor,
        colors=genePos$fgf$col,
        type='segment',
        tag = 'tss_labels',
        properties = list(
          size = lwd.gene/2
        )
      )
      
      isGene <- genePos$fgf$type %in% 'gene' & !genePos$missing_start
      if(any(isGene)){
        ## add arrow
        tss_arrow <- lapply(which(isGene), function(idx){
          threeJsGeometry(
            x = c(genePos$x2[idx], tail(genePos$x3[[idx]], n=1))/scale_factor,
            y = c(genePos$y2[idx], tail(genePos$y3[[idx]], n=1))/scale_factor,
            z = c(genePos$z2[idx], tail(genePos$z3[[idx]], n=1))/scale_factor,
            type = 'arrow',
            colors = genePos$fgf$col[idx],
            tag = 'tss_labels',
            properties = list(headLength =as.numeric(arrowLen)*.2,
                              headWidth =as.numeric(arrowLen)*.2,
                              size = lwd.gene/2)
          )
        })
        names(tss_arrow) <- paste0('arrow_', genePos$fgf$label[isGene])
        geometries <- c(geometries, tss_arrow)
      }
      
      notGene <- (!genePos$fgf$type %in% 'gene') & (!genePos$missing_start)
      if(any(notGene)){
        geometries$cRE <- threeJsGeometry(
          x = genePos$x2[notGene]/scale_factor,
          y = genePos$y2[notGene]/scale_factor,
          z = genePos$z2[notGene]/scale_factor,
          type = 'tetrahedron',
          colors = genePos$fgf$col[notGene],
          tag = 'cRE',
          properties = list(
            radius = convertUnit(genePos$fgf$size[notGene], unitTo = 'inch',
                                 valueOnly = TRUE)/5,
            pch = genePos$fgf$pch[notGene]
          )
        )
      }
      if(label_gene && any(!is.na(genePos$fgf$label))){
        gene_labels_geometries <- lapply(
          seq_along(genePos$fgf$label)[!is.na(genePos$fgf$label)],
          function(idx){
            threeJsGeometry(
              x = genePos$x2[idx]/scale_factor,
              y = genePos$y2[idx]/scale_factor,
              z = (genePos$z2[idx] + genePos$z1[idx])/2/scale_factor,
              type = 'label',#'text',
              colors = genePos$fgf$col[idx],
              tag='gene_labels',
              properties = list(
                label = unname(genePos$fgf$label[idx]),
                size = 0.1,
                depth = 0.02,
                pos = 4
              )
            )
          })
        names(gene_labels_geometries) <- 
          paste0('gene_label_', genePos$fgf$label[!is.na(genePos$fgf$label)])
        geometries <- c(geometries, gene_labels_geometries)
      }
    }
    if(renderer=='rgl'){
      return(rglViewer(geometries))
    }else{
      if(renderer=='threejs'){
        return(threeJsViewer(geometries))
      }else{
        return(geometries)
      }
    }
  }
}
