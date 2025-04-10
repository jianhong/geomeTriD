#' create 3d Geometry by given TADs
#' @description
#' Create a 3d Geometry by given TADs for target 3d positions.
#' @param tad The TAD. An object of
#' \link[GenomicRanges:GRanges-class]{GRanges}.
#' @param targetObj The GRanges object with mcols x0, y0, z0, x1, y1, and z1
#' @param type The Geometry type. default is sphere. Possible types are
#' sphere or segment.
#' @param tag The tag used to group geometries.
#' @param name The prefix for the name of the geometries.
#' @param alpha alpha value. default is 0.2
#' @param lwd line width for segment.
#' @return \link{threeJsGeometry} objects
#' @export
#' @examples
#' library(GenomicRanges)
#' obj <- readRDS(system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds",
#'   package = "geomeTriD"
#' ))
#' tjg <- view3dStructure(obj, renderer = "none")
#' pc <- pointCluster(as.data.frame(mcols(obj)))
#' tads <- split(obj, pc$cluster)
#' tads <- tads[names(tads)!="0"] # cluster 0 is noise
#' tads <- unlist(range(GRangesList(tads)))
#' backbone <- extractBackbonePositions(tjg)
#' tad_geometries <- createTADGeometries(tads, backbone)
#' threeJsViewer(tjg, tad_geometries)
createTADGeometries <- function(tad, targetObj,
                                type = 'sphere',
                                name = 'TAD_',
                                tag="TAD",
                                alpha = 0.2,
                                lwd = 3){
  stopifnot(is.numeric(alpha))
  stopifnot(alpha>=0)
  stopifnot(alpha<=1)
  stopifnot(is.character(name))
  stopifnot(length(name) == 1)
  stopifnot(is.character(tag))
  stopifnot(length(tag) == 1)
  stopifnot(is.numeric(lwd))
  type <- match.arg(type, choices = c('sphere', 'segment'))
  stopifnot(inherits(tad, c('GRanges', 'GRangesList')))
  checkSmoothedGR(targetObj)
  if(is(tad, 'GRangesList')){
    tad_geometries <- lapply(tad, createTADGeometries)
    names(tad_geometries) <- names(tad)
    if(is.null(names(tad_geometries))){
      names(tad_geometries) <- paste0('TAD_', seq_along(tad))
    }
    return(tad_geometries)
  }
  mcn <- colnames(mcols(tad))
  if(!'label' %in% mcn){
    tad$label <- paste0('tad_', seq_along(tad))
  }
  if(any(duplicated(tad$label))){
    stop('input tad has duplicated label. Please keep it unique.')
  }
  if(!'col' %in% mcn){
    colorColumn <- grepl('^(col|color)(s?)$', mcn, ignore.case = TRUE)
    if(any(colorColumn)){
      col <- mcols(tad)[, colorColumn[1]]
      if(all(areColors(col))){
        tad$col <- col
        mcn <- colnames(mcols(tad))
      }
    }
  }
  if(!'col' %in% mcn){
    tad$col <- rep(seq.int(7), length(tad))[seq_along(tad)]
  }
  if(type=="sphere"){
    targetObj$x <- (targetObj$x0 + targetObj$x1)/2
    targetObj$y <- (targetObj$y0 + targetObj$y1)/2
    targetObj$z <- (targetObj$z0 + targetObj$z1)/2
    ol <- findOverlaps(targetObj, tad, type='within')
    coor <- split(as.data.frame(
      mcols(targetObj[queryHits(ol)])[, c('x', 'y', 'z')]),
      tad[subjectHits(ol)]$label)
    coor.center <- lapply(coor, colMeans)
    coor.radius <- mapply(function(.data, .center){
      max(sqrt(colSums((t(.data) - .center)^2)))
    }, coor, coor.center)
    coor.center <- do.call(rbind, coor.center)
    tad_geometries <- lapply(rownames(coor.center), function(idx) {
      threeJsGeometry(
        x = coor.center[idx, 'x'],
        y = coor.center[idx, 'y'],
        z = coor.center[idx, 'z'],
        type = type,
        colors = tad$col[match(idx, tad$label)],
        tag = tag,
        properties = list(
          label = idx,
          radius = unname(coor.radius[idx]),
          alpha = alpha
        )
      )
    })
    names(tad_geometries) <- 
      paste0(name, names(coor))
  }else{
    ## type=='segment'
    if(!'lwd' %in% colnames(mcols(tad))){
      tad$lwd <- lwd
    }
    tad$score <- seq_along(tad)
    GenoSig <- resampleDataByFun(tad, targetObj,
                                 dropZERO = TRUE,
                                 na.rm = TRUE)
    tad_geometries <- lapply(seq_along(tad), function(score) {
      idx <- which(GenoSig$score == score)
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
        colors = tad$col[score],
        tag = tag,
        properties = list(
          size = tad$lwd[score],
          alpha = alpha
        )
      )
    })
    names(tad_geometries) <- 
      paste0(name, tad$label)
  }
  return(tad_geometries)
}

#' @importFrom grDevices col2rgb
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}