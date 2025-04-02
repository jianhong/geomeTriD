#' Perform DBSCAN clustering
#' @description
#' Perform DBSCAN clustering for given 3D coordinates.
#' @param xyz A data.frame with x, y, z coordinates
#' @param eps The size (radius) of the epsilon neighborhood. Default 'auto'.
#' @param ... other parameters could be used by dbscan function except x and eps.
#' @return A an object of class dbscan_fast.
#' @importFrom RANN nn2
#' @importFrom dbscan dbscan
#' @export
#' @examples
#' xyz <- readRDS(system.file('extdata', '4DNFI1UEG1HD.chr21.FLAMINGO.res.rds',
#'  package='geomeTriD'))
#' pc <- pointCluster(xyz)
pointCluster <- function(xyz, eps = 'auto', ...){
  if(is(xyz, 'GRanges')) xyz <- as.data.frame(mcols(xyz))
  stopifnot(ncol(xyz)==3)
  stopifnot(is.data.frame(xyz) || is.matrix(xyz))
  # Find the nearest neighbor (k = 2 means itself + 1 nearest neighbor)
  nn_result <- RANN::nn2(xyz, k=2)
  # Extract distances (column 2 gives nearest neighbor distance)
  euclidean_distances <- nn_result$nn.dists[, 2]  # Ignore first column (self-distance)
  if(eps=='auto'){
    ## create a sequence of eps
    eps_seq <- quantile(euclidean_distances, probs=seq(0, 1, 0.01))
    ## do dbscan
    dbscan_result <- lapply(eps_seq, function(.e){
      tryCatch(
        dbscan::dbscan(x=xyz, eps = .e, ...),
        error = function(e){
          list(cluster=-1)
        })
    })
    ## find the one with the maximal clusters
    l <- vapply(dbscan_result, function(.e) length(unique(.e$cluster)), integer(1L))
    dbscan_result <- dbscan_result[[which.max(l)]]
    message('eps is set to ', dbscan_result$eps)
  }else{
    stopifnot(is.numeric(eps))
    dbscan_result <- dbscan(x=xyz, eps = eps, ...)
  }
  dbscan_result$colors <- addColor2Cluster(dbscan_result$cluster)
  return(dbscan_result)
}

askNamespace <- function(...) {
  pkgs <- list(...)
  lapply(pkgs, function(pkg) {
    if (!requireNamespace(pkg)) {
      stop(
        "The ", pkg, " package is required for this function!")
    }
  })
}

#' @importFrom grDevices rainbow
addColor2Cluster <- function(cluster){
  N <- max(cluster)
  colors <- c('#CCCCCC', sample(rainbow(N), N, replace = TRUE))
  names(colors) <- c(0, seq.int(N))
  return(colors)
}

## used to create genomic signals
clusterAnno <- function(gr, clusters){
  cluster <- clusters$cluster
  tads <- GRanges(paste(as.character(seqnames(gr)), cluster, sep='__'),
                  ranges(gr))
  tads <- reduce(tads)
  seqn <- do.call(rbind, strsplit(as.character(seqnames(tads)), '__'))
  tads <- GRanges(seqn[, 1], ranges(tads), cluster=seqn[, 2])
  tads$label <- ifelse(tads$cluster=="0", 'noise', paste0('pointCluster', tads$cluster))
  tads$type <- 'pointCluster'
  tads$col <- clusters$colors[tads$cluster] 
  return(tads)
}

createPointClusterGeometries <- function(pc, obj){
  stopifnot(is(pc, 'GRanges'))
  stopifnot(all(c('label', 'col', 'cluster') %in% colnames(mcols(pc))))
  stopifnot(is(obj, 'GRanges'))
  stopifnot(all(c('x', 'y', 'z') %in% colnames(mcols(obj))))
  pc.s <- pc[pc$cluster!="0"]
  pc.s <- split(pc.s, pc.s$cluster)
  pc.s <- unlist(range(GRangesList(pc.s)))
  pc.s$cluster <- names(pc.s)
  pc.s$col <- pc$col[match(pc.s$cluster, pc$cluster)]
  ol <- findOverlaps(obj, pc.s, type='within')
  coor <- split(as.data.frame(mcols(obj[queryHits(ol)])[, c('x', 'y', 'z')]),
                pc.s$cluster[subjectHits(ol)])
  coor.center <- lapply(coor, colMeans)
  coor.radius <- mapply(function(.data, .center){
    max(sqrt(colSums((t(.data) - .center)^2)))
  }, coor, coor.center)
  coor.center <- do.call(rbind, coor.center)
  pc_geometries <- lapply(seq.int(nrow(coor.center)), function(idx) {
    threeJsGeometry(
      x = coor.center[idx, 'x'],
      y = coor.center[idx, 'y'],
      z = coor.center[idx, 'z'],
      type = "sphere",
      colors = pc$col[match(names(coor)[idx], pc$cluster)],
      tag = "pointCluter",
      properties = list(
        label = unname(pc$label[match(names(coor)[idx], pc$cluster)]),
        radius = unname(coor.radius[names(coor)[idx]]),
        alpha = 0.2
      )
    )
  })
  names(pc_geometries) <- 
    paste0("pointCluter_", names(coor))
  return(pc_geometries)
}