#' Perform DBSCAN clustering
#' @description
#' Perform DBSCAN clustering for given 3D coordinates.
#' @param xyz A data.frame with x, y, z coordinates
#' @param eps The size (radius) of the epsilon neighborhood. Default 'auto'.
#' @param quite Print message or not.
#' @param ... other parameters could be used by dbscan function except x and eps.
#' @return A an object of class dbscan_fast.
#' @importFrom RANN nn2
#' @importFrom dbscan dbscan
#' @export
#' @examples
#' xyz <- readRDS(system.file('extdata', '4DNFI1UEG1HD.chr21.FLAMINGO.res.rds',
#'  package='geomeTriD'))
#' pc <- pointCluster(xyz)
pointCluster <- function(xyz, eps = 'auto', quite=FALSE, ...){
  if(is(xyz, 'GRanges')) xyz <- as.data.frame(mcols(xyz))
  stopifnot(ncol(xyz) %% 3 == 0)
  stopifnot(is.data.frame(xyz) || is.matrix(xyz))
  # Find the nearest neighbor (k = 2 means itself + 1 nearest neighbor)
  nn_result <- NULL
  tryCatch(
    nn_result <- RANN::nn2(xyz, k=2), error=function(.e){
    if(!quite) message(.e)
  })
  if(is.null(nn_result)){
    return(structure(list(cluster = rep(0, nrow(xyz)),
                          eps = eps,
                          colors=rep('#CCCCCC', nrow(xyz))),
                     class = c("dbscan_fast", "dbscan")))
  }
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
    if(!quite) message('eps is set to ', dbscan_result$eps)
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
  if(N==0){
    colors <- c('0'='#CCCCCC')
  }else{
    colors <- c('#CCCCCC', sample(rainbow(N), N, replace = TRUE))
    names(colors) <- c(0, seq.int(N))
  }
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
##  summarize the clusters
summarizeClusters <- function(pc){
  stopifnot(is(pc, 'GRanges'))
  stopifnot(all(c('label', 'col', 'cluster') %in% colnames(mcols(pc))))
  pc.s <- pc[pc$cluster!="0"]
  pc.s <- split(pc.s, pc.s$cluster)
  pc.s <- unlist(range(GRangesList(pc.s)))
  pc.s$cluster <- names(pc.s)
  pc.s$col <- pc$col[match(pc.s$cluster, pc$cluster)]
  pc.s$label <- pc$label[match(pc.s$cluster, pc$cluster)]
  return(pc.s)
}

createPointClusterGeometries <- function(pc, obj, type="sphere", ...){
  pc.s <- summarizeClusters(pc)
  pc_geometries <- createTADGeometries(pc.s, obj,
                      type = type,
                      name = 'pointCluter_',
                      tag="pointCluter",
                      ...)
  return(pc_geometries)
}