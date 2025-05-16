#' cluster single cell by 3D structure
#' @description
#' Perform DBSCAN clustering for given 3D structure.
#' @param xyzs A data.frame with x, y, z coordinates.
#' @param TADs A list of index vectors, where each vector represents a TAD.
#'  For example, if the first TAD spans the 2nd to 4th coordinates and the
#'   second spans the 8th to 10th coordinates, the list would be:
#'    list(c(2, 3, 4), c(8, 9, 10)).
#' @param N Use top N most variable point clusters (or TADs). Default is 100.
#' @param eps The size (radius) of the epsilon neighborhood. Default is 'auto'.
#' @param ... not used.
#' @return A an object of class dbscan_fast.
#' @export
#' @importFrom stats var
#' @examples
#' set.seed(1)
#' xyzs <- lapply(seq.int(20), function(i){
#'   matrix(sample.int(100, 60, replace = TRUE),
#'    nrow=20, dimnames=list(NULL, c('x', 'y', 'z')))
#' })
#' cc <- cellClusters(xyzs, N=10)
cellClusters <- function(xyzs, TADs, N=100, eps='auto', ...){
  checkXYZdim(xyzs)
  k <- nrow(xyzs[[1]])
  if(!missing(TADs)){
    stopifnot(is.list(TADs))
    uTADs <- unlist(TADs)
    if(!is.numeric(uTADs)){
      stop('All elements in TADs list must be number.')
    }
    if(min(uTADs)<1){
      stop('All elements in TADs list should not smaller than 1')
    }
    if(max(uTADs)>k){
      stop('All elements in TADs list should not larger than ', k)
    }
  }
  ## find the center of xyzs
  ## rescale the xyzs to same size
  xyzs <- rescalePointClouds(xyzs)
  ## fill the NA with nearby points
  xyzs <- lapply(xyzs, fill_NA)
  if(!missing(TADs)){
    pcs <- TADs
  }else{
    if(N>k){
      N <- k
      pcs <- split(seq.int(k), seq.int(k))
    }else{
      ## find the point clusters with fixed eps
      pcs <- lapply(xyzs, pointCluster, eps=eps, quite = TRUE)
      pcs_num <- vapply(pcs, function(.ele) length(unique(.ele$cluster)),
                        numeric(1L))
      if(all(pcs_num==1)){
        ## split the xyzs into equal N spices
        pcs <-cut(seq.int(k), breaks = N, labels = FALSE, include.lowest = TRUE)
        pcs <- split(seq.int(k), pcs)
      }else{
        ## get all clusters
        pcs <- getClusters(pcs, N=N)
      }
    }
  }
  ## summarize the signals for the clusters
  xyzs <- lapply(xyzs, function(xyz){
    do.call(rbind, lapply(pcs, function(idx){
      colMeans(xyz[idx, , drop=FALSE])
    }))
  })
  
  xyzs <- findVariablePoints(xyzs, N=N) # N x 3 x M
  ## step4, Build Feature Matrix [M cells x 3N features]
  xyzs <- N3M2M3N(xyzs)
  ## step5, fill NA with 0
  xyzs[is.na(xyzs)] <- 0
  ## step5, do cluster
  dbscan_result <- pointCluster(xyzs, eps=eps)
}

checkXYZdim <- function(xyzs){
  stopifnot(is.list(xyzs))
  d <- vapply(xyzs, dim, integer(2L))
  d <- unique(t(d))
  if(nrow(d)!=1){
    stop('The input xyzs must have same dimentions')
  }
}
rescalePointClouds <- function(xyzs){
  stopifnot(is.list(xyzs))
  lapply(xyzs, function(xyz){
    stopifnot(is.matrix(xyz) || is.data.frame(xyz))
    colnames(xyz) <- tolower(colnames(xyz))
    stopifnot(all(c('x', 'y', 'z') %in% colnames(xyz)))
    xyz <- xyz[, c('x', 'y', 'z'), drop=FALSE]
    center <- colMeans(xyz, na.rm = TRUE)
    centered <- sweep(xyz, 2, center, '-')
    max_dist <- max(sqrt(rowSums(centered^2)), na.rm = TRUE)
    scaled <- centered / max_dist
    as.data.frame(scaled)
  })
}

fill_NA <- function(xyz, old_count=-1){
  id <- which(is.na(xyz[, 'x']))
  if(length(id)){
    ## fill both ends has values
    id0 <- id-1
    id0[id0<1] <- 1
    id1 <- id+1
    id1[id1>nrow(xyz)] <- nrow(xyz)
    id0[id0 %in% id] <- id1[id0 %in% id]
    id1[id1 %in% id] <- id0[id1 %in% id]
    res0 <- xyz[id0, , drop=FALSE]
    res1 <- xyz[id1, , drop=FALSE]
    res <- (xyz[id0, , drop=FALSE] + xyz[id1, , drop=FALSE])/2
    res[is.na(res[, 'x']) & !is.na(res0[, 'x']), ] <- 
      res0[is.na(res[, 'x']) & !is.na(res0[, 'x']), ]
    res[is.na(res[, 'x']) & is.na(res0[, 'x']), ] <- 
      res0[is.na(res[, 'x']) & is.na(res1[, 'x']), ]
    xyz[id, ] <- res
    id <- which(is.na(xyz[, 'x']))
    if(length(id)!=old_count){
      return(fill_NA(xyz=xyz, old_count=length(id)))
    }
  }
  return(xyz)
}

getClusters <- function(pcs, N){
  clusters <- lapply(pcs, function(.ele) .ele$cluster)
  clusters <- lapply(clusters, function(.ele) split(seq_along(.ele), .ele))
  clusters <- unlist(clusters, recursive = FALSE)
  clusters <- vapply(clusters, paste, character(1L), collapse=',')
  cnt <- table(clusters)
  cnt <- sort(cnt, decreasing = TRUE)
  clusters <- strsplit(names(cnt)[seq.int(min(length(cnt), N))], split=',')
  clusters <- lapply(clusters, as.numeric)
  return(clusters)
}

reshapeXYZs <- function(xyzs){
  checkXYZdim(xyzs)
  N <- nrow(xyzs[[1]])
  M <- length(xyzs)
  # Initialize empty array: [N x 3 x M]
  xyz_array <- array(NA, dim = c(N, 3, M))
  # Fill array
  for (i in seq.int(M)) {
    xyz_array[, , i] <- as.matrix(xyzs[[i]])
  }
  return(xyz_array)
}

findVariablePoints <- function(xyzs, N=2000){
  k <- nrow(xyzs[[1]])
  xyzs <- reshapeXYZs(xyzs)
  if(k>N){
    # Compute variance across cells for each point (x, y, z separately)
    point_vars <- apply(xyzs, c(1, 2), var, na.rm = TRUE)  # shape: N x 3
    
    # get means variance across x, y, z to get a single score per point
    point_mean_var <- rowMeans(point_vars, na.rm = TRUE)  # shape: N
    
    # find top N most variable points
    top_indices <- order(point_mean_var, decreasing = TRUE)[
      seq.int(min(length(point_mean_var), N))]
    # extract the top points
    xyzs <- xyzs[top_indices, , , drop=FALSE]
  }
  return(xyzs)
}

N3M2M3N <- function(xyzs){
  # Step 1: Permute axes to [M x N x 3]
  xyzs <- aperm(xyzs, c(3, 1, 2)) # now [M x N x 3]
  # Step 2: Flatten last two dimensions to make [M x (N*3)]
  M <- dim(xyzs)[1]
  N <- dim(xyzs)[2]
  # Flatten each [N x 3] matrix into a vector (row-wise)
  features <- matrix(NA, nrow = M, ncol = N * 3)
  for (i in 1:M) {
    features[i, ] <- as.vector(xyzs[i, , ])  # row-wise flattening
  }
  features
}
