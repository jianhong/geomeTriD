#' cluster single cell 3D structures
#' @description
#' Perform Hierarchical clustering for given 3D structures.
#' @param xyzs A data.frame with x, y, z coordinates or output of cellDistance.
#' @param TADs A list of index vectors, where each vector represents a TAD.
#'  For example, if the first TAD spans the 2nd to 4th coordinates and the
#'   second spans the 8th to 10th coordinates, the list would be:
#'    list(c(2, 3, 4), c(8, 9, 10)).
#' @param method The agglomeration method to be used for \link{hclust}.
#'  Default is 'ward.D2'.
#' @param quite Print the message or not.
#' @param parallel Run parallel by future or not. 
#' @param ... not used.
#' @return cellClusters return an object of class hclust.
#' @export
#' @importFrom stats var cutree hclust
#' @importFrom future.apply future_mapply
#' @importFrom progressr with_progress progressor
#' @examples
#' set.seed(1)
#' xyzs <- lapply(seq.int(20), function(i){
#'   matrix(sample.int(100, 60, replace = TRUE),
#'    nrow=20, dimnames=list(NULL, c('x', 'y', 'z')))
#' })
#' cd <- cellDistance(xyzs)
#' cc <- cellClusters(cd)
#' # plot(cc)
#' cutree(cc, k=3)
cellClusters <- function(xyzs, TADs, method='ward.D2', quite=FALSE,
                         parallel=FALSE,...){
  method <- match.arg(method,
                      choices = c("ward.D", "ward.D2", "single",
                                  "complete", "average", "mcquitty",
                                  "median", "centroid"))
  ## calculate distances among cells
  if(is(xyzs, 'dist')){
    dst <- xyzs
  }else{
    dst <- cellDistance(xyzs=xyzs, TADs = TADs,
                        quite = quite, parallel = parallel,
                        ...)
  }
  ## cluster
  hc <- hclust(dst, method = method)
}

#' cellDistance calculate distance matrix 
#' @description
#' Calculate euclidean distance for each pair of cells after alignment.
#' @export
#' @return cellDistance return distance matrix as an object of 'dist'
#' @rdname cellClusters
cellDistance <- function(xyzs, TADs, quite=FALSE, parallel=FALSE, ...){
  checkXYZdim(xyzs)
  if(parallel){
    applyFUN <- future_mapply
    on.exit({
      message('After the parallel computing, please try to release the memory ',
              'by restarting clean workers: ',
              'plan(sequential); and then plan(multisession).')
    })
  }else{
    applyFUN <- mapply
  }
  n_points <- nrow(xyzs[[1]])
  if(!missing(TADs)){
    stopifnot(is.list(TADs))
    uTADs <- unlist(TADs)
    if(!is.numeric(uTADs)){
      stop('All elements in TADs list must be number.')
    }
    if(min(uTADs)<1){
      stop('All elements in TADs list should not smaller than 1')
    }
    if(max(uTADs)>n_points){
      stop('All elements in TADs list should not larger than ', n_points)
    }
  }
  ## find the center of xyzs
  ## rescale the xyzs to same size
  xyzs <- rescalePointClouds(xyzs)
  ## fill the NA with nearby points
  xyzs <- lapply(xyzs, fill_NA)
  ## summarize the signals for each TAD by their centers
  if(!missing(TADs)){
    xyzs <- lapply(xyzs, function(xyz){
      do.call(rbind, lapply(TADs, function(idx){
        colMeans(xyz[idx, , drop=FALSE])
      }))
    })
  }
  ## calculate dist
  M <- length(xyzs)
  index <- expand.grid(i=seq.int(M), j=seq.int(M))
  upper_idx <- index$i >= index$j
  values <- rep(NA, nrow(index))
  with_progress({
    total_steps <- sum(upper_idx)
    verbose <- rep(FALSE, total_steps)
    if(!quite){
      pb <- progressor(steps = min(100, total_steps))
      if(total_steps>100){
        verbose[round(seq(1, total_steps, length=100))] <- TRUE
      }else{
        verbose <- rep(TRUE, total_steps)
      }
    }
    values[upper_idx] <- applyFUN(FUN=function(a, b, v){
      if(v) pb()
      ## why use both before alignment and after alignment?
      ## the alignment has limitations:
      ## Only rigid: it can not model scaling or non-rigid deformation
      ## Sensitive to Outliers: One or two bad correspondences can distort the result.
      ##                        No built-in outlier rejection or robust loss.
      ## Does not handle partial overlaps: works best when both sets fully match
      ##                      That means too much NA values will affect the results.
      ## No Uncertainty Estimation: No confidence intervals, posterior distribution
      ##                            or measure of certainty.
      v0 <- sum(sqrt(rowSums((a - b)^2, na.rm = TRUE)),
                na.rm = TRUE)
      ## after alignment
      a <- alignCoor(a, b)
      v1 <- sum(sqrt(rowSums((a - b)^2, na.rm = TRUE)),
                na.rm = TRUE)
      ifelse(v1<v0, v1, v0)
    }, xyzs[index[upper_idx, 1]], xyzs[index[upper_idx, 2]],
    verbose,
    SIMPLIFY = TRUE)
  })
  dst <- matrix(values, nrow=M, ncol=M)
  return(as.dist(dst, diag = TRUE))
}

checkXYZ <- function(xyz){
  stopifnot(is.matrix(xyz) || is.data.frame(xyz))
  colnames(xyz) <- tolower(colnames(xyz))
  stopifnot(all(c('x', 'y', 'z') %in% colnames(xyz)))
  return(xyz)
}
checkXYZdim <- function(xyzs){
  stopifnot(is.list(xyzs))
  stopifnot(length(xyzs)>2)
  d <- vapply(xyzs, dim, integer(2L))
  d <- unique(t(d))
  if(nrow(d)!=1){
    stop('The input xyzs must have same dimentions')
  }
}
rescalePointClouds <- function(xyzs){
  stopifnot(is.list(xyzs))
  lapply(xyzs, function(xyz){
    xyz <- checkXYZ(xyz)
    xyz <- xyz[, c('x', 'y', 'z'), drop=FALSE]
    center <- colMeans(xyz, na.rm = TRUE)
    centered <- sweep(xyz, 2, center, '-')
    max_dist <- max(sqrt(rowSums(centered^2)), na.rm = TRUE)
    scaled <- centered / max_dist
    as.data.frame(scaled)
  })
}

#' fill NA values by upstream and downstream points
#' @description
#' Fill NA values by previous and next points coordinates.
#' @param xyz A matrix or data.frame with columns 'x', 'y', 'z'
#' @return A matrix or data.frame.
#' @export
#' @examples
#' xyz <- matrix(seq.int(21), ncol=3, dimnames=list(NULL, c('x', 'y', 'z')))
#' xyz[c(1, 5, 7), ] <- NA
#' fill_NA(xyz)
#' 
fill_NA <- function(xyz){
  xyz <- checkXYZ(xyz)
  id <- which(is.na(xyz[, 'x']))
  old_count <- length(id)
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
    if(length(id)<old_count){
      return(fill_NA(xyz=xyz))
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

# Build Feature Matrix from [N, 3, M] to [M cells x 3N features]
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
