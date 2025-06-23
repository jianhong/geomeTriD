#' cluster single cell 3D structures
#' @description
#' Perform Hierarchical clustering for given 3D structures.
#' @param xyzs A list of data.frame with x, y, z coordinates or output of cellDistance.
#' @param TADs A list of index vectors, where each vector represents a TAD.
#'  For example, if the first TAD spans the 2nd to 4th coordinates and the
#'   second spans the 8th to 10th coordinates, the list would be:
#'    list(c(2, 3, 4), c(8, 9, 10)).
#' @param distance_method 'SRD', 'DSDC', 'RMSD', 'NMI', 'ARI', 'NID', or 'AMI'.
#' SRD method will first perform clustering and then calculate the 
#' Sequence Relabeling Distance \link{SRD}.
#' DSDC method will calculate the Euclidean distance of \link{SDC}.
#' RMSD method will first do alignment for
#' each cell x, y, z coordinates and the calculate Root Mean Square Deviation
#' (RMSD, the square root of the mean of squared 
#' Euclidean distance between corresponding points).
#' ARI, NID, NMI, and AMI method will first perform clustering and then
#' calculate the Adjusted Rand Index (ARI), Normalized information distance (NID),
#' Normalized Mutal Information (NMI), Adjusted Mutual Information (AMI).
#' @param cluster_method The agglomeration method to be used for \link{hclust}.
#'  Default is 'ward.D2'.
#' @param rescale Re-scale the object to similar size.
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
#' cd <- cellDistance(xyzs, distance_method='RMSD')
#' cc <- cellClusters(cd)
#' # plot(cc)
#' cutree(cc, k=3)
#' cd2 <- cellDistance(xyzs, distance_method='SRD', eps=40)
cellClusters <- function(xyzs, TADs,
                         distance_method='NID',
                         cluster_method='ward.D2', 
                         rescale = TRUE,
                         quite=FALSE,
                         parallel=FALSE, ...){
  cluster_method <- match.arg(cluster_method,
                      choices = c("ward.D", "ward.D2", "single",
                                  "complete", "average", "mcquitty",
                                  "median", "centroid"))
  ## calculate distances among cells
  if(is(xyzs, 'dist')){
    dst <- xyzs
  }else{
    dst <- cellDistance(xyzs=xyzs, TADs = TADs,
                        distance_method = distance_method,
                        rescale = rescale,
                        quite = quite, parallel = parallel,
                        ...)
  }
  ## cluster
  hc <- hclust(dst, method = cluster_method)
}

#' cellDistance calculate distance matrix 
#' @description
#' Calculate distance for each pair of cells after alignment.
#' @param eps numeric or 'auto'. The size (radius) of the epsilon neighborhood. 
#' If eps is set, use DBSCAN to cluster the points for each cell.
#' @param k numeric or 'auto'. The number of groups. If k is set, use hclust to cluster
#' the points for each cell.
#' @export
#' @return cellDistance return distance matrix as an object of 'dist'
#' @importFrom aricode NMI ARI NID AMI
#' @importFrom stats dist hclust
#' @rdname cellClusters
cellDistance <- function(xyzs, TADs, 
                         distance_method=c('NID', 'RMSD', 'SRD', 'DSDC',
                                           'NMI', 'ARI', 'AMI'),
                         eps, k, 
                         rescale=TRUE, quite=FALSE, parallel=FALSE, ...){
  checkXYZdim(xyzs)
  distance_method <- match.arg(distance_method)
  if(distance_method %in% c('SRD', 'NMI', 'ARI', 'NID', 'AMI')){
    if(missing(eps) && missing(k)){
      stop('eps or k is required.')
    }
    if(!missing(eps)) stopifnot(is.numeric(eps)||eps=='auto')
    if(!missing(k)) stopifnot(is.numeric(k)||k=='auto')
  }
  stopifnot(is.logical(rescale))
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
  if(distance_method=='RMSD'){
    if(rescale) xyzs <- rescalePointClouds(xyzs)
  }
  
  ## fill the NA with nearby points
  xyzs <- lapply(xyzs, fill_NA)
  ## summarize the signals for each TAD by their centers
  if(!missing(TADs)){
    xyzs <- lapply(xyzs, function(xyz){
      do.call(rbind, lapply(TADs, function(idx){
        colMeans(xyz[idx, , drop=FALSE], na.rm = TRUE)
      }))
    })
  }
  if(distance_method=='DSDC'){
    return(dist(vapply(xyzs, SDC, FUN.VALUE = numeric(1L)),
                method = 'euclidean'))
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
      if(total_steps>100){
        verbose[round(seq(1, total_steps, length=100))] <- TRUE
      }else{
        verbose <- rep(TRUE, total_steps)
      }
    }
    if(distance_method=='RMSD'){
      if(!quite) pb <- progressor(steps = min(100, total_steps))
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
        rmsd <- function(a, b){
          sqrt(mean(rowSums((a - b)^2, na.rm = TRUE),
                    na.rm = TRUE))
        }
        v0 <- rmsd(a, b)
        ## after alignment
        a <- alignCoor(a, b)
        v1 <- rmsd(a, b)
        ifelse(v1<v0, v1, v0)
      }, xyzs[index[upper_idx, 1]], xyzs[index[upper_idx, 2]],
      verbose,
      SIMPLIFY = TRUE)
    }else{
      ## get the point clusters
      if(!missing(eps)){
        pcs <- lapply(xyzs, pointCluster, eps=eps, quite=TRUE)
        ## relabel noise 0 into others
        if(distance_method %in% c("NMI", 'NID')){
          pcs <- lapply(pcs, function(.ele){
            label <- .ele$cluster
            r <- rle(label)
            r$values <- seq_along(r$values)
            .ele$cluster <- inverse.rle(r)
            .ele
          })
        }else{
          if(distance_method %in% c('ARI', 'AMI')){
            pcs <- lapply(pcs, function(.ele){
              label <- .ele$cluster
              label[label==0] <- -1
              .ele$cluster <- label
              .ele
            })
          }else{
            pcs <- lapply(pcs, function(.ele){
              label <- .ele$cluster
              r <- rle(label)
              r$values[r$values!=0] <- seq_along(r$values[r$values!=0])
              .ele$cluster <- inverse.rle(r)
              .ele
            })
          }
        }
      }else{
        if(!quite) pb <- progressor(steps = length(xyzs))
        pcs <- lapply(xyzs, function(.ele){
          if(!quite) pb()
          d <- spatialDistanceMatrix(.ele)
          d <- gaussianBlur(d)
          tad <- hierarchicalClusteringTAD(
            d, bin_size = 2, k=k, window = max(ceiling(nrow(.ele)/100), 3))
          cluster <- rep(seq.int(nrow(tad)), tad$second-tad$first+1)
          list(cluster=cluster)
        })
      } 
      if(!quite) pb <- progressor(steps = min(100, total_steps))
      dFUN <- switch(distance_method,
                     NMI=function(...){
                       1 - NMI(..., variant = 'sqrt')
                     }, 
                     ARI=function(...){
                       1 - ARI(...)
                     },
                     NID=NID,
                     AMI=function(...){
                       1 - AMI(...)
                     },
                     SRD=SRD)
      values[upper_idx] <- applyFUN(FUN=function(a, b, v){
        if(v) pb()
        v <- dFUN(a$cluster, b$cluster)
        ifelse(is.na(v), 1, v)
      }, pcs[index[upper_idx, 1]], pcs[index[upper_idx, 2]],
      verbose, SIMPLIFY = TRUE)
    }
  })
  dst <- matrix(values, nrow=M, ncol=M)
  return(as.dist(dst, diag = TRUE))
}

#' Sequence Relabeling Distance
#' @description Compares two cluster sequences after best label alignment.
#' @param c1,c2 The cluster sequence 1 and 2.
#' @param noise The noise cluster name. Default is 0.
#' @return The mean value of hamming distance after label alignment.
#' @importFrom clue solve_LSAP
#' @importFrom stats setNames
#' @export
#' @examples
#' c1 <- c(-1, 0, 1, 1, -1, 3, 3, 5, 5, 5)   # `-1` is noise
#' c2 <- c(-1, 4, 4, 4, -1, 2, 2, 2, 2, 2)   # `-1` is noise
#' SRD(c1, c2, noise=-1)
#' 
SRD <- function(c1, c2, noise=0) {
  # Check input
  if (length(c1) != length(c2)) stop("Both cluster vectors must have the same length.")
  c1.copy <- as.character(c1)
  c2.copy <- as.character(c2)
  c1[c1==noise] <- NA
  c2[c2==noise] <- NA
  # Convert to factors to handle non-consecutive labels
  c1_fac <- as.factor(c1)
  c2_fac <- as.factor(c2)
  if(length(levels(c1_fac)) && length(levels(c2_fac))){
    # Contingency table
    if(length(levels(c1_fac))>length(levels(c2_fac))){
      contingency <- table(c2_fac, c1_fac)
    }else{
      contingency <- table(c1_fac, c2_fac)
    }
    # Hungarian algorithm to find best label alignment
    perm <- solve_LSAP(contingency, maximum = TRUE)
    # Create a mapping from c2 labels to c1-aligned labels
    df <- data.frame(c(dimnames(contingency)[[1]][seq_along(perm)],
                       dimnames(contingency)[[1]],
                       noise),
                     c(dimnames(contingency)[[2]][perm],
                       dimnames(contingency)[[1]],
                       noise))
    colnames(df) <- names(dimnames(contingency))
    df <- df[!duplicated(df[, 1]), ]
    df_map <- setNames(df$c1_fac, df$c2_fac)
    c2.copy <- df_map[c2.copy]
  }
  # Compute Hamming distance
  hamming_dist <- mean(c1.copy != c2.copy, na.rm=TRUE)
  return(hamming_dist)
}

#' Distance to centroid
#' @description
#' Calculates the mean of distance from each point to the geometric center (centroid)
#' @param xyz A data.frame with x, y, z coordinates.
#' @return The mean of squared Euclidean distance to the centroid.
#' @export
#' @examples
#' xyz <- matrix(seq.int(12), ncol = 3, dimnames=list(NULL, c('x', 'y', 'z')))
#' SDC(xyz)
SDC <- function(xyz){
  xyz <- checkXYZ(xyz)
  centroid <- colMeans(xyz, na.rm = TRUE)
  mean(colSums((t(xyz) - centroid)^2), na.rm = TRUE)
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
  if(is(xyz, 'GRanges')){
    mcols(xyz) <- fill_NA(as.data.frame(mcols(xyz)))
    return(xyz) 
  }
  # xyz <- checkXYZ(xyz)
  stopifnot(is.matrix(xyz)||is.data.frame(xyz)||is(xyz, 'dist'))
  is_dist <- is(xyz, 'dist')
  if(is_dist){
    xyz <- as.matrix(xyz)
  }
  if(!any(is.na(xyz))){
    return(xyz)
  }
  half <- FALSE
  if(all(c('x', 'y', 'z') %in% tolower(colnames(xyz)))){
    colnames(xyz) <- tolower(colnames(xyz))
    xyz <- xyz[, c('x', 'y', 'z')]
  }else{
    if(nrow(xyz)==ncol(xyz)){
      half <- TRUE
    }else{
      stop('Not proper xyz.')
    }
  }
  id_x <- 1
  if(all(is.na(xyz[-id_x, id_x]))){
    id_x <- apply(xyz, 2, function(.ele) which(!is.na(.ele)), simplify = FALSE)
    id_x <- mapply(id_x, seq_along(id_x), FUN=function(i, j){
      i[i!=j]
    }, SIMPLIFY = FALSE)
    id_x <- which(lengths(id_x)>0)
    if(length(id_x)<1){
      stop('All NA values')
    }
    id_x <- id_x[1]
  }
  id <- is.na(xyz[, id_x])
  id.rle <- rle(id)
  ## fill both ends
  if(id.rle$values[1]){
    for(i in seq.int(id.rle$lengths[1])){
      xyz[i, ] <- xyz[id.rle$lengths[1]+1, ]
      if(half){
        xyz[, i] <- xyz[, id.rle$lengths[1]+1]
        xyz[i, i] <- 0
      }
    }
  }
  if(id.rle$values[length(id.rle$values)]){
    j <- cumsum(id.rle$lengths)[length(id.rle$lengths)-1]
    for(i in seq.int(nrow(xyz))[-seq.int(j)]){
      xyz[i, ] <- xyz[j, ]
      if(half){
        xyz[, i] <- xyz[, j]
        xyz[i, i] <- 0
      }
    }
  }
  id <- which(is.na(xyz[, id_x]))
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
    res[is.na(res[, id_x]) & !is.na(res0[, id_x]), ] <- 
      res0[is.na(res[, id_x]) & !is.na(res0[, id_x]), ]
    res[is.na(res[, id_x]) & is.na(res0[, id_x]), ] <- 
      res0[is.na(res[, id_x]) & is.na(res1[, id_x]), ]
    xyz[id, ] <- res
    if(half){
      xyz[, id] <- t(res)
      for(i in id){
        xyz[i, i] <- 0
      }
    }
    id <- which(is.na(xyz[, id_x]))
    if(length(id)<old_count){
      return(fill_NA(xyz=xyz))
    }
  }
  if(half){
    NAs <- which(is.na(xyz))
    xyz[NAs] <- (xyz[NAs-1] + xyz[NAs+1])/2
    xyz[is.na(xyz)] <- 0
    if(is_dist){
      xyz <- as.dist(xyz)
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
