#' Create the spatial distance matrix
#' @description
#' Create the spatial distance matrix for given 3D coordinates.
#' @param xyz A GRanges object with x, y, z coordinates
#' @param output "matrix" or "dist".
#' @param fill_NA Fill the missing value or not.
#' @param ... Parameters could be used by downstream function.
#' @return A matrix of Euclidean distance with fixed bins.
#' @importFrom stats dist
#' @importFrom Seqinfo seqnames
#' @importFrom BiocGenerics start end
#' @importFrom S4Vectors mcols
#' @export
#' @examples
#' xyz.gr <- readRDS(system.file('extdata', '4DNFI1UEG1HD.chr21.FLAMINGO.res.rds',
#'  package='geomeTriD'))
#' bin_size <- 5000 #width(xyz.gr)[1]
#' sdm <- spatialDistanceMatrix(xyz.gr)
#' spatialDistanceHeatmap(sdm)
#' head(boundaryScoreTAD(sdm, bin_size=bin_size))
#' head(hierarchicalClusteringTAD(sdm, bin_size=bin_size))
#' library(BSgenome.Hsapiens.UCSC.hg19)
#' compartment(xyz.gr, genome=BSgenome.Hsapiens.UCSC.hg19)
spatialDistanceMatrix <- function(xyz, output='matrix', fill_NA=FALSE, ...){
  output <- match.arg(output, c('matrix', 'dist'))
  isGR <- is(xyz, 'GRanges')
  if(isGR){
    xyz <- sort(xyz)
    names(xyz) <- NULL
    if(!all(seqnames(xyz)==seqnames(xyz)[1])){
      stop('all seqnames should be same.')
    }
    if(fill_NA){
      xyz <- fill_gap(xyz)
    }
    rn <- (start(xyz)-1 + end(xyz))/2
    xyz <- as.data.frame(mcols(xyz))
  }else{
    if(fill_NA){
      xyz <- fill_NA(xyz)
    }
  }
  xyz <- checkXYZ(xyz)
  d <- dist(xyz, method = 'euclidean')
  if(isGR){
    attr(d, 'Labels') <- rn
  }
  if(output=='dist') return(d)
  d <- as.matrix(d)
  return(d)
}

fill_gap <- function(xyz.gr){
  rg <- range(xyz.gr)
  if(length(rg)!=1){
    stop('The xyz.gr must be a single chromosome data and without strand info.')
  }
  w <- unique(width(xyz.gr))
  if(length(w)!=1){
    stop('The width of the xyz.gr must be same.')
  }
  gr <- slidingWindows(rg, width = w, step=w)[[1]]
  ol <- findOverlaps(xyz.gr, gr, type = 'equal')
  if(any(duplicated(queryHits(ol)))){
    stop('There is duplicated xyz.gr.')
  }
  mcols(gr) <- matrix(ncol = ncol(mcols(xyz.gr)),
                      dimnames = list(NULL, colnames(mcols(xyz.gr))))
  mcols(gr)[subjectHits(ol), ] <- mcols(xyz.gr)[queryHits(ol), ]
  mcols(gr) <- fill_NA(as.data.frame(mcols(gr)))
  return(gr)
}

safeIndex <- function(idx, n){
  idx[idx>0 & idx<=n]
}
gaussian_kernel <- function(size = 5, sigma = 1, ...) {
  # Ensure odd size
  if (size %% 2 == 0) stop("Kernel size must be odd")
  center <- floor(size / 2)
  x <- seq(-center, center)
  kernel <- outer(x, x, function(x, y) exp(-(x^2 + y^2)/(2 * sigma^2)))
  kernel <- kernel / sum(kernel)
  return(kernel)
}

#' Gaussian blur
#' @description
#' Do Gaussian for the distance matrix.
#' @param mat A matrix.
#' @param size The kernel size
#' @param sigma The strength of the blur.
#' @param ... Not used.
#' @return A matrix.
#' @export
#' @examples
#' mat <- matrix(runif(100), 10, 10)
#' blurred_mat <- gaussianBlur(mat, size = 5, sigma = 1)
gaussianBlur <- function(mat, size = 5, sigma = 1, ...) {
  stopifnot(is.matrix(mat))
  stopifnot(is.numeric(size))
  stopifnot(is.numeric(sigma))
  kernel <- gaussian_kernel(size, sigma)
  
  pad <- floor(size / 2)
  padded <- matrix(0, nrow = nrow(mat) + 2*pad, ncol = ncol(mat) + 2*pad)
  padded[(pad+1):(nrow(padded)-pad), (pad+1):(ncol(padded)-pad)] <- mat
  
  blurred <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      region <- padded[i:(i+size-1), j:(j+size-1)]
      blurred[i, j] <- sum(region * kernel)
    }
  }
  
  return(blurred)
}

#' @description boundaryScore calculate the boundary score for a distance matrix.
#' Please note that, this boundary score is the reverse of insulation score
#' because we are using the distance matrix but not the interaction matrix.
#' @rdname spatialDistanceMatrix
#' @param background The background window size for local background.
#' @export
#' @return boundaryScore return a data frame with the boundary score and the Z scores.
boundaryScore <- function(spatialDistances, window=5, background = 10, ...){
  stopifnot(window>2)
  if(!is.matrix(spatialDistances)) spatialDistances <- as.matrix(spatialDistances)
  stopifnot(is.matrix(spatialDistances))
  n <- nrow(spatialDistances)
  if(window>=n/2-1){
    stop('window is too big.')
  }
  stopifnot(background>0)
  sigs <- lapply(seq(window+1, n-window), FUN=function(i){
    region <- seq(i-window, i+window)
    background.left <- seq(i-window-background, i-window-1)
    background.right <- seq(i+window+1, i+window+background)
    background <- c(background.left, background.right)
    background <- safeIndex(background, n) 
    inner <- mean(spatialDistances[region, region], na.rm = TRUE)
    outer <- mean(spatialDistances[background, background], na.rm = TRUE)
    outer_sd <- sd(spatialDistances[background, background], na.rm = TRUE)
    c(inner, outer, outer_sd)
  })
  sigs <- rbind(matrix(NA, nrow=window, ncol=3),
                do.call(rbind, sigs),
                matrix(NA, nrow=window, ncol=3))
  score <- sigs[, 1]
  Z <- (sigs[, 1] - sigs[, 2])/sigs[, 3]
  return(data.frame(score=score, Z=Z,
                    background=sigs[, 2], background_sd=sigs[, 3],
                    row.names=rownames(spatialDistances)))
}

#' @description boundaryScoreTAD assign the TAD boundaries via boundary score.
#' @rdname spatialDistanceMatrix
#' @param window The window size for boundary score.
#' @param bin_size The bin size.
#' @param Z_cutoff The Z_cutoff value for boundary. 
#' @param norm Normalize the boundary score or not.
#' @param boundaryScores The output of boundaryScore.
#' @return boundaryScoreTAD return a list of the index or the positions of coordinates.
#' @importFrom stats sd
#' @importFrom utils combn
#' @export
boundaryScoreTAD <- function(spatialDistances, bin_size, window=5,
                           Z_cutoff=2.3, norm=FALSE,
                           boundaryScores, ...){
  stopifnot(window>2)
  stopifnot(is.numeric(bin_size))
  if(missing(boundaryScores)){
    boundaryScores <- boundaryScore(spatialDistances, window = window, ...)
  }else{
    if(!all(c('Z', 'score') %in% colnames(boundaryScores))){
      stop("boundaryScore must be output of boundaryScore")
    }
  }
  
  cn <- ifelse(norm, 'Z', 'score')
  n <- nrow(boundaryScores)
  left <- ceiling(window - window/2)
  roll_max <- vapply(seq(left+1, n-left), function(i){
    i <- which.max(boundaryScores[seq(i-left, i+left), cn])==left+1
    if(length(i)==1) return(i)
    return(FALSE)
  }, FUN.VALUE = logical(1L))
  b <- c(TRUE, rep(FALSE, left-2), roll_max, rep(FALSE, left), TRUE)
  b <- combn(which(b), 2, simplify = TRUE)
  b[1, b[1, ]!=1] <- b[1, b[1, ]!=1] + 1
  Z <- apply(b, 2, function(.ele){
    region <- seq(.ele[1], .ele[2])
    w <- abs(diff(.ele))
    background <- safeIndex(seq(.ele[1]-w, .ele[2]+w), n)
    background <- background[!background %in% region]
    background.top <- background[background<.ele[1]]
    background.right <- background[background>.ele[1]]
    inner <- mean(spatialDistances[region, region], na.rm = TRUE)
    inner.sd <- sd(spatialDistances[region, region], na.rm = TRUE)
    if(is.na(inner.sd)) inner.sd <- 1
    if(inner.sd==0) inner.sd <- 1
    outer.top <- mean(spatialDistances[background.top, region],
                       na.rm = TRUE)
    outer.right <- mean(spatialDistances[region, background.right],
                        na.rm = TRUE)
    if(is.na(outer.top)&&is.na(outer.right)){
      -inner/inner.sd
    }else{
      outer <- max(outer.top, outer.right, na.rm = TRUE)
      (outer - inner)/inner.sd
    }
  })
  b <- b[, Z>Z_cutoff, drop=FALSE]
  if(ncol(b)==0){
    b <- matrix(c(1, n), ncol=1)
  }
  b <- t(b)
  b <- as.data.frame(b)
  colnames(b) <- c('first', 'second')
  if(length(rownames(boundaryScores))==nrow(boundaryScores)){
    n <- as.numeric(rownames(boundaryScores))
    b$coor1 <- n[b$first]-bin_size/2+1
    b$coor2 <- n[b$second]+bin_size/2
  }
  return(b)
}

#' Automate Cluster Number Selection
#' @description
#' Automate cluster number selection using Silhouette Width
#' @param d A dist object.
#' @param hc A hclust object.
#' @param max_k The maximal k.
#' @return The best k number.
#' @importFrom cluster silhouette
#' @export
#' @examples
#' x <- matrix(rnorm(100), nrow = 5)
#' d <- dist(x)
#' hc <- hclust(d)
#' autoK(d, hc)
autoK <- function(d, hc, max_k){
  stopifnot(is(d, 'dist'))
  stopifnot(is(hc, 'hclust'))
  if(missing(max_k)) max_k <- round(attr(d, 'Size')/4)
  if(max_k<2) max_k <- 2
  sil_width <- vapply(seq.int(max_k)[-1], function(k) {
    clusters_try <- cutree(hc, k = k)
    sil <- silhouette(clusters_try, d)
    mean(sil[, 3])
  }, numeric(1L))
  k <- which.max(sil_width)+1
}

#' @description hierarchicalClusteringTAD assign the TAD boundaries via hierarchical clustering.
#' @rdname spatialDistanceMatrix
#' @param k The cluster number. The final TAD numbers will be no greater than this number.
#' @return hierarchicalClusteringTAD return a list of the index or the positions of coordinates.
#' @importFrom cluster silhouette
#' @importFrom stats hclust cutree
#' @export
hierarchicalClusteringTAD <- function(spatialDistances, bin_size, window=5, k, ...){
  stopifnot(is.numeric(bin_size))
  stopifnot(is.matrix(spatialDistances)||is(spatialDistances, 'dist'))
  spatialDistances <- fill_NA(spatialDistances)
  if(is.matrix(spatialDistances)) spatialDistances <- as.dist(spatialDistances)
  hc <- hclust(spatialDistances, method = "average")
  if(missing(k)||k=='auto'){
    ## Automate Cluster Number Selection Using Silhouette Width
    k <- autoK(spatialDistances, hc)
  }else{
    stopifnot(is.numeric(k))
  }
  clusters <- cutree(hc, k)
  ## Add minimum block size filtering
  r <- rle(clusters)
  # Filter runs by length >= min_size
  keep_idx <- r$lengths >= window
  keep_idx[length(keep_idx)] <- TRUE
  keep_idx <- which(keep_idx)
  # Compute starts and ends for filtered runs
  ends <- cumsum(r$lengths)[keep_idx]
  ends <- ends[ends!=1]
  starts <- c(1, ends[-length(ends)]+1)  # careful indexing
  
  # Build filtered TAD data.frame
  b <- data.frame(
    first = starts,
    second = ends
  )
  if(length(attr(spatialDistances, 'Labels'))){
    n <- as.numeric(attr(spatialDistances, 'Labels'))
    b$coor1 <- n[b$first]-bin_size/2+1
    b$coor2 <- n[b$second]+bin_size/2
  }
  return(b)
}

#' @description
#' compartment calculate the compartment by principal component analysis.
#' @rdname spatialDistanceMatrix
#' @param xyz.gr A GRanges object with x,y,z coordinates.
#' @param genome A BSgenome object
#' @param minWidth The minimal width of input region.
#' @return compartment return a GRanges object with A,B annotations.
#' @importFrom stats prcomp cor
#' @importFrom Biostrings getSeq letterFrequency 
#' @importFrom GenomicRanges GRangesList slidingWindows
#' @importFrom IRanges findOverlaps
#' @importFrom S4Vectors subjectHits queryHits metadata metadata<-
#' @export
compartment <- function(xyz.gr, genome, minWidth=1){
  if(!missing(genome)){
    stopifnot(is(genome, 'BSgenome'))
  }
  if(!is(xyz.gr, 'GRanges')){
    tile <- GRanges('seq1', IRanges(seq.int(nrow(xyz.gr)), width=1))
    if(all(c('x', 'y', 'z') %in% tolower(colnames(xyz.gr)))){
      mcols(tile) <- fill_NA(checkXYZ(xyz.gr))
    }
  }else{
    if(width(xyz.gr)[1]>minWidth){
      minWidth <- width(xyz.gr)[1]
      doSm <- FALSE
    }else{
      doSm <- TRUE
    }
    xyz.sm <- range(xyz.gr)
    if(length(xyz.sm)!=1){
      stop('single chromosome only.')
    }
    if(doSm){
      tile <- slidingWindows(xyz.sm, width = minWidth, step = minWidth)
      tile <- tile[[which(lengths(tile)>0)]]
      ol <- findOverlaps(tile, xyz.gr, minoverlap = width(xyz.gr)[1]/2)
      ol <- split(subjectHits(ol), queryHits(ol))
      xyz <- as.data.frame(mcols(xyz.gr))
      xyz <- checkXYZ(xyz)
      xyz <- fill_NA(xyz)
      xyz <- lapply(ol, function(idx){
        colMeans(xyz[idx, ], na.rm=TRUE)
      })
      tile <- tile[as.numeric(names(xyz))]
      mcols(tile) <- do.call(rbind, xyz)
    }else{
      xyz <- as.data.frame(mcols(xyz.gr))
      xyz <- checkXYZ(xyz)
      xyz <- fill_NA(xyz)
      mcols(xyz.gr) <- xyz
      tile <- xyz.gr
    }
  }
  if(length(tile$x)==length(tile)){
    spatialDistances <- spatialDistanceMatrix(tile)
  }else{
    spatialDistances <- xyz.gr
  }
  spatialDistances <- 
    fill_NA(spatialDistances)
  stopifnot(is.matrix(spatialDistances))
  if(!missing(genome) && is(xyz.gr, 'GRanges')){
    seq <- getSeq(genome, tile)
    GC_content <- letterFrequency(seq, letters="CG")
  }
  # 1. Calculate Pearson correlation matrix
  cor_mat <- cor(as.matrix(spatialDistances), use = "pairwise.complete.obs")
  # 2. PCA on the correlation matrix
  pc <- prcomp(cor_mat, center = TRUE, scale. = FALSE)
  # 3. check the correlation of PCs and GC contents
  pc1 <- pc$x[, 1]
  if(!missing(genome) && is(xyz.gr, 'GRanges')){
    GC_cor <- cor(pc1, GC_content, method = 'pearson')
    if(GC_cor<0) pc1 <- -1*pc1
  }
  compartment <- ifelse(pc1 > 0, "A", "B")
  compartment <- split(tile, compartment)
  compartment <- reduce(GRangesList(compartment))
  AB <- rep(names(compartment), lengths(compartment))
  compartment <- unlist(compartment)
  compartment$compartment <- AB
  compartment <- sort(compartment)
  compartment$label <- compartment$compartment
  compartment$type <- "compartment"
  colorCode <- c('active'='#EA262E', 'inactive'='#179281')
  compartment$col <- ifelse(is.na(compartment$compartment), 'gray', 
                             ifelse(grepl('^A', compartment$compartment),
                                    colorCode['active'],
                                    colorCode['inactive']))
  metadata(compartment) <- list(pc1=pc1)
  return(compartment)
}

#' @rdname spatialDistanceMatrix
#' @description
#' spatialDistanceHeatmap will use base R to plot the spatial distance matrix.
#' @param spatialDistances The output of spatialDistanceMatrix or the input of spatialDistanceMatrix.
#' @param components The components to plot.
#' @param col a list of colors such as that generated by hcl.colors, gray.colors or similar functions.
#' @param at The label position of X, and Y axis.
#' @param label_unit unit for labels. 'M', 1e6; 'K', 1e3, 'G', 1e9.
#' @param d_cutoff The maximal cutoff value of distance matrix.
#' @param Gaussian_blur Do Gaussian blur or not.
#' @param useRaster logical; if TRUE a bitmap raster is used to plot the image instead of polygons.
#' @importFrom graphics image axis rect layout
#' @importFrom grDevices hcl.colors
#' @export
spatialDistanceHeatmap <- function(spatialDistances,
                                   components=c('compartment',
                                                'boundaryScoreTAD',
                                                'hierarchicalClusteringTAD'),
                                   col=hcl.colors(n=12, "OrRd"),
                                   at=seq(0, 1, length.out=2),
                                   label_unit='M',
                                   window=5,
                                   background = 10,
                                   d_cutoff=Inf,
                                   Z_cutoff=2.3,
                                   norm=FALSE,
                                   Gaussian_blur=FALSE,
                                   useRaster=FALSE,
                                   ...){
  bin_size <- 10000 # no meaning.
  compartment <- NULL
  if(is(spatialDistances, 'GRanges')) {
    spatialDistances <- spatialDistanceMatrix(spatialDistances)
  }
  if(is.numeric(d_cutoff)||!is.infinite(d_cutoff)){
    spatialDistances[spatialDistances>d_cutoff] <- d_cutoff
  }
  origin <- spatialDistances[,
                             rev(seq.int(ncol(spatialDistances)))]
  if(Gaussian_blur){
    spatialDistances <- gaussianBlur(spatialDistances, ...)
  }
  if('compartment' %in% components) compartment <- compartment(spatialDistances)
  stopifnot(is.matrix(spatialDistances))
  label_unit <- match.arg(toupper(label_unit), c('G', 'M', 'K'))
  label_unit_num <- c('G'=1e9, 'M'=1e6, 'K'=1e3)[label_unit]
  label <- as.numeric(rownames(origin))
  if(any(is.na(label))){
    stop("spatialDistances must be output of spatialDistanceMatrix")
  }
  if('boundaryScoreTAD' %in% components){
    score <- boundaryScore(spatialDistances, window = window,
                            background = 2*window)
    TAD_boundaries <- boundaryScoreTAD(spatialDistances,
                                       bin_size = bin_size,
                                       window = window,
                                       Z_cutoff = Z_cutoff,
                                       boundaryScores=score)
  }else{
    TAD_boundaries <- NULL
  }
  if('hierarchicalClusteringTAD' %in% components){
    TAD_boundaries2 <- hierarchicalClusteringTAD(spatialDistances,
                                                 bin_size = bin_size,
                                                 window = window,
                                                 ...)
  }
  label <- quantile(label, probs = at)/label_unit_num
  label <- paste0(round(label, digits = 1), label_unit)
  op <- par('mar')
  op <- par('mar'=op)
  on.exit({
    layout(mat = matrix(1))
    par(op)
  })
  if(length(compartment)){
    if('boundaryScoreTAD' %in% components){
      layout(matrix(c(1, 2, 3), nrow=3), heights = c(1.25, 0.25, 10))
    }else{
      layout(matrix(c(1, 2), nrow=2), heights = c(1.25, 10))
    }
  }else{
    if('boundaryScoreTAD' %in% components){
      layout(matrix(c(1, 2), nrow=2), heights = c(1.25, 10))
    }else{
      layout(matrix(c(1), nrow=1), heights = 1)
    }
  }
  top <- 1
  if('boundaryScoreTAD' %in% components){
    par("mar"=c(0, 3.1, 1.1, 2.1))
    cn <- ifelse(norm, 'Z', 'score')
    ylim <- range(score[, cn], na.rm = TRUE)
    ylim[2] <- ylim[2]*1.1
    plot(score[, cn], type = 'l', col = 'blue',
         xaxt = 'n', yaxt = 'n',  xaxs="i", yaxs="i",
         xlim=c(0, nrow(score)*1.1),
         ylim=ylim,
         frame.plot=FALSE, xlab="", ylab="")
    TAD_boundaries_uniq <- sort(unique(c(TAD_boundaries$first, 
                                         TAD_boundaries$second)))
    points(TAD_boundaries_uniq,
           score[TAD_boundaries_uniq, cn], col='red', pch=19)
    text(nrow(score)*1.01,
         median(score[, cn], na.rm = TRUE),
         label='boundary', adj=c(0, 0.5))
    top <- 0
  }
  if(length(compartment)){
    par("mar"=c(0, 3.1, top + 0.1, 2.1))
    pc1 <- metadata(compartment)$pc1
    image(matrix(pc1>0, ncol=1), xlim=c(0, 1.1), axes=FALSE, frame.plot=FALSE)
    text(1.01, 0.4, label='compartment', adj=c(0, 0.5))
    top <- 0
  }
  par("mar"=c(3.1, 3.1, top + 0.1, 2.1))
  plot(1, type="n", xaxt = 'n', yaxt = 'n', xaxs = "i", yaxs = "i",
       axes=FALSE, frame.plot=FALSE,
       xlab="", ylab="", xlim=c(0, 1.1), ylim=c(0, 1))
  image(origin, col=col, axes = FALSE, add=TRUE, useRaster=useRaster)
  # add TAD_boundaries
  if('boundaryScoreTAD' %in% components){
    for(i in seq.int(nrow(TAD_boundaries))){
      b1 <- TAD_boundaries$first[i]
      b2 <- TAD_boundaries$second[i]
      b1 <- b1/ncol(origin)
      b2 <- b2/ncol(origin)
      segments(b1, 1-b1, b2, 1-b1)
      segments(b2, 1-b1, b2, 1-b2)
      #rect(b1, 1-b1, b2, 1-b2, col=NA, border='black', ...)
    }
  }
  # hierarchicalClusteringTAD
  if('hierarchicalClusteringTAD' %in% components){
    for(i in seq.int(nrow(TAD_boundaries2))){
      b1 <- TAD_boundaries2$first[i]
      b2 <- TAD_boundaries2$second[i]
      b1 <- b1/ncol(origin)
      b2 <- b2/ncol(origin)
      segments(b1, 1-b1, b1, 1-b2)
      segments(b1, 1-b2, b2, 1-b2)
      #rect(b1, 1-b1, b2, 1-b2, col=NA, border='black', ...)
    }
  }
  # axis
  axis(1, at=at, labels = label)
  axis(2, at=at, labels = rev(label))
  # legend
  # Define gradient colors and breaks
  n <- length(col)
  zlim <- range(origin, na.rm = TRUE)

  # Coordinates for the gradient legend
  xleft <- 1.01
  xright <- 1.05
  ybottom <- 0.2
  ytop <- 0.8

  # Draw the gradient legend
  rects <- seq(ybottom, ytop, length.out = n)
  for (i in (n-1):1) {
    rect(xleft, rects[i], xright, rects[i+1], col = col[n-i], border = NA)
  }
  # Add labels
  text(x = 1.01 , y = 0.19,
       labels = formatC(zlim[2], digits = 1),
       adj = c(0, 1))
  text(x = 1.01 , y = 0.81,
       labels = formatC(zlim[1], digits = 1),
       adj = c(0, 0))
}
