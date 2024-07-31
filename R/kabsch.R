#' Kabsch Algorithm
#' 
#' Aligns two sets of points via rotations and translations.
#' 
#' Kabsch–Umeyama algorithm is a method for aligning and comparing the similarity between two sets of points. It finds the optimal translation, rotation and scaling by minimizing the root-mean-square deviation (RMSD) of the point pairs.
#' 
#' @noRd
#'   
#' @param query,subject n x d matrix of points to align to to \code{subject}.
#' @return Matrix or GRanges \code{query} rotated and translated so that the ith point 
#'  is aligned to the ith point of \code{subject} in the least-squares sense.
#' @references
#' \url{https://en.wikipedia.org/wiki/Kabsch_algorithm}
#' \url{https://zpl.fi/aligning-point-patterns-with-kabsch-umeyama-algorithm/}
#' @importFrom S4Vectors queryHits subjectHits mcols
#' @importFrom IRanges findOverlaps
kabsch <- function(query, subject) {
  query0 <- query
  ol <- numeric(0L)
  if(is(query, 'GRanges')){
    if(is(subject, 'GRanges')){
      ## get the same coordinates
      ol <- findOverlaps(query, subject, type = "equal")
      query <- query[queryHits(ol)]
      subject <- subject[subjectHits(ol)]
      subject <- as.matrix(mcols(subject))
    }
    query <- as.matrix(mcols(query))
  }
  stopifnot (identical(dim(subject), dim(query)))
  dims <- dim(query)
  
  ## center the points
  query <- scale(query, center = TRUE, scale = FALSE)
  subject <- scale(subject, center = TRUE, scale = FALSE)
  
  ## determine the cross-covariance matrix
  ccm <- crossprod(query, subject)
  
  ## compute its singular value decomposition H = U D t(V)
  svd_res <- svd(ccm)
  # use the sign of the determinant to ensure a right-hand coordinate system
  d = sign(det(svd_res$u) * det(svd_res$v))
  
  ## The rotation matrix will have (ncol - 1) leading ones in the diagonal
  diag_ones <- rep(1, dims[2] - 1)
  S <- diag(c(diag_ones, d))
  
  # rotation matrix R = U S t(V)
  R <- svd_res$v %*% tcrossprod(S, svd_res$u)
  
  # Rotate and then translate to the original centroid location of subject
  if(length(ol)){
    query <- sweep(t(tcrossprod(R, as.matrix(mcols(query0)))),
                   2, -attr(subject, "scaled:center"))
    colnames(query) <- colnames(mcols(query0))
    mcols(query0) <- query
    query <- query0
  }else{
    query <- sweep(t(tcrossprod(R, query)), 2, -attr(subject, "scaled:center"))
  }
  query
}
