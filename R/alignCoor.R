#' Aligns two sets of genomic with x,y,z
#' 
#' Aligns two sets of points via rotations and translations by Kabsch Algorithm.
#' 
#' @param query,subject GRanges objects to alignment.
#' @return A GRanges object of \code{query} aligned  to \code{subject}.
#' @export
#' @examples
#' x <- readRDS(system.file('extdata', '4DNFI1UEG1HD.chr21.FLAMINGO.res.rds',
#'     package='geomeTriD'))
#' res <- alignCoor(x, x)
#' A <- view3dStructure(x, k=3, renderer='none')
#' B <- view3dStructure(res, k=3, renderer='none')
#' B <- lapply(B, function(.ele){
#'   .ele$side <- 'right'
#'   .ele
#' })
#' threeJsViewer(c(A, B))

alignCoor <- function(query, subject){
  stopifnot(all(colnames(mcols(query)) %in% colnames(mcols(subject))))
  m <- mcols(query)
  null <- mapply(function(.d, .n){
    if(!is.numeric(.d)){
      stop('metadata column', .n, 'is not a numeric vector')
    }
  }, m, colnames(m))
  kabsch(query, subject)
}
