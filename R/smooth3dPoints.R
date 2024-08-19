#' Calculate the smoothed curve for input GRanges
#' @param obj GRanges object with mcols x, y, and z
#' @param resolusion number of points at which to evaluate the smooth curve.
#' @param ... parameters passed to \link[stats]{splinefun}
#' @importFrom S4Vectors mcols
#' @importFrom IRanges tile
#' @export
#' @return GRanges object with smoothed points of x, y, and z.
#' @examples
#' library(GenomicRanges)
#' obj <- GRanges('1', IRanges(seq.int(5)*10, width=10),
#'  x=seq.int(5), y=seq.int(5), z=seq.int(5))
#' smooth3dPoints(obj, 5)
#' 
smooth3dPoints <- function(obj, resolusion=30, ...){
  stopifnot(is.numeric(resolusion))
  stopifnot(is(obj, "GRanges"))
  stopifnot(all(c('x', 'y', 'z') %in% colnames(mcols(obj))))
  if(length(obj)==0){
    return(obj)
  }
  t <- seq_along(obj)
  resolusion <- safe_resolution(obj, n = resolusion)
  tt <- seq(1, length(obj), len = sum(resolusion) + 1)
  sdata <- lapply(colnames(mcols(obj)), function(j) {
    splinefun(t, mcols(obj)[, j, drop = TRUE], ...)(tt)
  })
  sdata <- do.call(cbind, sdata)
  sdata <- data.frame(sdata)
  colnames(sdata) <- colnames(mcols(obj))
  obj <- tile(obj, n = resolusion)
  obj <- unlist(obj)
  stopifnot("Unexpected happend." = length(obj) + 1 == nrow(sdata))
  sdata0 <- sdata[-nrow(sdata), , drop = FALSE]
  sdata1 <- sdata[-1, , drop = FALSE]
  colnames(sdata0) <- paste0(colnames(sdata), "0")
  colnames(sdata1) <- paste0(colnames(sdata), "1")
  mcols(obj) <- cbind(sdata0, sdata1)
  obj
}
