#' Calculate the smoothed curve for input GRanges
#' @description
#' This function will do smooth for given resolution (tile) for inputs and
#' it is important step to prepare the inputs for \link{create3dGenomicSignals}
#' and \link{view3dStructure}.
#' @param obj GRanges object with mcols x, y, and z
#' @param resolution number of points at which to evaluate the smooth curve.
#' @param ... parameters passed to \link[stats]{splinefun}
#' @importFrom S4Vectors mcols
#' @importFrom IRanges tile
#' @export
#' @return GRanges object with smoothed points of x0, y0, z0,
#' x1, y1, and z1.
#' @examples
#' library(GenomicRanges)
#' obj <- GRanges('1', IRanges(seq.int(5)*10, width=10),
#'  x=seq.int(5), y=seq.int(5), z=seq.int(5))
#' smooth3dPoints(obj, 5)
#' 
smooth3dPoints <- function(obj, resolution=30, ...){
  stopifnot(is.numeric(resolution))
  stopifnot(is(obj, "GRanges"))
  stopifnot(all(c('x', 'y', 'z') %in% colnames(mcols(obj))))
  if(length(obj)==0){
    return(obj)
  }
  t <- seq_along(obj)
  resolution <- safe_resolution(obj, n = resolution)
  tt <- seq(1, length(obj), len = sum(resolution) + 1)
  sdata <- lapply(colnames(mcols(obj)), function(j) {
    splinefun(t, mcols(obj)[, j, drop = TRUE], ...)(tt)
  })
  sdata <- do.call(cbind, sdata)
  sdata <- data.frame(sdata)
  colnames(sdata) <- colnames(mcols(obj))
  obj <- tile(obj, n = resolution)
  obj <- unlist(obj)
  stopifnot("Unexpected happend." = length(obj) + 1 == nrow(sdata))
  sdata0 <- sdata[-nrow(sdata), , drop = FALSE]
  sdata1 <- sdata[-1, , drop = FALSE]
  colnames(sdata0) <- paste0(colnames(sdata), "0")
  colnames(sdata1) <- paste0(colnames(sdata), "1")
  mcols(obj) <- cbind(sdata0, sdata1)
  obj
}
