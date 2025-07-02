#' Extract the backbone coordinates from output of mdsPlot
#' @description
#' Extract the positions from output of mdsPlot and used as the 'targetObj'
#' for function create3dGenomicSignals
#' @param v3d_output The output of \link{mdsPlot} or
#'  \link{view3dStructure} for k=3.
#' @param n The backbone name of in the inputs.
#' @return An GRanges object with positions of x0, x1, y0, y1, z0 and z1.
#' @importFrom GenomicRanges GRanges GRangesList
#' @export
#' @examples
#' library(GenomicRanges)
#' gi_nij <- readRDS(system.file("extdata", "nij.chr6.51120000.53200000.gi.rds",
#'                   package = "geomeTriD"))
#' range_chr6 <- GRanges("chr6", IRanges(51120000, 53200000))
#' geos <- mdsPlot(gi_nij, range = range_chr6, k = 3, render = "none")
#' extractBackbonePositions(geos)
extractBackbonePositions <- function(v3d_output, n='backbone'){
  if(length(n)>1){
    obj.grs <- lapply(n, extractBackbonePositions, v3d_output=v3d_output)
    obj.grs <- obj.grs[lengths(obj.grs)>0]
    obj.grs <- unlist(GRangesList(obj.grs))
    return(obj.grs)
  }
  if(!n %in% names(v3d_output)){
    stop('v3d_output must be output of mdsPlot')
  }
  if(!'target' %in% names(v3d_output[[n]]$properties)){
    stop("v3d_output must be output of mdsPlot with k=3")
  }
  obj <- v3d_output[[n]]$properties$target
  obj <- strsplit(obj, '-')
  obj <- do.call(rbind, obj)
  mode(obj) <- 'numeric'
  obj.gr <- GRanges(v3d_output[[n]]$properties$seqn,
                    IRanges(obj[, 1], obj[, 2]))
  l <- length(v3d_output[[n]]$x)
  mc <- data.frame(x0=v3d_output[[n]]$x[-l],
                   x1=v3d_output[[n]]$x[-1],
                   y0=v3d_output[[n]]$y[-l],
                   y1=v3d_output[[n]]$y[-1],
                   z0=v3d_output[[n]]$z[-l],
                   z1=v3d_output[[n]]$z[-1])
  mcols(obj.gr) <- mc
  return(obj.gr)
}
