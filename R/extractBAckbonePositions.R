#' Extract the backbone coordinates from output of mdsPlot
#' @description
#' Extract the positions from output of mdsPlot and used as the 'targetObj'
#' for function create3dGenomicSignals
#' @param v3d_output The output of \link{mdsPlot} or
#'  \link{view3dStructure} for k=3.
#' @return An GRanges object with positions of x0, x1, y0, y1, z0 and z1.
#' @importFrom GenomicRanges GRanges
#' @export
#' @examples
#' library(GenomicRanges)
#' gi_nij <- readRDS(system.file("extdata", "nij.chr6.51120000.53200000.gi.rds",
#'                   package = "geomeTriD"))
#' range_chr6 <- GRanges("chr6", IRanges(51120000, 53200000))
#' geos <- mdsPlot(gi_nij, range = range_chr6, k = 3, render = "none")
#' extractBackbonePositions(geos)
extractBackbonePositions <- function(v3d_output){
  if(!'backbone' %in% names(v3d_output)){
    stop('v3d_output must be output of mdsPlot')
  }
  if(!'target' %in% names(v3d_output$backbone$properties)){
    stop("v3d_output must be output of mdsPlot with k=3")
  }
  obj <- v3d_output$backbone$properties$target
  obj <- strsplit(obj, '-')
  obj <- do.call(rbind, obj)
  mode(obj) <- 'numeric'
  obj.gr <- GRanges(v3d_output$backbone$properties$seqn,
                    IRanges(obj[, 1], obj[, 2]))
  l <- length(v3d_output$backbone$x)
  mc <- data.frame(x0=v3d_output$backbone$x[-l],
                   x1=v3d_output$backbone$x[-1],
                   y0=v3d_output$backbone$y[-l],
                   y1=v3d_output$backbone$y[-1],
                   z0=v3d_output$backbone$z[-l],
                   z1=v3d_output$backbone$z[-1])
  mcols(obj.gr) <- mc
  return(obj.gr)
}
