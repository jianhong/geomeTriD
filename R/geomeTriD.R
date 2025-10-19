#' Interactive 3D plot of epigenetic data or single cell data
#'
#' The geomeTriD (Three-Dimensional Geometry) Package provides interactive 3D
#' visualization of chromatin structures using the WebGL-based three.js or 
#' the rgl rendering library. It is designed to identify and explore spatial
#' chromatin patterns within genomic regions. The package generates dynamic 
#' 3D plots and HTML widgets that integrate seamlessly with Shiny applications,
#' enabling researchers to visualize chromatin organization, detect spatial
#' features, and compare structural dynamics across different conditions and
#' data types.
#' @keywords package
#' @examples
#' if(interactive()){
#'   ## quick start from a simple data
#'   library(geomeTriD)
#'   set.seed(123)
#'   obj <- GRanges("1", IRanges(seq.int(10), width = 1),
#'                   x = sample.int(10, 10),
#'                   y = sample.int(10, 10),
#'                   z = sample.int(10, 10)
#'                 )
#'   feature.gr <- GRanges("1", IRanges(c(3, 7), width = 3),
#'                         label = c("gene1", "gene2"),
#'                         col = c("red", "blue"),
#'                         type = "gene"
#'   )
#'   view3dStructure(obj, feature.gr,
#'                   renderer = "threejs",
#'                   coor_mark_interval = 5, coor_tick_unit = 2
#'   )
#' }
#' 
"_PACKAGE"
