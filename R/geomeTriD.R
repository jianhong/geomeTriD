#' Interactive 3D plot of epigenetic data or single cell data
#'
#' geomeTriD (Three Dimensional Geometry Package) create interactive 3D plots
#' using the GL library with the 'three.js' 
#' visualization library (https://threejs.org) or the rgl library.
#' In addition to creating interactive 3D plots, 
#' the application also generates simplified models in 2D.
#' These 2D models provide a more straightforward visual representation,
#' making it easier to analyze and interpret the data quickly.
#' This functionality ensures that users have access to both detailed 
#' three-dimensional visualizations and more accessible two-dimensional views,
#' catering to various analytical needs.
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
