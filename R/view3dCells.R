#' Plot cell xyz data in 2d or 3d
#' @description
#' Plot cell xyz data with grid or rgl package.
#' @param cells A data.frame.
#' @param x,y,z Column names of x, y, z.
#' @param color,shape,radius The column names for color, shape, radius or the
#' value(length=1) of them.
#' @param tag The tag for controler.
#' @param renderer The renderer of the 3D plots. Could be rgl or threejs.
#' The threejs will create a htmlwidgets. If 'none' is set, a list of object
#' will be returned.
#' @param ... Not used.
#' @return A list of threeJsGeometry objects or a 
#' htmlwidget.
#' @export
#' @examples
#' cells <- readRDS(system.file('extdata', 'pbmc_small.3d.rds',
#'  package='geometrid'))
#' view3dCells(cells, x="umap_1", y="umap_2", z="umap_3",
#'  renderer='threejs')
view3dCells <- function(cells, x, y, z,
                        color='blue', shape='sphere',
                        radius=0.1, tag='cell',
                        renderer = c('rgl', 'threejs', 'none'),
                        ...){
  stopifnot(is.data.frame(cells))
  stopifnot(all(c(x, y, z) %in% colnames(cells)))
  stopifnot(is.character(tag))
  if(color[1] %in% colnames(cells)){
    color <- cells[, color[1]]
  }
  if(shape[1] %in% colnames(cells)){
    shape <- cells[, shape[1]]
  }
  if(radius[1] %in% colnames(cells)){
    radius <- cells[, radius[1]]
  }
  if(nrow(cells)==0) return(NULL)
  cells <- cbind(cells[, c(x, y, z)], shape=shape, color=color, radius=radius)
  cells <- split(cells, paste0(cells$shape, cells$radius))
  geometries <- lapply(cells, function(.ele){
    threeJsGeometry(
      x = .ele[, x],
      y = .ele[, y],
      z = .ele[, z],
      type = .ele[1, 'shape', drop=TRUE],
      colors = .ele[, 'color', drop=TRUE],
      tag=tag,
      properties = list(radius=.ele[1, 'radius', drop=TRUE])
    )})
  if(renderer=='rgl'){
    return(rglViewer(geometries))
  }else{
    if(renderer=='threejs'){
      return(threeJsViewer(geometries))
    }else{
      return(geometries)
    }
  }
}
