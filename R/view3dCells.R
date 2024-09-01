#' Plot cell xyz data in 2d or 3d
#' @description
#' Plot cell xyz data with grid or rgl package.
#' @param cells A data.frame.
#' @param x,y,z Column names of x, y, z.
#' @param color,shape,radius The column names for color, shape, radius or the
#' value(length=1) of them.
#' @param colorFun The function to map values into colors.
#' @param tag The tag for controler.
#' @param renderer The renderer of the 3D plots. Could be rgl or threejs.
#' The threejs will create a htmlwidgets. If 'none' is set, a list of object
#' will be returned.
#' @param ... Not used.
#' @importFrom grDevices col2rgb colors rgb
#' @return A list of threeJsGeometry objects or a
#' htmlwidget.
#' @export
#' @examples
#' cells <- readRDS(system.file("extdata", "pbmc_small.3d.rds",
#'   package = "geomeTriD"
#' ))
#' view3dCells(cells,
#'   x = "umap_1", y = "umap_2", z = "umap_3",
#'   color = "nCount_RNA",
#'   renderer = "threejs"
#' )
view3dCells <- function(cells, x, y, z,
                        color = "blue",
                        colorFun = function(x, pal = seq.int(8)) {
                          if (is.character(x)) x <- as.numeric(factor(x))
                          limits <- range(x)
                          pal[findInterval(x, seq(limits[1], limits[2],
                            length.out = length(pal) + 1
                          ),
                          all.inside = TRUE
                          )]
                        },
                        shape = "sphere",
                        radius = 0.1, tag = "cell",
                        renderer = c("rgl", "threejs", "none"),
                        ...) {
  stopifnot(is.data.frame(cells))
  stopifnot(all(c(x, y, z) %in% colnames(cells)))
  stopifnot(is.character(tag))
  renderer <- match.arg(renderer)
  if (color[1] %in% colnames(cells)) {
    color <- cells[, color[1], drop = TRUE]
    if (is.character(color)) {
      ## is color
      if (all(color %in% colors() | grepl("^#.{3,6}$", color))) {
        color <- col2rgb(color)
        color <- apply(color, 2, function(.ele) {
          rgb(.ele[1], .ele[2], .ele[3], maxColorValue = 255)
        })
      } else {
        color <- colorFun(color)
      }
    } else if (is.numeric(color) && (
      any(color != round(color)) ||
        any(color > 8))) {
      ## is number
      color <- colorFun(color)
    }
  }
  if (shape[1] %in% colnames(cells)) {
    shape <- cells[, shape[1], drop = TRUE]
  }
  if (radius[1] %in% colnames(cells)) {
    radius <- cells[, radius[1], drop = TRUE]
  }
  if (nrow(cells) == 0) {
    return(NULL)
  }
  cells <- cbind(cells[, c(x, y, z), drop = FALSE], shape = shape, color = color, radius = radius)
  cells <- split(cells, paste0(cells$shape, cells$radius))
  geometries <- lapply(cells, function(.ele) {
    threeJsGeometry(
      x = .ele[, x, drop = TRUE],
      y = .ele[, y, drop = TRUE],
      z = .ele[, z, drop = TRUE],
      type = .ele[1, "shape", drop = TRUE],
      colors = .ele[, "color", drop = TRUE],
      tag = tag,
      properties = list(radius = .ele[1, "radius", drop = TRUE])
    )
  })
  if (renderer == "rgl") {
    return(rglViewer(geometries))
  } else {
    if (renderer == "threejs") {
      return(threeJsViewer(geometries))
    } else {
      return(geometries)
    }
  }
}
