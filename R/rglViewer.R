#' rgl Viewer
#' View the 3d structure by rgl.
#' @importFrom rgl open3d segments3d lines3d arrow3d points3d text3d
#'  rgl.bringtotop bg3d spheres3d cylinder3d shade3d addNormals subdivision3d
#'  translate3d tetrahedron3d octahedron3d icosahedron3d dodecahedron3d
#'  cube3d scale3d
#' @export
#' @param ... objects of threeJsGeometry.
#' @param background background of the main camera.
#' @return MULL
#' @examples
#' obj <- readRDS(system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds",
#'   package = "geomeTriD"
#' ))
#' feature.gr <- readRDS(system.file("extdata", "4DNFI1UEG1HD.feature.gr.rds",
#'   package = "geomeTriD"
#' ))
#' tjg <- view3dStructure(obj,
#'   k = 3, feature.gr = feature.gr, renderer = "none",
#'   length.arrow = grid::unit(0.000006, "native")
#' )
#' rglViewer(tjg)
rglViewer <- function(..., background = "gray") {
  geos <- list(...)
  if (length(geos) == 1) {
    if (is.list(geos[[1]])) {
      geos <- geos[[1]]
      if (length(names(geos)) != length(geos)) {
        stop("Names are required for the input threeJsGeometries.")
      }
    }
  } else {
    dots <- substitute(list(...))[-1]
    names <- unlist(vapply(dots, deparse, FUN.VALUE = character(1L)))
    names(geos) <- names
  }

  null <- lapply(geos, function(.ele) {
    stopifnot(
      "input must be an object of threeJsGeometry." =
        is(.ele, "threeJsGeometry")
    )
  })

  open3d()
  bg3d(color = background)
  doMapply <- function(.ele, FUN, scale_factor) {
    mapply(
      function(.color, .tag, .x, .y, .z) {
        shade3d(scale3d(
          translate3d(
            FUN(
              col = .color,
              tag = .tag
            ),
            x = .x,
            y = .y,
            z = .z
          ),
          x = scale_factor$x,
          y = scale_factor$y,
          z = scale_factor$z
        ))
      }, .ele$colors, .ele$tag,
      .ele$x, .ele$y, .ele$z
    )
  }
  null <- mapply(geos, names(geos), FUN = function(.ele, .name) {
    switch(.ele$type,
      arrow = {
        arrow3d(c(.ele$x[1], .ele$y[1], .ele$z[1]),
          c(.ele$x[2], .ele$y[2], .ele$z[2]),
          type = "rotation",
          id = .name,
          col = .ele$colors,
          fill = .ele$colors,
          tag = .ele$tag
        )
      },
      line = {
        lines3d(
          x = .ele$x,
          y = .ele$y,
          z = .ele$z,
          lwd = .ele$properties$size,
          col = .ele$colors,
          alpha = ifelse(length(.ele$properties$alpha) == 1,
            .ele$properties$alpha, 1
          ),
          tag = .ele$tag
        )
      },
      segment = {
        segments3d(
          x = .ele$x,
          y = .ele$y,
          z = .ele$z,
          col = .ele$colors,
          lwd = .ele$properties$size,
          alpha = ifelse(length(.ele$properties$alpha) == 1,
            .ele$properties$alpha, 1
          ),
          tag = .ele$tag
        )
      },
      box = {
        doMapply(.ele, cube3d, list(
          x = .ele$properties$width,
          y = .ele$properties$height,
          z = .ele$properties$depth
        ))
      },
      capsule = {

      },
      cone = {

      },
      cylinder = {
        c3 <- cylinder3d(
          center = cbind(
            x = .ele$x,
            y = .ele$y,
            z = .ele$z
          ),
          radius = .ele$properties$radiusTop
        )
        shade3d(addNormals(subdivision3d(c3, depth = 2)))
      },
      dodecahedron = {
        doMapply(.ele, dodecahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
      },
      icosahedron = {
        doMapply(.ele, icosahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
      },
      octahedron = {
        doMapply(.ele, octahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
      },
      sphere = {
        spheres3d(.ele$x, .ele$y, .ele$z,
          radius = .ele$properties$radius,
          tag = .ele$tag,
          col = .ele$colors
        )
      },
      tetrahedron = {
        doMapply(.ele, tetrahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
      },
      label = {
        text3d(.ele$x, .ele$y, .ele$z,
          texts = .ele$properties$label,
          id = .ele$properties$label,
          col = .ele$colors,
          tag = .ele$tag,
          pos = .ele$properties$pos
        )
      },
      text = {
        text3d(.ele$x, .ele$y, .ele$z,
          texts = .ele$properties$label,
          id = .ele$properties$label,
          col = .ele$colors,
          tag = .ele$tag,
          pos = .ele$properties$pos
        )
      },
      torus = {

      }
    )
  })
  rgl.bringtotop()
}
