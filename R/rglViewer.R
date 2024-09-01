#' rgl Viewer
#' View the 3d structure by rgl.
#' @importFrom rgl open3d segments3d lines3d arrow3d points3d texts3d
#'  rgl.bringtotop bg3d spheres3d cylinder3d shade3d addNormals subdivision3d
#'  translate3d tetrahedron3d octahedron3d icosahedron3d dodecahedron3d
#'  cube3d scale3d polygon3d
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
  doMapply <- function(.ele, FUN, scale_factor, subdivision=0) {
    mapply(
      function(.color, .tag, .x, .y, .z) {
        shade3d(scale3d(
          translate3d(
            if(subdivision==0){
              FUN(
                col = .color,
                tag = .tag
              )
            }else{
              subdivision3d(
                FUN(
                  col = .color,
                  tag = .tag
                ),
                depth = subdivision
              )
            }
            ,
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
      box = {
        doMapply(.ele, cube3d, list(
          x = .ele$properties$width,
          y = .ele$properties$height,
          z = .ele$properties$depth
        ))
      },
      capsule = {
        doMapply(.ele, cube3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$height
        ), subdivision=3)
      },
      cone = {
        c3 <- mapply(function(.x, .y, .z, .h, .r, .c){
          center <- matrix(
            c(.x , .x,
              .y, .y,
              .z+.h/2, .z-.h/2),
            nrow = 2
          )
          colnames(center) <- c('x', 'y', 'z')
          cylinder3d(
            center = center,
            radius = c(0, .r),
            color = .c,
            sides = 36
          )
        }, .ele$x, .ele$y, .ele$z, 
        .ele$properties$height,
        .ele$properties$radius,
        .ele$colors,
        SIMPLIFY = FALSE)
        lapply(c3, function(.c3){
          shade3d(addNormals(.c3))
        })
      },
      circle = {
        c3 <- mapply(function(.x, .y, .z, .r, .start, .end, .c, .tag){
          theta <- c(seq(.start, .end, length.out = 36))
          polygon3d(x=c(.x, .x+.r*sin(theta), .x),
                    y=c(.y, .y+.r*cos(theta), .y),
                    z=rep(.z, 38),
                    fill = TRUE, plot = TRUE,
                    col = .c,
                    tag = .tag)
        },.ele$x, .ele$y, .ele$z,
        .ele$properties$radius,
        .ele$properties$thetaStart,
        .ele$properties$thetaLength,
        .ele$colors, .ele$tag,
        SIMPLIFY = FALSE)
      },
      cylinder = {
        c3 <- mapply(function(.x, .y, .z, .h, .t, .b, .c){
          center <- matrix(
            c(.x , .x,
              .y, .y,
              .z+.h/2, .z-.h/2),
            nrow = 2
          )
          colnames(center) <- c('x', 'y', 'z')
          cylinder3d(
            center = center,
            radius = c(.t, .b),
            color = .c,
            sides = 36
          )
        }, .ele$x, .ele$y, .ele$z, 
        .ele$properties$height,
        .ele$properties$radiusTop,
        .ele$properties$radiusBottom,
        .ele$colors,
        SIMPLIFY = FALSE)
        lapply(c3, function(.c3){
          shade3d(addNormals(.c3))
        })
      },
      dodecahedron = {
        doMapply(.ele, dodecahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
      },
      json = {
        message('not supported!')
      },
      icosahedron = {
        doMapply(.ele, icosahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
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
      octahedron = {
        doMapply(.ele, octahedron3d, list(
          x = .ele$properties$radius,
          y = .ele$properties$radius,
          z = .ele$properties$radius
        ))
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
        texts3d(x=.ele$x, y=.ele$y, z=.ele$z,
          texts = .ele$properties$label,
          col = .ele$colors,
          tag = .ele$tag,
          pos = .ele$properties$pos
        )
      },
      text = {
        texts3d(x=.ele$x, y=.ele$y, z=.ele$z,
          texts = .ele$properties$label,
          col = .ele$colors,
          tag = .ele$tag,
          pos = .ele$properties$pos
        )
      },
      torus = {
        theta <- c(seq(0, 2*pi, length.out = 150))
        c3 <- mapply(function(.x, .y, .z, .h, .r, .c, .tag){
          center <- cbind(
            .x+.r*sin(theta),
            .y+.r*cos(theta),
            rep(.z, 150)
          )
          colnames(center) <- c('x', 'y', 'z')
          cylinder3d(
            center = center,
            radius = .r/2,
            color = .c,
            closed = 1,
            tag = .tag,
            sides = 36
          )
        }, .ele$x, .ele$y, .ele$z, 
        .ele$properties$tube,
        .ele$properties$radius,
        .ele$colors, .ele$tag,
        SIMPLIFY = FALSE)
        lapply(c3, function(.c3){
          shade3d(addNormals(.c3))
        })
      }
    )
  })
  rgl.bringtotop()
}
