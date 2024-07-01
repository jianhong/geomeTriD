#' rgl Viewer
#' View the 3d structure by rgl.
#' @importFrom rgl open3d segments3d lines3d arrow3d points3d text3d
#'  rgl.bringtotop bg3d
#' @export
#' @param ... objects of threeJsGeometry.
#' @param background background of the main camera.
#' @return MULL
#' @examples
#' # example code

rglViewer <- function(..., background = '#00000088') {
  geos <- list(...)
  if(length(geos)==1){
    if(is.list(geos[[1]])){
      geos <- geos[[1]]
      if(length(names(geos))!=length(geos)){
        stop('Names are required for the input threeJsGeometries.')
      }
    }
  }else{
    dots <- substitute(list(...))[-1]
    names <- unlist(vapply(dots, deparse, FUN.VALUE = character(1L)))
    names(geos) <- names
  }
  
  null <- lapply(geos, function(.ele){
    stopifnot('input must be an object of threeJsGeometry.'=
                is(.ele, 'threeJsGeometry'))
  })

  open3d()
  bg3d(color=background)
  null <- mapply(geos, names(geos), FUN=function(.ele, .name){
    switch(.ele$type,
            arrow = {
              arrow3d(c(.ele$x[1], .ele$y[1], .ele$z[1]),
                      c(.ele$x[2], .ele$y[2], .ele$z[2]),
                      type='rotation',
                      id=.name,
                      col=.ele$colors,
                      fill=.ele$colors,
                      tag=.ele$tag)
            },
            line={
              lines3d(x=.ele$x,
                      y=.ele$y,
                      z=.ele$z,
                      lwd=.ele$properties$size,
                      col=.ele$colors,
                      alpha=ifelse(length(.ele$properties$alpha)==1,
                                   .ele$properties$alpha, 1),
                      tag=.ele$tag)
            },
            segment={
              segments3d(
                x=.ele$x,
                y=.ele$y,
                z=.ele$z,
                col=.ele$colors,
                lwd=.ele$properties$size,
                tag=.ele$tag
              )
            },
            box={
              
            },
            capsule={
              
            },
            cone={
              
            },
            cylinder={
              
            },
            dodecahedron={
              
            },
            icosahedron={
              
            },
            octahedron={
              
            },
            sphere={
              
            },
            tetrahedron={
              
            },
            label={
              text3d(.ele$x, .ele$y, .ele$z,
                     texts = .ele$properties$label,
                     id = .ele$properties$label,
                     col= .ele$colors,
                     tag = .ele$tag,
                     pos = .ele$properties$pos)
            },
            text={
              text3d(.ele$x, .ele$y, .ele$z,
                     texts = .ele$properties$label,
                     id = .ele$properties$label,
                     col= .ele$colors,
                     tag = .ele$tag,
                     pos = .ele$properties$pos)
            },
            torus={
              
            })
  })
  rgl.bringtotop()
}
