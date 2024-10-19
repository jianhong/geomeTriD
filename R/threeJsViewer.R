#' threeJs Viewer
#' The htmlwidgets viewer for threeJs.
#' @importFrom htmlwidgets createWidget
#' @export
#' @param ... objects of threeJsGeometry.
#' @param background background of the main camera (left and right).
#' @param maxRadius max value of the controls for radius.
#' @param maxLineWidth max value of the controls for line width.
#' @param title the titles of the plot.
#' @param width,height width and height of the widgets.
#' @return A htmlwidgets widget.
#' @examples
#' library(GenomicRanges)
#' flamingo <- system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds", package = "geomeTriD")
#' x <- readRDS(flamingo[[1]])
#' ## resize to bigger value to get better init view
#' mcols(x) <- as.data.frame(mcols(x)) * 1e5
#' set.seed(1)
#' line <- threeJsGeometry(
#'   x = x$x, y = x$y, z = x$z,
#'   colors = sample(palette(), length(x), replace = TRUE),
#'   type = "line",
#'   properties = list(size = 4)
#' )
#' sphere <- x[sample.int(length(x), 100)]
#' sphere <- threeJsGeometry(
#'   x = sphere$x, y = sphere$y, z = sphere$z,
#'   colors = "red",
#'   type = "sphere",
#'   properties = list(radius = 0.08)
#' )
#' torus <- x[sample.int(length(x), 100)]
#' torus <- threeJsGeometry(
#'   x = torus$x, y = torus$y, z = torus$z,
#'   colors = "blue",
#'   type = "torus",
#'   properties = list(
#'     radius = 0.08,
#'     tube = 0.03
#'   )
#' )
#' cylinder <- x[sample.int(length(x), 100)]
#' cylinder <- threeJsGeometry(
#'   x = cylinder$x, y = cylinder$y, z = cylinder$z,
#'   colors = "green",
#'   type = "cylinder",
#'   properties = list(
#'     "height" = 0.07,
#'     "radiusTop" = 0.05,
#'     "radiusBottom" = 0.09
#'   )
#' )
#' labels <- x[sample.int(length(x), 5)]
#' fontURL <- paste0('https://raw.githubusercontent.com/mrdoob/three.js/refs/',
#'    'heads/dev/examples/fonts/helvetiker_regular.typeface.json')
#' labels <- threeJsGeometry(
#'   x = labels$x, y = labels$y, z = labels$z,
#'   colors = "black",
#'   type = "text",
#'   properties = list(
#'     "label" = "text",
#'     "font" = readLines(fontURL),
#'     "size" = .5,
#'     "depth" = .1
#'   )
#' )
#' threeJsViewer(line, sphere, torus, cylinder)
threeJsViewer <- function(...,
                          background = c(
                            "#33333388", "#FFFFFFDD",
                            "#FFFFFFDD", "#33333388"
                          ),
                          maxRadius = 1,
                          maxLineWidth = 50,
                          title = NULL,
                          width = NULL, height = NULL) {
  if(!missing(title)){
    stopifnot(is.character(title))
    title <- unname(title)
  }
  geos <- list(...)
  if (length(geos) == 1) {
    if (is.list(geos[[1]])) {
      geos <- geos[[1]]
      if (length(names(geos)) != length(geos)) {
        stop("Names are required for the input threeJsGeometries.")
      }
    }
  }
  if (any(vapply(geos, is.list, FUN.VALUE = logical(1L)))) {
    geos <- unlist(geos)
  }
  if (length(names(geos)) != length(geos)) {
    dots <- substitute(list(...))[-1]
    names <- unlist(vapply(dots, deparse, FUN.VALUE = character(1L)))
    names(geos) <- names
  }
  if (any(is.na(names(geos))) || any(names(geos) == "")) {
    stop("Can not handle the names for input threeJsGeometries.")
  }
  null <- lapply(geos, function(.ele) {
    stopifnot(
      "input must be an object of threeJsGeometry." =
        is(.ele, "threeJsGeometry")
    )
  })

  if (length(background) < 4) {
    background <- rep(background, 4)[seq.int(4)]
  }
  background <- grDevices::col2rgb(background, alpha = TRUE)
  background <- background / 255
  # pass the data and settings using 'x'
  tags <- unname(vapply(geos, function(.geo) {
    .geo$tag[1]
  }, FUN.VALUE = character(1L)))
  x <- list(
    background = list(
      r = unname(background["red", , drop = TRUE]),
      g = unname(background["green", , drop = TRUE]),
      b = unname(background["blue", , drop = TRUE]),
      alpha = unname(background["alpha", , drop = TRUE])
    ),
    maxRadius = maxRadius,
    maxLineWidth = maxLineWidth,
    sideBySide = any(vapply(geos, function(.geo) {
      .geo$side[1] == "right"
    }, FUN.VALUE = logical(1L))),
    overlay = any(vapply(geos, function(.geo) {
      .geo$layer[1] == "bottom"
    }, FUN.VALUE = logical(1L))),
    taglayers = unique(tags),
    tagWithChild = unique(tags[duplicated(tags)])
  )
  if(!missing(title)){
    x$title <- c(title, '')[c(1, 2)]
  }
  x <- c(
    x,
    lapply(geos, function(.geo) {
      # convert x, y, z to numeric point(x, y, z), point2(x, y, z)
      positions <- data.frame(.geo$x, .geo$y, .geo$z)
      positions <- as.numeric(t(positions))
      # rotation, one object can only have one rotation
      rotation <- .geo$rotation
      if(length(rotation)==0){
        rotation <- rep(0, 3)
      }else if(length(rotation)<=3){
        rotation <- c(rotation, 0, 0, 0)[seq.int(3)]
      }else{
        rotation <- rotation[seq.int(3)]
      }
      if(!all(c('x', 'y', 'z') %in% names(rotation))){
        names(rotation) <- c('x', 'y', 'z')
      }
      # convert color to rgb, max=1
      colors <- grDevices::col2rgb(.geo$colors, alpha = FALSE)
      colors <- colors / 255
      colors <- as.numeric(colors)
      c(
        list(
          type = .geo$type,
          positions = positions,
          rotation = as.list(rotation),
          colors = colors,
          tag = .geo$tag,
          side = .geo$side,
          layer = .geo$layer
        ),
        .geo$properties
      )
    })
  )
  # create the widget
  htmlwidgets::createWidget(
    "threeJsViewer", x,
    width = width, height = height,
    package = getPackageName()
  )
}

#' Shiny bindings for threeJsViewer
#'
#' Output and render functions for using threeJsViewer within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'600px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a threeJsViewer
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @return An output or render function that enables the use of the
#' threeJsViewer widget.
#' @name threeJsViewer-shiny
#' @importFrom htmlwidgets shinyWidgetOutput
#' @export
#' @examples
#' if (interactive()) {
#'   library(GenomicRanges)
#'   flamingo <- system.file("extdata", "4DNFI1UEG1HD.chr21.FLAMINGO.res.rds", package = "geomeTriD")
#'   x <- readRDS(flamingo[[1]])
#'   ## resize to bigger value to get better init view
#'   mcols(x) <- as.data.frame(mcols(x)) * 1e5
#'   line <- threeJsGeometry(
#'     x = x$x, y = x$y, z = x$z,
#'     colors = sample(palette(), length(x), replace = TRUE),
#'     type = "line",
#'     properties = list(size = 4)
#'   )
#'   library(shiny)
#'   runApp(list(
#'     ui = bootstrapPage(
#'       threejsOutput("plot")
#'     ),
#'     server = function(input, output) {
#'       output$plot <- renderthreeJsViewer({
#'         threeJsViewer(line)
#'       })
#'     }
#'   ))
#' }
threejsOutput <- function(outputId, width = "100%", height = "600px") {
  htmlwidgets::shinyWidgetOutput(
    outputId, "threeJsViewer", width, height,
    package = getPackageName()
  )
}

#' @name threeJsViewer-shiny
#' @importFrom htmlwidgets shinyRenderWidget
#' @export
renderthreeJsViewer <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, threejsOutput, env, quoted = TRUE)
}
