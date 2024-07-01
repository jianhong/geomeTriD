# a class for either numeric or character
setClassUnion("maybeColor", c("numeric", "character"))

#' Class \code{"threeJsGeometry"}
#' @description An object of class \code{"threeJsGeometry"} 
#'              represents `three.js` geometry.
#' @aliases threeJsGeometry
#' @rdname threeJsGeometry-class
#' @slot x,y,z \code{"numeric"}, specify the x, y, and z coordinates.
#' @slot colors \code{"character"}, the colors for each geometry.
#' @slot type \code{"charater"}, the type of the geometry. Available types are
#' 'arrow', 'box', 'capsule', 'cone', 'cylinder', 'dodecahedron', 'line', 
#' 'label',
#' 'icosahedron', 'octahedron', 'segment', 'sphere', 'tetrahedron', 'text',
#'  and 'torus'.
#' @slot tag \code{'character'}, the tag used to group geometries.
#' @slot properties A \code{"list"}, the properties to control the geometry.
#' @import methods
#' @exportClass threeJsGeometry
#' @examples
#' tjg <- threeJsGeometry()
#' 
setClass("threeJsGeometry",
         representation = representation(
           x = "numeric",
           y = "numeric",
           z = "numeric",
           colors = 'maybeColor',
           type = 'character',
           properties = 'list',
           tag = 'character'
         ),
         prototype = prototype(
           x = 0,
           y = 0,
           z = 0,
           colors = 'black',
           type = 'sphere',
           tag = 'sphere',
           properties = list(radius=0.05)
         ),
         validity=function(object){
           if(length(object@x)!=length(object@y) ||
              length(object@x)!=length(object@z)){
             return('x, y, z must keep same length.')
           }
           if(!object@type %in% c('arrow', 'box', 'capsule', 'cone', 'cylinder',
                           'dodecahedron', 'icosahedron', 'line', 'label',
                           'octahedron',
                           'segment', 'sphere',
                           'tetrahedron', 'text', 'torus' )){
             return("type only support 'arrow',
              'box', 'capsule', 'cone', 'cylinder',
              'dodecahedron', 'icosahedron', 'line', 'label', 'octahedron',
              'segment', 'sphere', 'tetrahedron', 'text', and 'torus'.")
           }
           switch(object@type,
                  arrow = {
                    if(!all(c("size", "headLength", 'headWidth') %in%
                            names(object@properties))){
                      return("Property size, headLength, and headWidth are
                             required for arrow")
                    }
                  },
                  line={
                    if(length(object@properties$size)==0){
                      return("Property size is required for line.")
                    }
                  },
                  segment={
                    if(length(object@properties$size)==0){
                      return("Property size is required for segment.")
                    }
                  },
                  box={
                    if(!all(c('width', 'height', 'depth') 
                            %in% names(object@properties))){
                      return("Properties width, height and depth are
                             required for box.")
                    }
                  },
                  capsule={
                    if(!all(c('height', 'radius') 
                            %in% names(object@properties))){
                      return("Properties height, and radius are
                             required for capsule.")
                    }
                  },
                  cone={
                    if(!all(c('height', 'radius') 
                            %in% names(object@properties))){
                      return("Properties height, and radius are
                             required for cone.")
                    }
                  },
                  cylinder={
                    if(!all(c('height', 'radiusTop', 'radiusBottom') 
                            %in% names(object@properties))){
                      return("Properties height, radiusTop, and radiusBottom are
                             required for capsule.")
                    }
                  },
                  dodecahedron={
                    if(!'radius' %in% names(object@properties)){
                      return("Property radius is
                             required for dodecahedron")
                    }
                  },
                  icosahedron={
                    if(!'radius' %in% names(object@properties)){
                      return("Property radius is
                             required for icosahedron")
                    }
                  },
                  label={
                    if(!all(c('label', 'size') %in%
                            names(object@properties))){
                      return("Property label, and size are
                             required for text.")
                    }
                  },
                  octahedron={
                    if(!'radius' %in% names(object@properties)){
                      return("Property radius is
                             required for octahedron")
                    }
                  },
                  sphere={
                    if(!'radius' %in% names(object@properties)){
                      return("Property radius is
                             required for sphere")
                    }
                  },
                  tetrahedron={
                    if(!'radius' %in% names(object@properties)){
                      return("Property radius is
                             required for tetrahedron")
                    }
                  },
                  text={
                    if(!all(c('label', 'font', 'size', 'depth') %in%
                            names(object@properties))){
                      return("Property label, font, size, and depth are
                             required for text.")
                    }
                    for(i in c('label', 'font', 'size', 'depth')){
                      if(length(object@properties[[i]])!=1){
                        return("The length of property ", i, " must be 1.")
                      }
                    }
                  },
                  torus={
                    if(!all(c('tube', 'radius') 
                            %in% names(object@properties))){
                      return("Properties tube, and radius are
                             required for torus.")
                    }
                  })
           return(TRUE)
         })

#' @rdname threeJsGeometry-class
#' @param \dots Each argument in \dots becomes an slot in the new threeJsGeometry.
#' @export

threeJsGeometry <- function(...){
  new("threeJsGeometry", ...)
}

#' Method $
#' @rdname threeJsGeometry-class
#' @param x an object of threeJsGeometry
#' @param name slot name of threeJsGeometry
#' @exportMethod $
#' @aliases $,threeJsGeometry-method
setMethod("$", "threeJsGeometry", function(x, name) slot(x, name))
#' Method $<-
#' @rdname threeJsGeometry-class
#' @param value value to be assigned
#' @exportMethod $<-
#' @aliases $<-,threeJsGeometry-method
setReplaceMethod("$", "threeJsGeometry", 
                 function(x, name, value){
                   slot(x, name, check = TRUE) <- value
                   x
                 })
#' Method show
#' @rdname threeJsGeometry-class
#' @param object an object of threeJsGeometry
setMethod("show", "threeJsGeometry", function(object){
  message('An threeJsGeometry object of type ', object@type, ' at coordinates:')
  print(head(cbind(x=object$x, y=object$y, z=object$z)))
  message('with properties:', names(object$properties))
  
})