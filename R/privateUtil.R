orderedGR <- function(gr=GRanges()){
  if(length(gr)>0){
    gr[order(as.character(seqnames(gr)), start(gr))]
  }else{
    gr
  }
}

#' @importFrom grDevices col2rgb rgb
invertCol <- function (col){
  n <- names(col)
  col <- col2rgb(col,alpha=FALSE)
  int_col <- abs(255 - col)
  int_col <- apply(int_col, 2, function(.ele){
    rgb(.ele[1 ], .ele[2 ], .ele[3 ], maxColorValue = 255)})
  int_col <- unlist(int_col)
  names(int_col) <- n
  int_col
}

checkSignalTransformFun <- function(signalTransformFun){
  if(length(signalTransformFun)>1||is.list(signalTransformFun)){
    stopifnot(all(vapply(signalTransformFun, is.function, FUN.VALUE = logical(1L))))
  }else{
    stopifnot(is.function(signalTransformFun))
  }
}

#' @importFrom BiocGenerics width
#' @importFrom S4Vectors mcols<-
checkGI <- function(gi, fixedBin=FALSE){
  stopifnot(is(gi, "GInteractions"))
  stopifnot('score' %in% colnames(mcols(gi)))
  if(fixedBin){
    w <- unique(width(regions(gi)))
    stopifnot('The input gi is not bin based data.'=length(w)==1)
  }
  if(any(is.na(mcols(gi)$score))) {
    warning('There are NA values in the gi score. It will be removed.')
    gi <- gi[!is.na(mcols(gi)$score)]
  }
  if(any(is.infinite(mcols(gi)$score))){
    warning('There are infinite values in the gi score.',
            ' It will be changed to .Machine$integer.max')
    mcols(gi)$score[is.infinite(mcols(gi)$score)] <- 
      sign(mcols(gi)$score[is.infinite(mcols(gi)$score)]) * 
      .Machine$integer.max
  }
  return(gi)
}

parseFeature <- function(feature.gr){
  if(!missing(feature.gr)){
    stopifnot(is(feature.gr, "GRanges"))
  }else{
    feature.gr <- GRanges()
  }
  if(length(feature.gr$col)==0) feature.gr$col <- rep("black", length(feature.gr))
  if(length(feature.gr$type)==0) {
    message('Feature type is missing. Set as gene.')
    feature.gr$type <- rep("gene", length(feature.gr))
  }
  if(length(feature.gr$cex)==0) feature.gr$cex <- rep(1, length(feature.gr))
  if(length(feature.gr$pch)==0) {
    feature.gr$pch <- rep(11, length(feature.gr))
  }
  if(length(feature.gr$size)==0) {
    if(length(feature.gr)){
      feature.gr$size <- rep(unit(0.25, "char"), length(feature.gr))
    }
  }
  stopifnot(length(feature.gr$type)==length(feature.gr))
  stopifnot(length(feature.gr$label)==length(feature.gr))
  feature.gr
}
