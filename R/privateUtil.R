orderedGR <- function(gr=GRanges()){
  if(length(gr)>0){
    gr[order(as.character(seqnames(gr)), start(gr))]
  }else{
    gr
  }
}

checkSignalTransformFun <- function(signalTransformFun){
  if(length(signalTransformFun)>1){
    stopifnot(all(vapply(signalTransformFun, is.function, FUN.VALUE = logical(1L))))
  }else{
    stopifnot(is.function(signalTransformFun))
  }
}
