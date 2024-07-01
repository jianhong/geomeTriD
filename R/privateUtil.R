orderedGR <- function(gr=GRanges()){
  if(length(gr)>0){
    gr[order(as.character(seqnames(gr)), start(gr))]
  }else{
    gr
  }
}
