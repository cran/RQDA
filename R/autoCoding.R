AutoCoding <- function(KeyWord,expansion=6){
  Files <- SearchFiles(paste("%",KeyWord,"%",collapse=""),content=TRUE)
  AnsIndex <- gregexpr(KeyWord,Files$file)
  AnsIndex2 <- lapply(AnsIndex, FUN=function(x) {
    begin <- x-expansion
    begin[begin<0]<-0
    data.frame(begin=begin,end=x+attr(x,"match.length"))
  })
  ## if any index > nchar(Files$file), set to nchar(Files$file)
  ## for each file, simplify the coding index, so erase the overlapping codings or proximity with distance=0
}
