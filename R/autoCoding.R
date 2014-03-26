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

codingBySearchOneFile <- function(pattern, fid, cid, seperator, ...) {
  ## auto coding: when seperator is \n, each paragraph is a analysis unit
  ## by providing approperiate seperator, it allows flexible control on the unit of autocoding
    txt <- RQDAQuery(sprintf("select file from source where status=1 and id=%s",fid))$file
    Encoding(txt) <- "UTF-8"
    pidx <- gregexpr(sprintf("(%s){1,}", seperator),txt)
    idx1 <- c(0,pidx[[1]]+attr(pidx[[1]],"match.length")-1)
    idx2 <- c(pidx[[1]]-1,nchar(txt))
    sidx <- gregexpr(pattern,txt, ...)[[1]]
    if (length(sidx) > 1 || (sidx != -1)) {
        residx <- unique(findInterval(sidx,sort(c(idx1,idx2))))
        idx <- (residx + 1)/2
        anstxt <- strsplit(txt,sprintf("(%s){1,}", seperator))[[1]][idx]
        ## create data frame
        df <- data.frame(cbind(cid     = as.integer(cid),
                               fid      = as.integer(fid),
                               seltext  = anstxt,
                               selfirst = idx1[idx],
                               selend   = idx2[idx],
                               status   = as.integer(1),
                               owner    = Sys.info()["user"],
                               date     = date(),
                               memo     = sprintf("auto coding by searching %s", pattern)),
                         stringsAsFactors=FALSE
                         )
        dfprev <- RQDAQuery("select cid, fid, selfirst, selend from coding where status=1")
        if (any(duplicated(dfprev))) stop("Duplicated coding records, clean it manually before autocoding.")
        idx <- apply(df[,c("cid","fid","selfirst","selend")],1,function(x) {
            any(duplicated(rbind(x,dfprev)))
        })
        df <- df[!idx,] ## non-duplicated codings only
        if (nrow(df)>0) {
            dbWriteTable(.rqda$qdacon, "coding", df, row.names = FALSE, append = TRUE)
        }
    }
}

codingBySearch <- function(pattern, fid = getFileIds(), cid, seperator="\n", ...) {
    if (length(fid)> 0) {
        for (i in fid) {
            codingBySearchOneFile(pattern, fid=i, cid=cid, seperator=seperator, ...)
        }
    }
}
