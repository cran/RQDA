mergeCodes <- function(cid1,cid2){
  mergeHelperFUN <- function(From,Exist){
    if (nrow(Exist)==0){
      success <- dbWriteTable(.rqda$qdacon,"coding",From,row.name=FALSE,append=TRUE)
      if (!success) gmessage("Fail to write to database.")
    } else {
      Relations <- apply(Exist,1,FUN=function(x) relation(x[c("selfirst","selend")],c(From$selfirst,From$selend)))
      Exist$Relation <- sapply(Relations,FUN=function(x) x$Relation)
      if (!any(Exist$Relation=="exact")){
        ## if they are axact, do nothing; -> if they are not exact, do something.
        Exist$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
        Exist$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
        Exist$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
        if (all(Exist$Relation=="proximity")){
          success <- dbWriteTable(.rqda$qdacon,"coding",From,row.name=FALSE,append=TRUE)
          if (!success) gmessage("Fail to write to database.")
          ## if there are no overlap in any kind, just write to database; otherwise, pass to else{}.
        } else {
          del1 <- (Exist$Relation =="inclusion" & any(Exist$WhichMin==2,Exist$WhichMax==2))
          ## if overlap or inclusion  old nested in new]
          ## then the original coding should be deleted; then write the new coding to table
          del2 <- Exist$Relation =="overlap"
          del <- (del1 | del2)
          if (any(del)){
            Sel <- c(min(Exist$Start[del]), max(Exist$End[del]))
            memo <- dbGetQuery(.rqda$qdacon,sprintf("select memo from coding where rowid in (%s)",
                                                    paste(Exist$rowid[del],collapse=",",sep="")))$memo
            memo <- paste(c(memo,From$memo),collapse="\n",sep="") ## merge the memo from From
            dbGetQuery(.rqda$qdacon,sprintf("delete from coding where rowid in (%s)",
                                            paste(Exist$rowid[del],collapse=",",sep="")))
            ## get the fulltext of the file tt
            tt <-   dbGetQuery(.rqda$qdacon,sprintf("select file from source where fid=='%i'", From$fid))
            Encoding(tt) <- "UTF-8"
            DAT <- data.frame(cid=From$cid,fid=From$fid,seltext=substr(tt,Sel[1],Sel[2]),
                              selfirst=Sel[1],selend=Sel[2],status=1,
                              owner=.rqda$owner,date=date(),memo=memo)
            success <- dbWriteTable(.rqda$qdacon,"coding",DAT,row.name=FALSE,append=TRUE)
            if (!success) gmessage("Fail to write to database.")
          }
        }
      }
    }
  }

  Coding1 <-  dbGetQuery(.rqda$qdacon,sprintf("select * from coding where cid==%i and status=1",cid1))
  Coding1$selfirst <- as.numeric(Coding1$selfirst)
  Coding1$selend <- as.numeric(Coding1$selend)
  Coding2 <-  dbGetQuery(.rqda$qdacon,sprintf("select * from coding where cid==%i and status=1",cid2))
  Coding2$selfirst <- as.numeric(Coding2$selfirst)
  Coding2$selend <- as.numeric(Coding2$selend)
  if (any(c(nrow(Coding1),nrow(Coding2))==0)) stop("One code has empty coding.")
  if (nrow(Coding1) >= nrow(Coding2)) {
    FromDat <- Coding2
    ToDat <- Coding1
    FromDat$cid <- cid1
    FromCid <- cid1
  } else {
    FromDat <- Coding1
    ToDat <- Coding2
    FromDat$cid <- cid2
    FromCid <- cid2
  }
  for (i in seq_len(nrow(FromDat))) {
    x <- FromDat[i,,drop=FALSE]
    Exist <- subset(ToDat,subset=fid==x$fid)
    mergeHelperFUN(From=x,Exist=Exist)
  }
  ## dbGetQuery(.rqda$qdacon,sprintf("delete from coding where cid=='%i'",From))
  dbGetQuery(.rqda$qdacon,sprintf("update coding set status==0 where cid=='%i'",FromCid))
  dbGetQuery(.rqda$qdacon,sprintf("update freecode set status==0 where id=='%i'",FromCid))
  ## delete the FromDat
}
