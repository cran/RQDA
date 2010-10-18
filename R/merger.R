mergeCodes <- function(cid1,cid2){ ## cid1 and cid2 are two code IDs.
  mergeHelperFUN <- function(From,Exist){ ## from and exist are data frame of codings.
    if (nrow(Exist)==0){## just write to the new code if there is no coding related to that code.
      success <- dbWriteTable(.rqda$qdacon,"coding",From,row.name=FALSE,append=TRUE)
      if (!success) gmessage("Fail to write to database.")
    } else {
      Relations <- apply(Exist[c("selfirst","selend")],1,FUN=function(x) relation(x,c(From$selfirst,From$selend)))
      ## because apply convert data to an array, and Exist containts character -> x is charater rather than numeric
      Exist$Relation <- sapply(Relations,FUN=function(x) x$Relation) ## add Relation to the data frame as indicator.
      ## possible bugs: should handle exact explicitely.
      if (!any(Exist$Relation=="exact")){
        ## if they are axact, do nothing; -> if they are not exact, do something. The following lines record meta info.
        Exist$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
        Exist$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
        Exist$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
        if (all(Exist$Relation=="proximity")){ ## if there are no overlap in any kind, just write to database
            dis <- sapply(Relations,function(x) x$Distance)
            if (all(dis>0)) {
                success <- dbWriteTable(.rqda$qdacon,"coding",From,row.name=FALSE,append=TRUE)
                if (!success) gmessage("Fail to write to database.")
            } else {
                idx0 <- which(dis==0)
                index3 <- unlist(c(From[,c("selfirst","selend")], Exist[idx0,c("selfirst","selend")]))
                From["seltext"] <- paste(Exist$coding[idx0][rank(Exist$selfirst[idx0])],collapse="")
                From["selfirst"] <- min(index3)
                From["selend"] <- max(index3)
                ## DAT <- From[,c("rowid","fid","filename","index1","index2","coding"),drop=FALSE] ## write to coding
                ## delete the adjacent one.
            }
        } else { ## if not proximate, pass to else branch.
          del1 <- (Exist$Relation =="inclusion" & any(Exist$WhichMin==2,Exist$WhichMax==2))
          ## ==2 -> take care of NA. Here 2 means From according to how Relations is returned.
          del2 <- Exist$Relation =="overlap"
          ## if overlap or inclusion [Exist nested in From] -> delete codings in Exist
          del <- (del1 | del2) ## index of rows in Exist that should be deleted.
          if (any(del)){
            ## no rowid in Exist by sql of select, so add rowid to it (That is ToDat data frame).
            oldmemo <- dbGetQuery(.rqda$qdacon,sprintf("select memo from coding where rowid in (%s)",
                                                    paste(Exist$rowid[del],collapse=",",sep="")))$memo
            memo <- paste(c(oldmemo,From$memo),collapse="\n",sep="") ## merge the old memo from From
            dbGetQuery(.rqda$qdacon,sprintf("delete from coding where rowid in (%s)",
                                            paste(Exist$rowid[del],collapse=",",sep=""))) ## delete codings
            tt <-   dbGetQuery(.rqda$qdacon,sprintf("select file from source where id=='%i'", From$fid))[1,1]
            Encoding(tt) <- "UTF-8"  ## fulltext of the file
            Sel <- c(min(Exist$Start[del]), max(Exist$End[del])) ## index to get the new coding
            ## what is Sel?
            DAT <- data.frame(cid=From$cid,fid=From$fid,seltext=substr(tt,Sel[1],Sel[2]),
                              selfirst=Sel[1],selend=Sel[2],status=1,
                              owner=.rqda$owner,date=date(),memo=memo) ## The new coding to table.
            success <- dbWriteTable(.rqda$qdacon,"coding",DAT,row.name=FALSE,append=TRUE)
            if (!success) gmessage("Fail to write to database.")
          }
        }
      }
    }
  } ## end of helper function.

  Coding1 <-  dbGetQuery(.rqda$qdacon,sprintf("select * from coding where cid==%i and status=1",cid1))
  Coding2 <-  dbGetQuery(.rqda$qdacon,sprintf("select * from coding where cid==%i and status=1",cid2))
  if (any(c(nrow(Coding1),nrow(Coding2))==0)) stop("One code has empty coding.")
  if (nrow(Coding1) >= nrow(Coding2)) {
    FromDat <- Coding2
    ToDat <- Coding1
    ToDat$rowid <- RQDAQuery(sprintf("select rowid from coding where cid==%i and status=1",cid1))$rowid
    FromDat$cid <- cid1 ## so can write to directly where it is proximate
    FromCid <- cid2
  } else {
    FromDat <- Coding1
    ToDat <- Coding2
    ToDat$rowid <- RQDAQuery(sprintf("select rowid from coding where cid==%i and status=1",cid2))$rowid
    FromDat$cid <- cid2
    FromCid <- cid1
  } ## use small coding as FromDat -> speed it up.
  for (i in seq_len(nrow(FromDat))) {
    x <- FromDat[i,,drop=FALSE]
    Exist <- ToDat[ToDat$fid==x$fid,] ## subset(ToDat,subset=fid==x$fid)
    ## avoding the NOTE from docs checking.
    mergeHelperFUN(From=x,Exist=Exist)
  }
  dbGetQuery(.rqda$qdacon,sprintf("update coding set status==0 where cid=='%i'",FromCid))
  dbGetQuery(.rqda$qdacon,sprintf("update freecode set status==0 where id=='%i'",FromCid))
}
