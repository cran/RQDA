# J-P Mueller, SSP/ UNIL, jean-pierre.mueller@unil.ch
# version 1.1 du 13 mars 2009
# distributed under the terms of the GNU General Public License Version 2, June 1991.

RQDA2tm <- function(Code,language="french"){
  require("tm", quietly = TRUE)
  retrieval <- NULL
  currentCode <- Code
  if (length(currentCode)!=0)
    {		
      Encoding(currentCode) <- "UTF-8"
      currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name== '%s' ",currentCode))[1,1]
      ## reliable is more important   
      if(!is.null(currentCid))
        {
          retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i",as.numeric(currentCid)))
          if (nrow(retrieval)!=0) 
  				{
                                  retrieval <-  retrieval[order( retrieval$fid),]
                                  fid <- unique(retrieval$fid)
                                  retrieval$fname <-""
                                  for (i in fid)
  					{
                                          FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status==1 and id==%i",i))[['name']]
                                          tryCatch(Encoding(FileName) <- "UTF-8",error=function(e){})
                                          retrieval$fname[retrieval$fid==i] <- FileName
  					}
                                  Encoding(retrieval$seltext) <-  Encoding(retrieval$fname) <- "UTF-8"
				}  
        }
    }  
  retrived <- Corpus(VectorSource(retrieval$seltext), readerControl = list( language = language))
		retrieval$seltext <- NULL
  retrived <- appendMeta(retrived, dmeta = retrieval)
  return(retrived)
}
