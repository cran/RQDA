ImportFile <- function(path,encoding=.rqda$encoding,con=.rqda$qdacon,...){
  ## import a file into a DBI connection _con_.
  Fname <- gsub("\\.[[:alpha:]]*$","",basename(path))## Fname is in locale Encoding Now.
  FnameUTF8 <- iconv(Fname,to="UTF-8")
  ## remove the suffix such as .txt
  if ( Fname!="" ) {
    file_con <- file(path,open="r")
    if (isTRUE(.rqda$BOM)) seek(file_con,3)
    content <- readLines(file_con,warn=FALSE,encoding=encoding)
    close(file_con)
    content <- paste(content,collapse="\n")
    content <- enc(content,encoding=Encoding(content))
    if (Encoding(content)!="UTF-8"){
      content <- iconv(content,to="UTF-8") ## UTF-8 file content
    }
    maxid <- dbGetQuery(con,"select max(id) from source")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
    ## check if the content should be written into con.
    if (nextid==1) {
      write <- TRUE
      ## if this is the first file, no need to worry about the duplication issue.
    } else {
      if (nrow(dbGetQuery(con,sprintf("select name from source where name=='%s'",FnameUTF8)))==0) {
        ## no duplication file exists, then write.
        write <- TRUE
      } else {
        gmessage("A file withe the same name exists in the database!")
      }
    }
    if (write ) {
      dbGetQuery(con,sprintf("insert into source (name, file, id, status,date,owner )
                             values ('%s', '%s',%i, %i, '%s', '%s')",
                             Fname,content, nextid, 1,date(),.rqda$owner))
    } 
  }
}


FileNamesUpdate <- function(FileNamesWidget=.rqda$.fnames_rqda,sort=TRUE,decreasing = FALSE,...){
  ##update file names list in the FileNamesWidget
  wopt <- options(warn=-2)
  on.exit(options(wopt))
  source <- dbGetQuery(.rqda$qdacon, "select name, date, id from source where status=1")
  if (nrow(source)!=0) {
    fnames <- source$name
    Encoding(fnames) <- "UTF-8"
    if (sort){
      fnames <- fnames[OrderByTime(source$date,decreasing=decreasing)]
    }
    tryCatch(FileNamesWidget[] <- fnames,error=function(e){})
  }
}



## UncodedFileNamesUpdate <- function(FileNamesWidget = .rqda$.fnames_rqda, sort=TRUE, decreasing = FALSE){
## replaced by the general function of FileNameWigetUpdate() and GetFileId()
## ## only show the uncoded file names in the .rqda$.fnames_rqda
## ## The fnames will be sort if sort=TRUE
##   fid <- dbGetQuery(.rqda$qdacon,"select id from source where status==1 group by id")$id
##   if (!is.null(fid)){
##     fid_coded <- dbGetQuery(.rqda$qdacon,"select fid from coding where status==1 group by fid")$fid
##     fid_uncoded <- fid[! (fid %in% fid_coded)]
##     source <- dbGetQuery(.rqda$qdacon,
##                          sprintf("select name,date, id from source where status=1 and id in (%s)",
##                                  paste(fid_uncoded,sep="",collapse=",")))
##     if (nrow(source) != 0){
##       fnames <- source$name
##       Encoding(fnames) <- "UTF-8"
##       if (sort){
##       fnames <- fnames[OrderByTime(source$date,decreasing=decreasing)]
##       }
##     }
##     tryCatch(FileNamesWidget[] <- fnames, error = function(e) {})
##   }
## }


## setEncoding <- function(encoding="unknown"){
  ## moved to utils.R
##   ## specify what encoding is used in the imported files.
##   .rqda$encoding <- encoding
## }

## enc <- function(x,encoding="UTF-8") {
##   ## replace " with two '. to make insert smoothly.
##   ## encoding is the encoding of x (character vector).
##   ## moved to utils.R
##   Encoding(x) <- encoding
##   x <- gsub("'", "''", x)
##   if (Encoding(x)!="UTF-8") {
##     x <- iconv(x,to="UTF-8")
##   }
##   x
## }


ViewFileFun <- function(FileNameWidget){
## FileNameWidget=.rqda$.fnames_rqda in Files Tab
## FileNameWidget=.rqda$.FileofCat in F-CAT Tab
        if (is_projOpen(env = .rqda, conName = "qdacon")) {
            if (length(svalue(FileNameWidget)) == 0) {
                gmessage("Select a file first.", icon = "error", 
                  con = TRUE)
            }
            else {
                tryCatch(dispose(.rqda$.root_edit), error = function(e) {
                })
                SelectedFileName <- svalue(FileNameWidget)
                assign(".root_edit", gwindow(title = SelectedFileName, 
                  parent = c(395, 10), width = 600, height = 600), 
                  env = .rqda)
                .root_edit <- get(".root_edit", .rqda)
                assign(".openfile_gui", gtext(container = .root_edit, 
                  font.attr = c(sizes = "large")), env = .rqda)
                Encoding(SelectedFileName) <- "unknown"
                IDandContent <- dbGetQuery(.rqda$qdacon, sprintf("select id, file from source where name='%s'", 
                  SelectedFileName))
                content <- IDandContent$file
                Encoding(content) <- "UTF-8"
                W <- get(".openfile_gui", .rqda)
                add(W, content, font.attr = c(sizes = "large"))
                tryCatch(slot(W, "widget")@widget$SetEditable(FALSE),error=function(e){})
                mark_index <-
                  dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend from coding where fid=%i and status=1",IDandContent$id))
                if (nrow(mark_index)!=0){
                ## make sense only when there is coding there
                  ClearMark(W ,0 , max(mark_index$selend))
                  HL(W,index=mark_index)
                }
            }
        }
    }


write.FileList <- function(FileList,encoding=.rqda$encoding,con=.rqda$qdacon,...){
  ## import a list of files into the source table
  ## FileList is a list of file content, with names(FileList) the name of the files.
  WriteToTable <- function(Fname,content){
    ## helper function
    FnameUTF8 <- iconv(Fname,to="UTF-8")
    content <- enc(content,encoding=encoding) ## adjust encoding argument.
    if (Encoding(content)!="UTF-8"){
      content <- iconv(content,to="UTF-8") ## UTF-8 file content
    }
    maxid <- dbGetQuery(con,"select max(id) from source")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
  ## check if the content should be written into con.
    if (nextid==1) {
      write <- TRUE
      ## if this is the first file, no need to worry about the duplication issue.
    } else {
      if (nrow(dbGetQuery(con,sprintf("select name from source where name=='%s'",FnameUTF8)))==0) {
      ## no duplication file exists, then write.
        write <- TRUE
      } else {
        cat(sprintf("%s exists in the database!\n",Fname))
      }
    }
  if (write ) {
    dbGetQuery(con,sprintf("insert into source (name, file, id, status,date,owner )
                             values ('%s', '%s',%i, %i, '%s', '%s')",
                           Fname,content, nextid, 1,date(),.rqda$owner))
  }
  }
  FileNames <- names(FileList)
  FileNames[FileNames==""] <- as.character(1:sum(FileNames==""))

  if (isIdCurrent(con)) {
    for (i in 1:length(FileList)) {
      WriteToTable(FileNames[i],FileList[[i]])
    }
    FileNamesUpdate(FileNamesWidget=.rqda$.fnames_rqda)
    } else gmessage("Open a project first.", con=TRUE)
}


ProjectMemoWidget <- function(){
  if (is_projOpen(env=.rqda,"qdacon")) {
    ## use enviroment, so you can refer to the same object easily, this is the beauty of environment
    ## if project is open, then continue
    tryCatch(dispose(.rqda$.projmemo),error=function(e) {})
    ## Close the open project memo first, then open a new one
    ## .projmemo is the container of .projmemocontent,widget for the content of memo
    assign(".projmemo",gwindow(title="Project Memo", parent=c(395,10),width=600,height=400),env=.rqda)
    .projmemo <- get(".projmemo",.rqda)
    .projmemo2 <- gpanedgroup(horizontal = FALSE, con=.projmemo)
    ## use .projmemo2, so can add a save button to it.
    gbutton("Save memo",con=.projmemo2,handler=function(h,...){
      ## send the new content of memo back to database
      newcontent <- svalue(W)
      ## Encoding(newcontent) <- "UTF-8"
      newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
      dbGetQuery(.rqda$qdacon,sprintf("update project set memo='%s' where rowid==1", ## only one row is needed
                                      newcontent)
                 ## have to quote the character in the sql expression
                 )
    }
            )## end of save memo button
    tmp <- gtext(container=.projmemo2,font.attr=c(sizes="large"))
    font <- pangoFontDescriptionFromString("Sans 11")
    gtkWidgetModifyFont(tmp@widget@widget,font)
    assign(".projmemocontent",tmp,env=.rqda)
    prvcontent <- dbGetQuery(.rqda$qdacon, "select memo from project")[1,1]
    ## [1,1]turn data.frame to 1-length character. Existing content of memo
    if (length(prvcontent)==0) {
      dbGetQuery(.rqda$qdacon,"replace into project (memo) values('')")
      prvcontent <- ""
      ## if there is no record in project table, it fails to save memo, so insert sth into it
    }
    W <- .rqda$.projmemocontent
    Encoding(prvcontent) <- "UTF-8"
    ## add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
    add(W,prvcontent,do.newline=FALSE)
    ## do.newline:do not add a \n (new line) at the beginning
    ## push the previous content to the widget.
    }
}



FileNameWidgetUpdate <- function(FileNamesWidget=.rqda$.fnames_rqda,sort=TRUE,decreasing = FALSE,FileId=NULL,...){
  ##update file names list in the FileNamesWidget
  wopt <- options(warn=-2)
  on.exit(options(wopt))
  source <- dbGetQuery(.rqda$qdacon, "select name, date, id from source where status=1")
  if (nrow(source)==0){
    fnames <- NULL
  } else {
    Encoding(source$name) <- "UTF-8"
    if (!is.null(FileId)){
      source <- source[source$id %in% FileId,]
      fnames <- source$name ## when FileId is not in source$id, fnames is character(0), still works.
      date <- source$date
    } else{
      fnames <- source$name
      date <- source$date
    }
    if (sort){
      fnames <- fnames[OrderByTime(date,decreasing=decreasing)]
    }
  }
  tryCatch(FileNamesWidget[] <- fnames,error=function(e){})
}


GetFileId <- function(condition=c("unconditional","case","filecategory"),type=c("all","coded","uncoded","selected"))
{
  ## helper function
  unconditionalFun <- function(type)
    {
      if (type=="selected"){
        selected <- svalue(.rqda$.fnames_rqda)
        ans <- dbGetQuery(.rqda$qdacon,
                          sprintf("select id from source where status==1 and name in (%s)",
                                  paste(paste("'",selected,"'",sep=""),collapse=",")
                                  ))$id
      } else {
      allfid <- dbGetQuery(.rqda$qdacon,"select id from source where status==1 group by id")$id
      if (type!="all"){
        fid_coded <- dbGetQuery(.rqda$qdacon,"select fid from coding where status==1 group by fid")$fid
      }
      if (type=="all") {
        ans <- allfid
      } else if (type=="coded"){
        ans <- fid_coded
      } else if (type=="uncoded"){
        ans <- allfid[! (allfid %in% fid_coded)]
      }
    }
      ans
    }

  FidOfCaseFun <- function(type){
    if (type=="selected"){
      selected <- svalue(.rqda$.FileofCase)
      ans <- dbGetQuery(.rqda$qdacon,
                        sprintf("select id from source where status==1 and name in (%s)",
                                paste(paste("'",selected,"'",sep=""),collapse=",")
                                ))$id
    } else {
      Selected <- svalue(.rqda$.CasesNamesWidget)
      if (length(Selected)==0){
        ans <- NULL
      } else {
        if (length(Selected)>1) {gmessage("select one file category only.",con=TRUE)
                                  stop("more than one file categories are selected")
                                }
        caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",Selected))$id
        fidofcase <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status==1 and caseid==%i",caseid))$fid
##         caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name in (%s)",
##                                                  paste(paste("'",Selected,"'",sep=""),collapse=",")))$id
##         fidofcase <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status==1 and caseid in (%s)",
##                                                     paste(paste("'",caseid,"'",sep=""),collapse=",")))$fid
## roll back to rev 90
        allfid <-  unconditionalFun(type=type)
        ans <- intersect(fidofcase,allfid)
      }
    }
    ans
  }

  FidOfCatFun <- function(type){
    if (type=="selected"){
      selected <- svalue(.rqda$.FileofCat)
      ans <- dbGetQuery(.rqda$qdacon,
                  sprintf("select id from source where status==1 and name in (%s)",
                          paste(paste("'",selected,"'",sep=""),collapse=",")
                          ))$id
    } else {
      Selected <- svalue(.rqda$.FileCatWidget)
      if (length(Selected)==0){
        ans <- NULL
      } else {
        if (length(Selected)>1) {gmessage("select one case only.",con=TRUE)
                                 stop("more than one cases are selected")
                               }
        catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",Selected))$catid
        fidofcat <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status==1 and catid==%i",catid))$fid
        ## roll back to rev 90
##         catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name in (%s)",
##                                                  paste(paste("'",Selected,"'",sep=""),collapse=",")))$catid
##         fidofcat <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status==1 and catid in (%s)",
##                                                     paste(paste("'",catid,"'",sep=""),collapse=",")))$fid
        allfid <-  unconditionalFun(type=type)
        ans <- intersect(fidofcat,allfid)
      }
      ans
    }
  }
    
  condition <- match.arg(condition)
  type <- match.arg(type)
  fid <- switch(condition,
                unconditional=unconditionalFun(type=type),
                case=FidOfCaseFun(type=type),
                filecategory=FidOfCatFun(type=type)
                )
fid
}



GetFileIdSets <- function(set=c("case","filecategory"),relation=c("union","intersect")){
  set <- match.arg(set)
  relation <- match.arg(relation)
  if (set=="case") {
    Selected <- svalue(.rqda$.CasesNamesWidget)
    if (length(Selected)==0){
      ans <- NULL
    } else {
      Selected <- gsub("'", "''", Selected)
      if (relation=="union"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status==1 and caseid in (select id from cases where status=1 and name in (%s)) group by fid", paste(paste("'",Selected,"'",sep=""),collapse=",")))$fid
      } else if (relation=="intersect"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid, count(fid) as n from caselinkage where status==1 and caseid in (select id from cases where status=1 and name in (%s)) group by fid having n= %i", paste(paste("'",Selected,"'",sep=""),collapse=","),length(Selected)))$fid
      }
    }
  }## end of set=="case"
  if (set=="filecategory"){
    Selected <- svalue(.rqda$.FileCatWidget)
    if (length(Selected)==0){
      ans <- NULL
    } else {
      Selected <- gsub("'", "''", Selected)
      if (relation=="union"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status==1 and catid in (select catid from filecat where status=1 and name in (%s)) group by fid", paste(paste("'",Selected,"'",sep=""),collapse=",")))$fid
      } else if (relation=="intersect"){
        ans <- dbGetQuery(.rqda$qdacon,sprintf("select fid, count(fid) as n from treefile where status==1 and catid in (select catid from filecat where status=1 and name in (%s)) group by fid having n= %i", paste(paste("'",Selected,"'",sep=""),collapse=","),length(Selected)))$fid
      }
    }
  } ## end of set=="filecategory"
  ans
}

AddToFileCategory<- function(Widget=.rqda$.fnames_rqda,updateWidget=TRUE){
  ## filenames -> fid -> selfirst=0; selend=nchar(filesource)
  filename <- svalue(Widget)
  Encoding(filename) <- "unknown"
  query <- dbGetQuery(.rqda$qdacon,sprintf("select id, file from source where name in(%s) and status=1",paste("'",filename,"'",sep="",collapse=","))) ## multiple fid
  fid <- query$id
  Encoding(query$file) <- "UTF-8"
  
  ## select a F-cat name -> F-cat id
  Fcat <- dbGetQuery(.rqda$qdacon,"select catid, name from filecat where status=1")
  if (nrow(Fcat)==0){gmessage("Add File Categroy first.",con=TRUE)} else{
    Encoding(Fcat$name) <- "UTF-8"
    Selected <- gselect.list(Fcat$name,multiple=FALSE)
    ## CurrentFrame <- sys.frame(sys.nframe())
    ## RunOnSelected(Fcat$name,multiple=TRUE,enclos=CurrentFrame,expr={
    if (Selected!=""){ ## must use Selected to represent the value of selected items. see RunOnSelected() for info.
      ##Selected <- iconv(Selected,to="UTF-8")
      Encoding(Selected) <- "UTF-8"
      Fcatid <- Fcat$catid[Fcat$name %in% Selected]
      exist <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and fid in (%s) and catid=%i",paste("'",fid,"'",sep="",collapse=","),Fcatid))
    if (nrow(exist)!=length(fid)){
      ## write only when the selected file associated with specific f-cat is not there
      DAT <- data.frame(fid=fid[!fid %in% exist$fid], catid=Fcatid, date=date(),dateM=date(),memo='',status=1)
      ## should pay attention to the var order of DAT, must be the same as that of treefile table
      success <- dbWriteTable(.rqda$qdacon,"treefile",DAT,row.name=FALSE,append=TRUE)
      ## write to caselinkage table
      if (success && updateWidget) {
        UpdateFileofCatWidget()
      }
      if (!success) gmessage("Fail to write to database.")
    }
    }
  }
}
  
