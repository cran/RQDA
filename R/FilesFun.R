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



ViewFileFun <- function(FileNameWidget,hightlight=TRUE){
## FileNameWidget=.rqda$.fnames_rqda in Files Tab
## FileNameWidget=.rqda$.FileofCat in F-CAT Tab
  if (is_projOpen(env = .rqda, conName = "qdacon")) {
    if (length(svalue(FileNameWidget)) == 0) {
      gmessage("Select a file first.", icon = "error",con = TRUE)
    } else {
      SelectedFileName <- svalue(FileNameWidget)
      ViewFileFunHelper(SelectedFileName,hightlight=TRUE)
    }}}


ViewFileFunHelper <- function(FileName,hightlight=TRUE){
  tryCatch(dispose(.rqda$.root_edit), error = function(e) {})
  SelectedFileName <- FileName
  gw <- gwindow(title = SelectedFileName,parent = getOption("widgetCoordinate"), width = 600, height = 600)
  mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
  gw@widget@widget$SetIconFromFile(mainIcon)
  assign(".root_edit", gw, env = .rqda)
  .root_edit <- get(".root_edit", .rqda)
  tmp <- gtext(container=.root_edit)
  font <- pangoFontDescriptionFromString(.rqda$font)
  gtkWidgetModifyFont(tmp@widget@widget,font)
  tmp@widget@widget$SetPixelsBelowLines(5) ## set the spacing
  tmp@widget@widget$SetPixelsInsideWrap(5) ## so the text looks more confortable.
  assign(".openfile_gui", tmp, env = .rqda)
  Encoding(SelectedFileName) <- "unknown"
  IDandContent <- dbGetQuery(.rqda$qdacon, sprintf("select id, file from source where name='%s'",SelectedFileName))
  content <- IDandContent$file
  Encoding(content) <- "UTF-8"
  W <- get(".openfile_gui", .rqda)
  add(W, content)
  slot(W, "widget")@widget$SetEditable(FALSE)
  markidx <- dbGetQuery(.rqda$qdacon,sprintf("select coding.rowid,coding.selfirst,coding.selend,freecode.name from coding,freecode where coding.fid=%i and coding.status=1 and freecode.id==coding.cid and freecode.status==1",IDandContent$id))
  anno <- RQDAQuery(sprintf("select position,rowid from annotation where status==1 and fid==%s",IDandContent$id))
  buffer <- W@widget@widget$GetBuffer()
  if (nrow(markidx)!=0){ ## make sense only when there is coding there
    apply(markidx[,1:3],1,function(x){
      iter <- gtkTextBufferGetIterAtOffset(buffer, x["selfirst"]) ## index to iter
      buffer$CreateMark(sprintf("%s.1",x["rowid"]),where=iter$iter) ## insert marks
      iter <- gtkTextBufferGetIterAtOffset(buffer, x["selend"])
      buffer$CreateMark(sprintf("%s.2",x["rowid"]),where=iter$iter)
    })} ## create marks
  if (nrow(anno)!=0){
    apply(anno,1,function(x){
      iter <- gtkTextBufferGetIterAtOffset(buffer, x["position"]) ## index to iter
      buffer$CreateMark(sprintf("%s.3",x["rowid"]),where=iter$iter) ## insert marks
    })} ## creat marks for annotation
  if (nrow(markidx)!=0){
    sapply(markidx[, "rowid"], FUN = function(x) {
      code <- enc(markidx[markidx$rowid == x, "name"],"UTF-8")
      m1 <- buffer$GetMark(sprintf("%s.1", x))
      iter1 <- buffer$GetIterAtMark(m1)
      idx1 <- gtkTextIterGetOffset(iter1$iter)
      InsertAnchor(.rqda$.openfile_gui, label = sprintf("%s<",code), index = idx1,handler=TRUE)
      m2 <- buffer$GetMark(sprintf("%s.2", x))
      iter2 <- buffer$GetIterAtMark(m2)
      idx2 <- gtkTextIterGetOffset(iter2$iter)
      InsertAnchor(.rqda$.openfile_gui, label = sprintf(">%s",code), index = idx2)
    }) ## end of sapply -> insert code label
    if (hightlight){
      idx <- sapply(markidx[, "rowid"], FUN = function(x) {
        m1 <- buffer$GetMark(sprintf("%s.1", x))
        iter1 <- buffer$GetIterAtMark(m1)
        idx1 <- gtkTextIterGetOffset(iter1$iter)
        m2 <- buffer$GetMark(sprintf("%s.2", x))
        iter2 <- buffer$GetIterAtMark(m2)
        idx2 <- gtkTextIterGetOffset(iter2$iter)
        return(c(idx1,idx2))
      })## get offset for HL.
      idx <- t(idx)
      HL(W, idx, fore.col = .rqda$fore.col, back.col = NULL)
    }}
  if (nrow(anno)!=0){
    apply(anno,1,function(x){
      m <- buffer$GetMark(sprintf("%s.3", x["rowid"]))
      iter <- buffer$GetIterAtMark(m)
      idx <- gtkTextIterGetOffset(iter$iter)
      InsertAnnotation(index=idx,fid=IDandContent$id, rowid=x["rowid"])
    })}
    buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter) ## place cursor at the beginning
}


EditFileFun <- function(FileNameWidget=.rqda$.fnames_rqda){
  ## FileNameWidget=.rqda$.fnames_rqda in Files Tab
  ## FileNameWidget=.rqda$.FileofCat in F-CAT Tab
  if (is_projOpen(env = .rqda, conName = "qdacon")) {
    SelectedFileName <- svalue(FileNameWidget)
    if (length(svalue(FileNameWidget)) == 0) {
      gmessage("Select a file first.", icon = "error", con = TRUE)
    }
    else {
      tryCatch(dispose(.rqda$.root_edit),error=function(e) {})
      assign(".root_edit",gwindow(title=SelectedFileName,parent=getOption("widgetCoordinate"),
                                  width=600,height=600),env=.rqda)
      assign(".root_edit2",gpanedgroup(horizontal = FALSE, con=.rqda$.root_edit),env=.rqda)
      gbutton("Save File",con=.rqda$.root_edit2,handler=function(h,...){
        content <-  svalue(.rqda$.openfile_gui)
        RQDAQuery(sprintf("update source set file='%s', dateM='%s' where name='%s'",
                          enc(content,"UTF-8"),date(),enc(svalue(.rqda$.root_edit),"UTF-8"))) ## update source table
        if (nrow(mark_index)!=0){ ## only manipulate the coding when there is one.
            idx <- apply(mark_index, 1, FUN = function(x) {
            m1 <- buffer$GetMark(sprintf("%s.1", x[3]))
            iter1 <- buffer$GetIterAtMark(m1)
            idx1 <- gtkTextIterGetOffset(iter1$iter)
            m2 <- buffer$GetMark(sprintf("%s.2", x[3]))
            iter2 <- buffer$GetIterAtMark(m2)
            idx2 <- gtkTextIterGetOffset(iter2$iter)
            ans <- c(selfirst = idx1, selend = idx2,x[3])## matrix of 3x N (N=nrow(mark_index))
        }) ## end of apply
            apply(idx,2,FUN=function(x){
                if (x[1]==x[2])  RQDAQuery(sprintf("update coding set status=0 where rowid=%i",x[3])) else {
                    Encoding(content) <- "UTF-8"
                    RQDAQuery(sprintf("update coding set seltext='%s',selfirst=%i, selend=%i where rowid=%i",
                                      enc(substr(content,x[1],x[2]),"UTF-8"),x[1],x[2],x[3]))
                }
            })## update the coding table (seltext,selfirst, selend), on the rowid (use rowid to name the marks)
        }
        if (nrow(mark_idx_case)!=0){ ## only manipulate the coding when there is one.
            idx_case <- apply(mark_idx_case, 1, FUN = function(x) {
                m1 <- buffer$GetMark(sprintf("c%s.1", x["rowid"]))
                iter1 <- buffer$GetIterAtMark(m1)
                idx1 <- gtkTextIterGetOffset(iter1$iter)
                m2 <- buffer$GetMark(sprintf("c%s.2", x["rowid"]))
                iter2 <- buffer$GetIterAtMark(m2)
                idx2 <- gtkTextIterGetOffset(iter2$iter)
                ans <- c(selfirst = idx1, selend = idx2,x["rowid"])
            }) ## end of apply
            apply(idx_case,2,FUN=function(x){
                if (x[1]==x[2])  RQDAQuery(sprintf("update caselinkage set status=0 where rowid=%i",x["rowid"])) else {
                    RQDAQuery(sprintf("update caselinkage set selfirst=%i, selend=%i where rowid=%i",x[1],x[2],x[3]))
                }
            })## end of apply
        }
    })## end of save memo button
      tmp <- gtext(container=.rqda$.root_edit2)
      font <- pangoFontDescriptionFromString(.rqda$font)
      gtkWidgetModifyFont(tmp@widget@widget,font)
      assign(".openfile_gui", tmp, env = .rqda)
      Encoding(SelectedFileName) <- "unknown"
      IDandContent <- dbGetQuery(.rqda$qdacon, sprintf("select id, file from source where name='%s'",SelectedFileName))
      content <- IDandContent$file
      Encoding(content) <- "UTF-8"
      W <- get(".openfile_gui", .rqda)
      ## add(W, content, font.attr = c(sizes = "large"))
      add(W, content)
      buffer <- slot(W, "widget")@widget$GetBuffer() ## get text buffer.
      mark_index <- dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend,rowid from coding where fid=%i and status=1",
                                                    IDandContent$id))
      if (nrow(mark_index)!=0){## make sense only when there is coding there
        ClearMark(W ,0 , max(mark_index$selend),TRUE,FALSE)
        HL(W,index=mark_index[,c("selfirst","selend")],.rqda$fore.col,NULL)
        ## insert marks according to mark_index (use rowid to name the marks)
        apply(mark_index,1,function(x){
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[1]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.1",x[3]),where=iter$iter)         ## insert marks
          ## gtkTextMarkSetVisible(mark,TRUE)                   ## set itvisible
          iter <- gtkTextBufferGetIterAtOffset(buffer, x[2]) ## index to iter
          mark <- buffer$CreateMark(sprintf("%s.2",x[3]),where=iter$iter)         ## insert marks
          ## gtkTextMarkSetVisible(mark,TRUE)                   ## set itvisible
        }) ## end of apply
    }
      mark_idx_case<- dbGetQuery(.rqda$qdacon,sprintf("select selfirst,selend,rowid from caselinkage where fid=%i and status=1",
                                                      IDandContent$id))
      if (nrow(mark_idx_case)!=0){
          ClearMark(W ,0 , max(mark_idx_case$selend),FALSE,TRUE)
          HL(W,index=mark_idx_case[,c("selfirst","selend")],NULL,.rqda$back.col)
          apply(mark_idx_case,1,function(x){
              iter <- gtkTextBufferGetIterAtOffset(buffer, x["selfirst"])
              mark <- buffer$CreateMark(sprintf("c%s.1",x["rowid"]),where=iter$iter)
              gtkTextMarkSetVisible(mark,TRUE)
              iter <- gtkTextBufferGetIterAtOffset(buffer, x["selend"])
              mark <- buffer$CreateMark(sprintf("c%s.2",x["rowid"]),where=iter$iter)
              gtkTextMarkSetVisible(mark,TRUE)
          }) ## end of apply
    }
  }}}


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
    tmp <- gtext(container=.projmemo2)
    font <- pangoFontDescriptionFromString(.rqda$font)
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
    }
    allfid <- GetFileIdSets("filecategory","intersect")
    if (type=="all") {ans <- allfid} else {
      codedfid <- RQDAQuery(sprintf("select fid from coding where status==1 and fid in (%s) group by fid",paste(shQuote(allfid),collapse=",")))$fid
      if (type=="coded") {ans <- codedfid}
      if (type=="uncoded") { ans <-  setdiff(allfid,codedfid)}
    }
    ans
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

AddToFileCategory <- function(Widget=.rqda$.fnames_rqda,updateWidget=TRUE){
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
    Selecteds <- gselect.list(Fcat$name,multiple=TRUE)
    if (length(Selecteds)>0 || Selecteds!=""){
      Encoding(Selecteds) <- "UTF-8"
      for (Selected in Selecteds) {
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
          if (!success) gmessage(sprintf("Fail to write to file category of %s",Selected))
        }
      }
    }
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
