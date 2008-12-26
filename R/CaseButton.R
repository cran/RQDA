AddCaseButton <- function(label="ADD"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      CaseName <- ginput("Enter new Case Name. ", icon="info")
      if (CaseName!="") {
        Encoding(CaseName) <- "UTF-8"
        AddCase(CaseName)
        CaseNamesUpdate()
      }
    }
  }
          )
}

DeleteCaseButton <- function(label="Delete"){
  gbutton(label,
          handler=function(h,...)
          {
            if (is_projOpen(env=.rqda,conName="qdacon") &
                length(svalue(.rqda$.CasesNamesWidget))!=0) {
              del <- gconfirm("Really delete the Case?",icon="question")
              if (isTRUE(del)){
                SelectedCase <- svalue(.rqda$.CasesNamesWidget)
                Encoding(SelectedCase) <- "UTF-8"
                caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where name=='%s'",SelectedCase))$id
                dbGetQuery(.rqda$qdacon,sprintf("update cases set status=0 where name=='%s'",SelectedCase))
                ## set status in table freecode to 0
                dbGetQuery(.rqda$qdacon,sprintf("update caselinkage set status=0 where caseid=%i",caseid))
                ## set status in table caselinkage to 0
                CaseNamesUpdate()
              }
                                 }
          }
          )
}

Case_RenameButton <- function(label="Rename",CaseNamesWidget=.rqda$.CasesNamesWidget,...)
{
  ## rename of selected case.
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,"qdacon")) {
      ## if project is open, then continue
      selectedCaseName <- svalue(CaseNamesWidget)
      if (length(selectedCaseName)==0){
        gmessage("Select a Case first.",text=selectedCaseName,icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewName <- ginput("Enter new Case name. ", text=selectedCaseName, icon="info")
        if (NewName != ""){
          Encoding(NewName) <- "UTF-8"
          rename(selectedCaseName,NewName,"cases")
          CaseNamesUpdate()
        }
      }
    }
  }
          )
}



CaseMemoButton <- function(label="Memo",...){
## no longer used
  gbutton(label, handler=function(h,...) {
    ## code memo: such as meaning of code etc.
    if (is_projOpen(env=.rqda,"qdacon")) {
      currentCase <- svalue(.rqda$.CasesNamesWidget)
      if (length(currentCase)==0){
        gmessage("Select a Case first.",icon="error",con=TRUE)
      }
      else {
        tryCatch(dispose(.rqda$.casememo),error=function(e) {})
        assign(".casememo",gwindow(title=paste("Case Memo",.rqda$currentCase,sep=":"),
                                   parent=c(370,10),width=600,height=400),env=.rqda)
        .casememo <- .rqda$.casememo
        .casememo2 <- gpanedgroup(horizontal = FALSE, con=.casememo)
        gbutton("Save Case Memo",con=.casememo2,handler=function(h,...){
          newcontent <- svalue(W)
          Encoding(newcontent) <- "UTF-8"
          newcontent <- enc(newcontent) ## take care of double quote.
          Encoding(currentCase) <- "UTF-8"
          dbGetQuery(.rqda$qdacon,sprintf("update cases set memo='%s' where name='%s'",newcontent,currentCase))
        }
                )## end of save memo button
        assign(".casememoW",gtext(container=.casememo2,font.attr=c(sizes="large")),env=.rqda)
        prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select memo from cases where name='%s'",currentCase))[1,1]
        if (is.na(prvcontent)) prvcontent <- ""
        Encoding(prvcontent) <- "UTF-8"
        W <- .rqda$.casememoW
        add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
      }
    }
  }
          )
}


CaseMark_Button<-function(){
  gbutton("Mark",
          handler=function(h,...) {MarkCaseFun()}
          )
}

MarkCaseFun <- function(){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    con <- .rqda$qdacon
    tryCatch({
      ans <- mark(get(".openfile_gui",env=.rqda),fore.col=NULL,back.col=.rqda$back.col)
      ## can change the color
      if (ans$start != ans$end){ 
        ## when selected no text, makes on sense to do anything.
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        Encoding(SelectedCase) <- "UTF-8"
        currentCid <-  dbGetQuery(con,sprintf("select id from cases where name=='%s'",
                                              SelectedCase))[,1]
        SelectedFile <- svalue(.rqda$.root_edit)
        Encoding(SelectedFile) <- "UTF-8"
        currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",
                                              SelectedFile))[,1]
        ## Query of caselinkage
        ExistLinkage <-  dbGetQuery(con,sprintf("select rowid, selfirst, selend,status from caselinkage where caseid==%i and fid=%i and status=1",currentCid,currentFid))
        DAT <- data.frame(cid=currentCid,fid=currentFid,
                          selfirst=ans$start,selend=ans$end,status=1,
                          owner=.rqda$owner,date=date(),memo="")
        if (nrow(ExistLinkage)==0){
          ## if there are no relevant caselinkage, write the caselinkage table
          success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
          if (!success) gmessage("Fail to write to database.")
        } else {
          Relations <- apply(ExistLinkage,1,FUN=function(x) relation(x[c("selfirst","selend")],c(ans$start,ans$end)))
          ExistLinkage$Relation <- sapply(Relations,FUN=function(x)x$Relation)
          if (!any(ExistLinkage$Relation=="exact")){
            ## if there are exact caselinkage, skip; if no exact linkage then continue
            ExistLinkage$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
            ExistLinkage$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
            ExistLinkage$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
            if (all(ExistLinkage$Relation=="proximity")){
              success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
              if (!success) gmessage("Fail to write to database.")
            } else {
              del1 <- ExistLinkage$WhichMin==2 & ExistLinkage$Relation =="inclusion"; del1[is.na(del1)] <- FALSE
              del2 <- ExistLinkage$Relation =="overlap"; del2[is.na(del2)] <- FALSE
              del <- (del1 | del2)
              if (any(del)){
              Sel <- c(min(ExistLinkage$Start[del]), max(ExistLinkage$End[del]))
              memo <- dbGetQuery(.rqda$qdacon,sprintf("select memo from caselinkage where rowid in (%s)",
                                                      paste(ExistLinkage$rowid[del],collapse=",",sep="")))$memo
              memo <- paste(memo,collapse="",sep="")
              dbGetQuery(.rqda$qdacon,sprintf("delete from caselinkage where rowid in (%s)",
                                              paste(ExistLinkage$rowid[del],collapse=",",sep="")))
              DAT <- data.frame(cid=currentCid,fid=currentFid,
                                selfirst=Sel[1],selend=Sel[2],status=1,
                                owner=.rqda$owner,date=date(),memo=memo)
              success <- dbWriteTable(.rqda$qdacon,"caselinkage",DAT,row.name=FALSE,append=TRUE)
              if (!success) gmessage("Fail to write to database.")
            }
            }
            }
          }
        }
    },error=function(e){}
             )
  }
}



CaseUnMark_Button<-function(label="Unmark"){
  gbutton(label,
          handler=function(h,...) {
            if (is_projOpen(env=.rqda,conName="qdacon")) {
              con <- .rqda$qdacon
              W <- tryCatch(get(h$action$widget,env=.rqda),error=function(e){})
              ## get the widget for file display. If it does not exist, then return NULL.
              sel_index <- tryCatch(sindex(W),error=function(e) {})
              ## if the not file is open, unmark doesn't work.
              if (!is.null(sel_index)) {
                SelectedCase <- svalue(.rqda$.CasesNamesWidget)
                if (length(SelectedCase)==0) {gmessage("Select a case first.",con=TRUE)} else{
                ## Encoding(SelectedCase) <- "UTF-8"
                caseid <-  dbGetQuery(.rqda$qdacon,sprintf("select id from cases where name=='%s'",SelectedCase))[,1]
                SelectedFile <- svalue(.rqda$.root_edit)
                ## Encoding(SelectedFile) <- "UTF-8"
                currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'", SelectedFile))[,1]
                codings_index <-  dbGetQuery(con,sprintf("select rowid, caseid, fid, selfirst, selend from caselinkage where caseid==%i and fid==%i", caseid, currentFid))
                ## should only work with those related to current case and current file.
                rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                             (codings_index$selend  <= sel_index$endN)]
                if (is.numeric(rowid)) for (j in rowid) {
                  dbGetQuery(con,sprintf("update caselinkage set status=0 where rowid=%i", j))
                }
                ## better to get around the loop by sqlite condition expression.
                ClearMark(W,min=sel_index$startN,max=sel_index$endN,clear.fore.col = FALSE, clear.back.col = TRUE)
                ## even for the non-current code. can improve.
              }
              }
            }},action=list(widget=".openfile_gui")
            )
        }
  
##   AddWebSearchButton <- function(label="WebSearch",CaseNamesWidget=.rqda$.CasesNamesWidget){
##     gbutton(label,handler=function(h,...) {
##       if (is_projOpen(env=.rqda,conName="qdacon")) {
##         KeyWord <- svalue(CaseNamesWidget)
##         engine <- select.list(c("Baidu","Google","Yahoo"))
##         if (engine=="Baidu") {
##           KeyWord <- iconv(KeyWord, from="UTF-8")
##           browseURL(sprintf("http://www.baidu.com/s?wd=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
##         }
##         if (engine=="Yahoo") {
##           KeyWord <- iconv(KeyWord, from="UTF-8")
##           browseURL(sprintf("http://search.yahoo.com/search;_ylt=A0oGkmFV.CZJNssAOK.l87UF?p=%s&ei=UTF-8&iscqry=&fr=sfp&fr2=sfp"
##                             ,KeyWord))
##         }
##     if (engine=="Google")browseURL(sprintf("http://www.google.com/search?q=%s",KeyWord))
##       }
##     }
##             )
## }
  
CaseNamesWidgetMenu <- list()
CaseNamesWidgetMenu$"Add File(s)"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    SelectedCase <- svalue(.rqda$.CasesNamesWidget)
    caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
    freefile <-  dbGetQuery(.rqda$qdacon,"select name, id, file from source where status=1")
    fileofcase <- dbGetQuery(.rqda$qdacon,sprintf("select fid from caselinkage where status=1 and caseid=%i",caseid))
    Encoding(freefile[['name']]) <- Encoding(freefile[['file']]) <- "UTF-8"
    if (nrow(fileofcase)!=0){
      fileoutofcase <- subset(freefile,!(id %in% fileofcase$fid))
      } else  fileoutofcase <- freefile
    if (length(fileoutofcase[['name']])==0) gmessage("All files are linked with this case.", cont=TRUE) else {
      ##Selected <- select.list(fileoutofcase[['name']],multiple=TRUE)    
    CurrentFrame <- sys.frame(sys.nframe())
    ## sys.frame(): get the frame of n
    ## nframe(): get n of current frame
    ## The value of them depends on where they evaluated, should not placed inside RunOnSelected()
    RunOnSelected(fileoutofcase[['name']],multiple=TRUE,enclos=CurrentFrame,expr={
      if (length(Selected)> 0) {
        Selected <- iconv(Selected,to="UTF-8")
        fid <- fileoutofcase[fileoutofcase$name %in% Selected,"id"]
        selend <- nchar(fileoutofcase[fileoutofcase$name %in% Selected,"file"])
        Dat <- data.frame(caseid=caseid,fid=fid,selfirst=0,selend,status=1,owner=.rqda$owner,date=date(),memo="")
        dbWriteTable(.rqda$qdacon,"caselinkage",Dat,row.names=FALSE,append=TRUE)
        UpdateFileofCaseWidget()
      }})
  }
  }
}
CaseNamesWidgetMenu$"Case Memo"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    MemoWidget("Case",.rqda$.CasesNamesWidget,"cases")
    ## see CodeCatButton.R  for definition of MemoWidget
  }
}
CaseNamesWidgetMenu$"Sort by created time"$handler <- function(h,...){
CaseNamesUpdate(.rqda$.CasesNamesWidget)
}
CaseNamesWidgetMenu$"Web Search"$Google$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8")
    browseURL(sprintf("http://www.google.com/search?q=%s",KeyWord))
  }
}
CaseNamesWidgetMenu$"Web Search"$Yahoo$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8")
    browseURL(sprintf("http://search.yahoo.com/search;_ylt=A0oGkmFV.CZJNssAOK.l87UF?p=%s&ei=UTF-8&iscqry=&fr=sfp&fr2=sfp"
                      ,KeyWord))
  }
}
CaseNamesWidgetMenu$"Web Search"$Baidu$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8",to="CP936") ## should be in CP936 to work properly.
    browseURL(sprintf("http://www.baidu.com/s?wd=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
  }
}
CaseNamesWidgetMenu$"Web Search"$Sogou$handler <- function(h,...){
  KeyWord <- svalue(.rqda$.CasesNamesWidget)
  if (length(KeyWord)!=0){
    KeyWord <- iconv(KeyWord, from="UTF-8",to="CP936")## should be in CP936 to work properly.
    browseURL(sprintf("http://www.sogou.com/sohu?query=%s",paste("%",paste(charToRaw(KeyWord),sep="",collapse="%"),sep="",collapse="")))
  }
}


## pop-up menu of .rqda$.FileofCase
FileofCaseWidgetMenu <- list() ## not used yet.
FileofCaseWidgetMenu$"Drop Selected File(s)"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    FileOfCat <- svalue(.rqda$.FileofCase)
    if ((NumofSelected <- length(FileOfCat)) ==0) {
      gmessage("Please select the Files you want to delete.",con=TRUE)} else
    {
      ## Give a confirm msg
      del <- gconfirm(sprintf("Delete %i file(s) from this category. Are you sure?",NumofSelected),con=TRUE,icon="question")
      if (isTRUE(del)){
        SelectedCase <- svalue(.rqda$.CasesNamesWidget)
        ## Encoding(SelectedCase) <- Encoding(FileOfCat)<- "UTF-8"
        caseid <- dbGetQuery(.rqda$qdacon,sprintf("select id from cases where status=1 and name='%s'",SelectedCase))[,1]
        for (i in FileOfCat){
          fid <- dbGetQuery(.rqda$qdacon,sprintf("select id from source where status=1 and name='%s'",i))[,1]
          dbGetQuery(.rqda$qdacon,sprintf("update caselinkage set status==0 where caseid==%i and fid==%i",caseid,fid))
        }
        ## update Widget
        UpdateFileofCaseWidget()
      }
    }
  }
  }
FileofCaseWidgetMenu$"File Memo"$handler <- function(h,...){
        MemoWidget("File",.rqda$.FileofCase,"source")
}
FileofCaseWidgetMenu$"Show Uncoded Files Only (sorted)"$handler <- function(h,...){
if (is_projOpen(env=.rqda,conName="qdacon")) {
   fid <- GetFileId(condition="case",type="uncoded")
   FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
 }
}
FileofCaseWidgetMenu$"Show Coded Files Only (sorted)"$handler <- function(h,...){
if (is_projOpen(env=.rqda,conName="qdacon")) {
   fid <- GetFileId(condition="case",type="coded")
   FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
 }
}
FileofCaseWidgetMenu$"Sort by imported time"$handler <- function(h,...){
## UpdateFileofCaseWidget()
if (is_projOpen(env=.rqda,conName="qdacon")) {
   fid <- GetFileId(condition="case",type="all")
   FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCase,FileId=fid)
 }
}

