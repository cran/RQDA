AddCodeButton <- function(label="Add"){
  gbutton(label,
          handler=function(h,...) {
            if (is_projOpen(env=.rqda,conName="qdacon")) {
              codename <- ginput("Enter new code. ", icon="info")
              if (codename!=""){
                Encoding(codename) <- "UTF-8"
                addcode(codename)
                CodeNamesUpdate()
              }
            }
          }
          )
}


DeleteCodeButton <- function(label="Delete"){
  gbutton(label,
          handler=function(h,...)
          {
            if (is_projOpen(env=.rqda,conName="qdacon") &
                length(svalue(.rqda$.codes_rqda))!=0) {
              ## if project is open and one code is selected,then continue
              del <- gconfirm("Really delete the code?",icon="question")
              if (isTRUE(del)){
                SelectedCode <- svalue(.rqda$.codes_rqda)
                Encoding(SelectedCode) <- "UTF-8"
                cid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name=='%s'",SelectedCode))$id
                dbGetQuery(.rqda$qdacon,sprintf("update freecode set status=0 where name=='%s'",SelectedCode))
                ## set status in table freecode to 0
                dbGetQuery(.rqda$qdacon,sprintf("update coding set status=0 where cid==%i",cid))
                ## set status in table coding to 0
                CodeNamesUpdate()
              }
                                 }
          }
          )
}

RetrievalButton <- function(label){
  gbutton(label,
          handler=function(h,...) {
            if (is_projOpen(env=.rqda,conName="qdacon")) {
              retrieval()
            }
          }
          )
}

ExtendButton <- function(label){
  gbutton(label,
          handler=function(h,...) {
            if (is_projOpen(env=.rqda,conName="qdacon")) {
              retrieval2(CodeNameWidget=.rqda$.codes_rqda)
            }
          }
          )
}


HL_ALLButton <- function(){
  gbutton("HL ALL",
          handler=function(h,...) {
            if (is_projOpen(env=.rqda,conName="qdacon")) {
              con <- .rqda$qdacon
              SelectedFile <- tryCatch(svalue(.rqda$.root_edit),error=function(e){NULL})
              if (!is.null(SelectedFile)) {
              Encoding(SelectedFile) <- "UTF-8"
              currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
              W <- tryCatch( get(h$action$widget,.rqda),
                            error=function(e) {}
                            )
              if (length(currentFid)!=0 & !is.null(W)) {
                ## if fid is integer(0), then there is no file selected and open
                ## if W is null, then there is no valid widget. No need to HL.
                ## Though W may be expired, but ClearMark and HL will take care of the issue.
                mark_index <-
                  dbGetQuery(con,sprintf("select selfirst,selend,status from coding where fid=%i and status=1",currentFid))
                ## only select thoses with the open_file and not deleted (status=1).
                ClearMark(W ,0 , max(mark_index$selend))
                HL(W,index=mark_index)
              }
            }
            }
          },
          action=list(widget=".openfile_gui")
          )
}



Mark_Button<-function(){
  gbutton("Mark",
          handler=function(h,...) {MarkCodeFun()}
          )
}

MarkCodeFun <- function(){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    con <- .rqda$qdacon
    tryCatch({
      W <- get(".openfile_gui",env=.rqda)
      ans <- mark(W) ## can change the color
      if (ans$start != ans$end){ 
        ## when selected no text, makes on sense to do anything.
        SelectedCode <- svalue(.rqda$.codes_rqda)
        Encoding(SelectedCode) <- "UTF-8"
        currentCid <-  dbGetQuery(con,sprintf("select id from freecode where name=='%s'",SelectedCode))[,1]
        SelectedFile <- svalue(.rqda$.root_edit)
        Encoding(SelectedFile) <- "UTF-8"
        currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
        Exist <-  dbGetQuery(con,sprintf("select rowid, selfirst, selend from coding where cid==%i and fid=%i and status=1",currentCid,currentFid))
        DAT <- data.frame(cid=currentCid,fid=currentFid,seltext=ans$text,selfirst=ans$start,selend=ans$end,status=1,
                          owner=.rqda$owner,date=date(),memo="")
        if (nrow(Exist)==0){
          success <- dbWriteTable(.rqda$qdacon,"coding",DAT,row.name=FALSE,append=TRUE)
          if (!success) gmessage("Fail to write to database.")
        } else {
          Relations <- apply(Exist,1,FUN=function(x) relation(x[c("selfirst","selend")],c(ans$start,ans$end)))
          Exist$Relation <- sapply(Relations,FUN=function(x)x$Relation)
          if (!any(Exist$Relation=="exact")){
            ## if they are axact, do nothing; -> if they are not exact, do something.
            Exist$WhichMin <- sapply(Relations,FUN=function(x)x$WhichMin)
            Exist$Start <- sapply(Relations,FUN=function(x)x$UnionIndex[1])
            Exist$End <- sapply(Relations,FUN=function(x)x$UnionIndex[2])
            if (all(Exist$Relation=="proximity")){
              success <- dbWriteTable(.rqda$qdacon,"coding",DAT,row.name=FALSE,append=TRUE)
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
                memo <- paste(memo,collapse="",sep="")
                dbGetQuery(.rqda$qdacon,sprintf("delete from coding where rowid in (%s)",
                                                paste(Exist$rowid[del],collapse=",",sep="")))
                tt <- svalue(W); Encoding(tt) <- "UTF-8"
                DAT <- data.frame(cid=currentCid,fid=currentFid,seltext=substr(tt,Sel[1],Sel[2]),
                                  selfirst=Sel[1],selend=Sel[2],status=1,
                                  owner=.rqda$owner,date=date(),memo=memo)
                success <- dbWriteTable(.rqda$qdacon,"coding",DAT,row.name=FALSE,append=TRUE)
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
                                     
Unmark_Button <- function(){
  gbutton("Unmark",
                               handler=function(h,...) {
                                 if (is_projOpen(env=.rqda,conName="qdacon")) {
                                   con <- .rqda$qdacon
                                   W <- tryCatch( get(h$action$widget,env=.rqda),
                                                 error=function(e){}
                                                 )
                                   ## get the widget for file display. If it does not exist, then return NULL.
                                   sel_index <- tryCatch(sindex(W),error=function(e) {})
                                   ## if the not file is open, unmark doesn't work.
                                   if (!is.null(sel_index)) {
                                     SelectedCode <- svalue(.rqda$.codes_rqda)
                                     if (length(SelectedCode)==0) {gmessage("Select a code first.",con=TRUE)} else{
                                     Encoding(SelectedCode) <- "UTF-8"
                                     currentCid <-  dbGetQuery(.rqda$qdacon,
                                                               sprintf("select id from freecode where name=='%s'",
                                                                       SelectedCode))[,1]
                                     SelectedFile <- svalue(.rqda$.root_edit)
                                     Encoding(SelectedFile) <- "UTF-8"
                                     currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",
                                                                           SelectedFile))[,1]
codings_index <-  dbGetQuery(con,sprintf("select rowid, cid, fid, selfirst, selend from coding where cid==%i and fid==%i",
                                         currentCid, currentFid))
                                     ## should only work with those related to current code and current file.
                                     rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                                                  (codings_index$selend  <= sel_index$endN)]
                                     if (is.numeric(rowid)) for (j in rowid) {
                                       dbGetQuery(con,sprintf("update coding set status=-1 where rowid=%i", j))  }
                                     ## better to get around the loop by sqlite condition expression.
                                     ## status=-1 to differentiate the result of delete button
                                     ClearMark(W,min=sel_index$startN,max=sel_index$endN)
                                     ## This clear all the marks in the gtext window,
                                     ## even for the non-current code. can improve.
                                   }
                                   }
                                 }
                               },
          action=list(widget=".openfile_gui")
          )
}




CodeMemoButton <- function(label="C-Memo",...){
  gbutton(label, handler=function(h,...){
    MemoWidget("code",.rqda$.codes_rqda,"freecode")
  }
          )
}
##           {
##     ## code memo: such as meaning of code etc.
##     if (is_projOpen(env=.rqda,"qdacon")) {
##       currentCode <- svalue(.rqda$.codes_rqda)
##       if (length(currentCode)==0){
##         gmessage("Select a code first.",icon="error",con=TRUE)
##       }
##       else {
##         tryCatch(dispose(.rqda$.codememo),error=function(e) {})
##         assign(".codememo",gwindow(title=paste("Code Memo",.rqda$currentCode,sep=":"),
##                                    parent=c(370,10),width=600,height=400),env=.rqda)
##         .codememo <- .rqda$.codememo
##         .codememo2 <- gpanedgroup(horizontal = FALSE, con=.codememo)
##         gbutton("Save Code Memo",con=.codememo2,handler=function(h,...){
##           newcontent <- svalue(W)
##           Encoding(newcontent) <- "UTF-8"
##           newcontent <- enc(newcontent) ## take care of double quote.
##           Encoding(currentCode) <- "UTF-8"
##           dbGetQuery(.rqda$qdacon,sprintf("update freecode set memo='%s' where name='%s'",newcontent,currentCode))
##         }
##                 )## end of save memo button
##         assign(".cmemocontent",gtext(container=.codememo2,font.attr=c(sizes="large")),env=.rqda)
##         prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select memo from freecode where name='%s'",currentCode))[1,1]
##         if (is.na(prvcontent)) prvcontent <- ""
##         Encoding(prvcontent) <- "UTF-8"
##         W <- .rqda$.cmemocontent
##         add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
##       }
##     }
##   }
##           )
## }






CodingMemoButton <- function(label="C2Memo")
{
  gbutton(label, handler= function(h,...){
    con <- .rqda$qdacon
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      W <- tryCatch( get(".openfile_gui",env=.rqda), error=function(e){})
      ## get the widget for file display. If it does not exist, then return NULL.
      sel_index <- tryCatch(sindex(W),error=function(e) {}) ## if the not file is open, it doesn't work.
      if (is.null(sel_index)) {gmessage("Open a file first!",con=TRUE)}
      else {
        SelectedCode <- svalue(.rqda$.codes_rqda); Encoding(SelectedCode) <- "UTF-8"
        if (length(SelectedCode)==0) gmessage("Select a code first!") else {
          currentCid <-  dbGetQuery(con,sprintf("select id from freecode where name=='%s'",SelectedCode))[,1]
          ## SelectedFile <- svalue(.rqda$.fnames_rqda); Encoding(SelectedFile) <- "UTF-8"
          ## confused when selected file is not the open one
          SelectedFile <- svalue(.rqda$.root_edit); Encoding(SelectedFile) <- "UTF-8" ## more reliable
          currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
          codings_index <-  dbGetQuery(con,sprintf("select rowid, cid, fid, selfirst, selend from coding where
                                                   cid==%i and fid==%i ",currentCid, currentFid))
          ## should only work with those related to current code and current file.
          rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                       (codings_index$selfirst  <= sel_index$startN + 4) &
                                       (codings_index$selend  <= sel_index$endN)&
                                       (codings_index$selend  >= sel_index$endN - 4)
                                       ] ## determine which one is the current text chunk?
          if (length(rowid)!= 1) {gmessage("Select the exact coding first!", con=TRUE)}
          else {
            ##  open a widget for memo, and take care of the save memo function
            tryCatch(dispose(.rqda$.codingmemo),error=function(e) {})
            ## Close the coding memo first, then open a new one
            assign(".codingmemo",gwindow(title=paste("Coding Memo for",SelectedCode,sep=":"),
                                         parent=c(395,10),width=600,height=400
                                         ), env=.rqda
                   )
            .codingmemo <- get(".codingmemo",env=.rqda)
            .codingmemo2 <- gpanedgroup(horizontal = FALSE, con=.codingmemo)
            gbutton("Save Coding Memo",con=.codingmemo2,handler=function(h,...){
              newcontent <- svalue(W)
              Encoding(newcontent) <- "UTF-8"
              newcontent <- enc(newcontent) ## take care of double quote.
              dbGetQuery(con,sprintf("update coding set memo='%s' where rowid=%i",newcontent,rowid))
            }
                    )## end of save memo button
            assign(".cdmemocontent",gtext(container=.codingmemo2,font.attr=c(sizes="large")),env=.rqda)
            prvcontent <- dbGetQuery(con, sprintf("select memo from coding where rowid=%i",rowid))[1,1]
            if (is.na(prvcontent)) prvcontent <- ""
            Encoding(prvcontent) <- "UTF-8"
            W <- get(".cdmemocontent",env=.rqda)
            add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
          }
        }
      }
    }
  }
          )
}



FreeCode_RenameButton <- function(label="Rename",CodeNamesWidget=.rqda$.codes_rqda,...)
{
  ## rename of selected file.
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,"qdacon")) {
      ## if project is open, then continue
      selectedCodeName <- svalue(CodeNamesWidget)
      if (length(selectedCodeName)==0){
        gmessage("Select a code first.",icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewCodeName <- ginput("Enter new code name. ", text=selectedCodeName, icon="info")
        if (NewCodeName != "") {
          Encoding(NewCodeName) <- "UTF-8"
          ## update the name in source table by a function
          rename(selectedCodeName,NewCodeName,"freecode")
          ## (name is the only field should be modifed, as other table use ID rather than name)
          CodeNamesUpdate()
        }
      }
    }
  }
          )
}

## popup-menu
CodesNamesWidgetMenu <- list()
CodesNamesWidgetMenu$"Code Memo"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    MemoWidget("code",.rqda$.codes_rqda,"freecode")
    }
  }
CodesNamesWidgetMenu$"Sort by created time"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
     CodeNamesUpdate()
    }
  }


CodingInfoButton <- function(label="C2Info")
{
  gbutton(label,handler= function(h,...) c2InfoFun())
}

c2InfoFun <- function(){
    con <- .rqda$qdacon
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      W <- tryCatch( get(".openfile_gui",env=.rqda), error=function(e){})
      ## get the widget for file display. If it does not exist, then return NULL.
      sel_index <- tryCatch(sindex(W),error=function(e) {})
      ## if the not file is open, it doesn't work.
      if (is.null(sel_index)) {gmessage("Open a file first!",con=TRUE)}
      else {
          CodeTable <-  dbGetQuery(con,"select id,name from freecode where status==1")
          SelectedFile <- svalue(.rqda$.root_edit); Encoding(SelectedFile) <- "UTF-8" ##file title
          currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
          codings_index <-  dbGetQuery(con,sprintf("select rowid, cid, fid, selfirst, selend from coding where fid==%i ", currentFid))
          ## should only work with those related to current code and current file.
          rowid <- codings_index$rowid[(codings_index$selfirst  >= sel_index$startN) &
                                       (codings_index$selend  <= sel_index$endN)
                                       ] ## determine which codes correspond to the selection
          cid <- codings_index$cid[codings_index$rowid %in% rowid]
          Codes <- CodeTable$name[CodeTable$id %in% cid]
          ## should not use data frame as x, otherwise, svalue(c2infoWidget) is a factor rather than a character
          if (length(Codes)!=0){
            Encoding(Codes) <- "UTF-8"
            tryCatch(dispose(.rqda$.c2info),error=function(e){})
            gw <- gwindow(title="Associted code-list.",heigh=min(33*length(Codes),600),parent=.rqda$.openfile_gui)
            c2infoWidget <- gtable(Codes,con=gw)
            assign(".c2info",gw,env=.rqda)
            addhandlerdoubleclick(c2infoWidget,handler=function(h,...) retrieval(CodeNameWidget=c2infoWidget))
          }
        }}}
