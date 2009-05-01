addcode <- function(name,conName="qdacon",assignenv=.rqda,...) {
  if (name != ""){
    con <- get(conName,assignenv)
    maxid <- dbGetQuery(con,"select max(id) from freecode")[[1]]
    nextid <- ifelse(is.na(maxid),0+1, maxid+1)
    write <- FALSE
    if (nextid==1){
      write <- TRUE
    } else {
      dup <- dbGetQuery(con,sprintf("select name from freecode where name=='%s'",name))
      if (nrow(dup)==0) write <- TRUE
    }
    if (write ) {
      dbGetQuery(con,sprintf("insert into freecode (name, id, status,date,owner)
                                            values ('%s', %i, %i,%s, %s)",
                             name,nextid, 1, shQuote(date()),shQuote(.rqda$owner)))
    }
  }
}



CodeNamesUpdate <- function(CodeNamesWidget=.rqda$.codes_rqda,sortByTime=TRUE,decreasing = FALSE,...)
{
  if (isIdCurrent(.rqda$qdacon)){
  freecode <- dbGetQuery(.rqda$qdacon, "select name, id,date from freecode where status=1 order by lower(name)")
  codeName <- freecode$name
  if (nrow(freecode)!=0) {
    Encoding(codeName) <- "UTF-8"
    if (sortByTime){
      codeName <- codeName[OrderByTime(freecode$date,decreasing=decreasing)]
    }
  }
  tryCatch(CodeNamesWidget[] <- codeName, error=function(e){})
  } else gmessage("Cannot update Code List in the Widget. Project is closed already.\n",con=TRUE)
}

CodeNamesWidgetUpdate <- function(CodeNamesWidget=.rqda$.codes_rqda,sortByTime=TRUE,decreasing = FALSE,CodeId=NULL,...)
  ## CodeNamesWidgetUpdate is the alternative function of CodeNamesUpdate, should be used afterwards
{
  if (isIdCurrent(.rqda$qdacon)){
    freecode <- dbGetQuery(.rqda$qdacon, "select name, id,date from freecode where status=1 order by lower(name)")
    if (nrow(freecode)!=0) {
      if (!is.null(CodeId)) {freecode <- freecode[freecode$id %in% CodeId,]}
      codeName <- freecode$name
      Encoding(codeName) <- "UTF-8"
      if (sortByTime){
        codeName <- codeName[OrderByTime(freecode$date,decreasing=decreasing)]
      }
    }
    tryCatch(CodeNamesWidget[] <- codeName, error=function(e){})
  } else gmessage("Cannot update Code List in the Widget. Project is closed already.\n",con=TRUE)
}


mark <- function(widget,fore.col=.rqda$fore.col,back.col=NULL){
  ## modified so can change fore.col and back.col easily
  ## when col is NULL, it is skipped
  index <- sindex(widget)
  startI <- index$startI ## start and end iter
  endI <- index$endI
  selected <- index$seltext
  Encoding(selected) <- "UTF-8"
  startN <- index$startN # translate iter pointer to number
  endN <- index$endN
  if (startN != endN){
    buffer <- slot(widget,"widget")@widget$GetBuffer()
    TagTable <- buffer$GetTagTable()
    if (!is.null(fore.col)){
      if (is.null(TagTable$Lookup("MarkForeGround"))) {
      TagTable$Add(buffer$createTag("MarkForeGround",foreground = fore.col))
      }
      buffer$ApplyTagByName("MarkForeGround",startI,endI)
    }
    if (!is.null(back.col)){
      if (is.null(TagTable$Lookup("MarkBackGround"))) {
      TagTable$Add(buffer$createTag("MarkBackGround",background = back.col))
      }
      buffer$ApplyTagByName("MarkBackGround",startI,endI)
    }
    ## buffer$createTag("red.foreground",foreground = "red")
    ## buffer$ApplyTagByName("red.foreground",startI,endI)
    ## buffer$createTag("red.background",list(foreground = "red")) ## better, it can mark space
    ## buffer$ApplyTagByName("red.background",startI,endI); ## change colors   
  }
  ## only when selected text chunk is not "", apply the color scheme.
  return(list(start=startN,end=endN,text=selected))
}


ClearMark <- function(widget,min=0, max, clear.fore.col=TRUE,clear.back.col=FALSE){
  ## max position of marked text.
  tryCatch({
    buffer <- slot(widget,"widget")@widget$GetBuffer()
    startI <-gtkTextBufferGetIterAtOffset(buffer,min)$iter # translate number back to iter
    endI <-gtkTextBufferGetIterAtOffset(buffer,max)$iter
    TagTable <- buffer$GetTagTable()
    if (clear.fore.col && !is.null(TagTable$Lookup("MarkForeGround"))) gtkTextBufferRemoveTagByName(buffer,"MarkForeGround",startI,endI)
    if (clear.back.col && !is.null(TagTable$Lookup("MarkBackGround"))) gtkTextBufferRemoveTagByName(buffer,"MarkBackGround",startI,endI)
  },
           error=function(e){})
}


HL <- function(W,index,fore.col=.rqda$fore.col,back.col=NULL){
  ## W is the gtext widget of the text.
  ## highlight text chuck according to index
  ## index is a data frame, each row == one text chuck.
  buffer <- slot(W,"widget")@widget$GetBuffer()
  TagTable <- buffer$GetTagTable()
  if (!is.null(fore.col)){
    if (is.null(TagTable$Lookup("MarkForeGround"))) {
      TagTable$Add(buffer$createTag("MarkForeGround",foreground = fore.col))
    }
  }
  if (!is.null(back.col)){
    if (is.null(TagTable$Lookup("MarkBackGround"))) {
      TagTable$Add(buffer$createTag("MarkBackGround",background = back.col))
    }
  }
  tryCatch(
           apply(index,1, function(x){
             start <-gtkTextBufferGetIterAtOffset(buffer,x[1])$iter # translate number back to iter
             end <-gtkTextBufferGetIterAtOffset(buffer,x[2])$iter
             if (!is.null(fore.col)){
               buffer$ApplyTagByName("MarkForeGround",start,end)
             }
             if (!is.null(back.col)){
               buffer$ApplyTagByName("MarkBackGround",start,end)
             }
           }
                 ),
           error=function(e){})
}



sindex <- function(widget){
  buffer <- slot(widget,"widget")@widget$GetBuffer()
  bounds = buffer$GetSelectionBounds()
  startI = bounds$start ## start and end iter
  endI = bounds$end
  selected <- buffer$GetText(startI,endI)
  startN <- gtkTextIterGetOffset(startI) # translate iter pointer to number
  endN <- gtkTextIterGetOffset(endI)
  return(list(startI=startI,endI=endI,
              startN=startN,endN=endN,seltext=selected))
}



## retrieval <- function(CodeNameWidget=.rqda$.codes_rqda,type= c("unconditional", "case", "filecategory")){
## ##CodeNameWidget=.rqda$.codes_rqda
##   currentCode <- svalue(CodeNameWidget)
##   if (length(currentCode)!=0){
##   Encoding(currentCode) <- "UTF-8"
##   currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name== '%s' ",currentCode))[1,1]
##   ## reliable is more important
##   type=match.arg(type)
##   if (type=="unconditional"){
##     retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i",currentCid))
##   } else {
##     retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i and fid in (%s)",currentCid,paste(GetFileId(condition=type),collapse=",")))
##   }
##   if (nrow(retrieval)==0) gmessage("No Coding associated with the selected code.",con=TRUE) else {
##   retrieval <-  retrieval[order( retrieval$fid),]
##   fid <- unique(retrieval$fid)
##   retrieval$fname <-""
##   .gw <- gwindow(title=sprintf("Retrieved text: %s",currentCode),parent=c(395,10),width=600,height=600)
##   .retreivalgui <- gtext(con=.gw)
##   for (i in fid){
##     FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status==1 and id==%i",i))[['name']]
##     tryCatch(Encoding(FileName) <- "UTF-8",error=function(e){})
##     ##fname <- paste("Source: ", FileNames, sep="")
##     retrieval$fname[retrieval$fid==i] <- FileName
##   }
##   Encoding(retrieval$seltext) <-  Encoding(retrieval$fname) <- "UTF-8"
  
##   apply(retrieval,1, function(x){
##      metaData <- sprintf("%s [%s:%s]\n",x[['fname']],x[['selfirst']],x[['selend']])
##      add(.retreivalgui,metaData,font.attr=c(foreground="red",size="x-large"),do.newline=FALSE)
##      add(.retreivalgui,x[['seltext']],font.attr=c(style="normal",size="large"),do.newline=FALSE)
##      add(.retreivalgui,"\n\n",font.attr=c(style="normal",size="large"),do.newline=FALSE)
##    }
##         )
## }
##   }
## }

retrieval <- function(Fid=NULL,CodeNameWidget=.rqda$.codes_rqda){
  currentCode <- svalue(CodeNameWidget)
  if (length(currentCode)!=0){
    ## Encoding(currentCode) <- "UTF-8"
    currentCode <- enc(currentCode,"UTF-8")
    currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name== '%s' ",currentCode))[1,1]
    ## reliable is more important
  if (is.null(Fid)){
    retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i order by fid",currentCid))
  } else {
    retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i and fid in (%s)",currentCid,paste(Fid,collapse=",")))
   }
    if (nrow(retrieval)==0) gmessage("No Coding associated with the selected code.",con=TRUE) else {
      ## retrieval <-  retrieval[order( retrieval$fid),]
      ## use sql to order the fid
      fid <- unique(retrieval$fid)
      retrieval$fname <-""
      .gw <- gwindow(title=sprintf("Retrieved coding(s): %s",currentCode),parent=c(395,10),width=600,height=600)
      .retreivalgui <- gtext(con=.gw)
      for (i in fid){
        FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status==1 and id==%i",i))[['name']]
        tryCatch(Encoding(FileName) <- "UTF-8",error=function(e){})
        ##fname <- paste("Source: ", FileNames, sep="")
        retrieval$fname[retrieval$fid==i] <- FileName
      }
      Encoding(retrieval$seltext) <-  Encoding(retrieval$fname) <- "UTF-8"

      ## modification begins
        ComputeCallbackFun <- function(BeginPosition,EndPosition,FileName){
          CallBackFUN <- function(button){  
            tryCatch(dispose(.rqda$.root_edit),error=function(e) {})
            root <- gwindow(title=FileName, parent=c(395,40),width=580,height=580)
            ## use the same names as the of ViewFile, so can do coding when back to the original file.
            assign(".root_edit",root,env=.rqda)
            displayFile <- gtext(container=.rqda$.root_edit,font.attr=c(sizes="large"))
            assign(".openfile_gui",displayFile,env=.rqda)
            content <- dbGetQuery(.rqda$qdacon, sprintf("select file from source where name='%s'",FileName))[1,1]
            Encoding(content) <- "UTF-8" ## so it display correct in the gtext widget
            add(.rqda$.openfile_gui,content,font.attr=c(sizes="large"))
            HL(.rqda$.openfile_gui,data.frame(begin=BeginPosition,end=EndPosition))
            .rqda$.openfile_gui@widget@widget$SetEditable(FALSE)
            MarkHere <- .rqda$.openfile_gui@widget@widget$GetBuffer()$CreateMark(mark.name = "MarkHere", where=.rqda$.openfile_gui@widget@widget$GetBuffer()$GetIterAtOffset(BeginPosition)$iter)
            # create a mark -> more reliable to use ScrollToMark than ScrollToIter
            #gtkTextViewScrollToIter(.rqda$.openfile_gui@widget@widget,
            #                          .rqda$.openfile_gui@widget@widget$GetBuffer()$GetIterAtOffset(BeginPosition)$iter,
            #                          0.001,xal=0,yal=0,use.align=TRUE)## doesn't seem to work.
            gtkTextViewScrollToMark(.rqda$.openfile_gui@widget@widget,
                                      MarkHere,0,xal=0,yal=0.2,use.align=TRUE)
            }    
         CallBackFUN
        }

      buffer <- .retreivalgui@widget@widget$GetBuffer()
      iter <- buffer$getIterAtOffset(0)$iter
create.tags <- function(buffer)
{
buffer$createTag("big",size = 14 * PANGO_SCALE)
buffer$createTag("x-large",scale = PANGO_SCALE_X_LARGE)
buffer$createTag("large",scale = PANGO_SCALE_LARGE)
buffer$createTag("red.foreground",foreground = "red")
}
create.tags(buffer)

      apply(retrieval,1, function(x){
        metaData <- sprintf("%s [%s:%s]",x[['fname']],x[['selfirst']],x[['selend']])
        buffer$InsertWithTagsByName(iter, metaData,"x-large","red.foreground")
        anchorcreated <- buffer$createChildAnchor(iter)
        iter$BackwardChar()
        anchor <- iter$getChildAnchor()  
        widget <- gtkButtonNewWithLabel("Back")
        gSignalConnect(widget, "clicked", ComputeCallbackFun(x[['selfirst']],x[['selend']],x[['fname']]))
        .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
        widget$showAll()
        iter$ForwardChar()
        buffer$insert(iter, "\n")
        buffer$InsertWithTagsByName(iter, x[['seltext']],"large")
        buffer$insert(iter, "\n\n")
      }
            )
   buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
    }
  }
}

retrieval2 <- function(CodeNameWidget,type= c("unconditional", "case", "filecategory")){
## CodeNameWidget=.rqda$.codes_rqda for Codes Tab
## CodeNameWidget=.rqda$.CodeofCat for C-Cat Tab
  currentCode <- svalue(CodeNameWidget)
  if (length(currentCode)!=0){
    ## Encoding(currentCode) <- "UTF-8"
    currentCode <- enc(currentCode,"UTF-8")
    currentCid <- dbGetQuery(.rqda$qdacon,sprintf("select id from freecode where name== '%s' ",currentCode))[1,1]
    ## reliable is more important
  type=match.arg(type)
  if (type=="unconditional"){
    retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i order by fid",currentCid))
   } else {
    retrieval <- dbGetQuery(.rqda$qdacon,sprintf("select cid,fid, selfirst, selend,seltext from coding where status==1 and cid=%i and fid in (%s)",currentCid,paste(GetFileId(condition=type),collapse=",")))
   }
    if (nrow(retrieval)==0) gmessage("No Coding associated with the selected code.",con=TRUE) else {
      ## retrieval <-  retrieval[order( retrieval$fid),]
      ## use sql to order the fid
      fid <- unique(retrieval$fid)
      retrieval$fname <-""
      .gw <- gwindow(title=sprintf("Retrieved coding(s): %s",currentCode),parent=c(395,10),width=600,height=600)
      .retreivalgui <- gtext(con=.gw)
      for (i in fid){
        FileName <- dbGetQuery(.rqda$qdacon,sprintf("select name from source where status==1 and id==%i",i))[['name']]
        tryCatch(Encoding(FileName) <- "UTF-8",error=function(e){})
        ##fname <- paste("Source: ", FileNames, sep="")
        retrieval$fname[retrieval$fid==i] <- FileName
      }
      Encoding(retrieval$seltext) <-  Encoding(retrieval$fname) <- "UTF-8"

      ## modification begins
        ComputeCallbackFun <- function(BeginPosition,EndPosition,FileName){
          CallBackFUN <- function(button){  
            tryCatch(dispose(.rqda$.root_edit),error=function(e) {})
            root <- gwindow(title=FileName, parent=c(395,40),width=580,height=580)
            ## use the same names as the of ViewFile, so can do coding when back to the original file.
            assign(".root_edit",root,env=.rqda)
            displayFile <- gtext(container=.rqda$.root_edit,font.attr=c(sizes="large"))
            assign(".openfile_gui",displayFile,env=.rqda)
            content <- dbGetQuery(.rqda$qdacon, sprintf("select file from source where name='%s'",FileName))[1,1]
            Encoding(content) <- "UTF-8" ## so it display correct in the gtext widget
            add(.rqda$.openfile_gui,content,font.attr=c(sizes="large"))
            HL(.rqda$.openfile_gui,data.frame(begin=BeginPosition,end=EndPosition))
            .rqda$.openfile_gui@widget@widget$SetEditable(FALSE)
            MarkHere <- .rqda$.openfile_gui@widget@widget$GetBuffer()$CreateMark(mark.name = "MarkHere", where=.rqda$.openfile_gui@widget@widget$GetBuffer()$GetIterAtOffset(BeginPosition)$iter)
            # create a mark -> more reliable to use ScrollToMark than ScrollToIter
            #gtkTextViewScrollToIter(.rqda$.openfile_gui@widget@widget,
            #                          .rqda$.openfile_gui@widget@widget$GetBuffer()$GetIterAtOffset(BeginPosition)$iter,
            #                          0.001,xal=0,yal=0,use.align=TRUE)## doesn't seem to work.
            gtkTextViewScrollToMark(.rqda$.openfile_gui@widget@widget,
                                      MarkHere,0,xal=0,yal=0.2,use.align=TRUE)
            }    
         CallBackFUN
        }

      buffer <- .retreivalgui@widget@widget$GetBuffer()
      iter <- buffer$getIterAtOffset(0)$iter
create.tags <- function(buffer)
{
buffer$createTag("big",size = 14 * PANGO_SCALE)
buffer$createTag("x-large",scale = PANGO_SCALE_X_LARGE)
buffer$createTag("large",scale = PANGO_SCALE_LARGE)
buffer$createTag("red.foreground",foreground = "red")
}
create.tags(buffer)

      apply(retrieval,1, function(x){
        metaData <- sprintf("%s [%s:%s]",x[['fname']],x[['selfirst']],x[['selend']])
        buffer$InsertWithTagsByName(iter, metaData,"x-large","red.foreground")
        anchorcreated <- buffer$createChildAnchor(iter)
        iter$BackwardChar()
        anchor <- iter$getChildAnchor()  
        widget <- gtkButtonNewWithLabel("Back")
        gSignalConnect(widget, "clicked", ComputeCallbackFun(x[['selfirst']],x[['selend']],x[['fname']]))
        .retreivalgui@widget@widget$addChildAtAnchor(widget, anchor)
        widget$showAll()
        iter$ForwardChar()
        buffer$insert(iter, "\n")
        buffer$InsertWithTagsByName(iter, x[['seltext']],"large")
        buffer$insert(iter, "\n\n")
      }
            )
   buffer$PlaceCursor(buffer$getIterAtOffset(0)$iter)
    }
  }
}



ClickHandlerFun <- function(CodeNameWidget=.rqda$.codes_rqda){
  if (is_projOpen(env=.rqda,conName="qdacon")){
    ## CodeNamesUpdate(CodeNamesWidget=.rqda$.codes_rqda)
    con <- .rqda$qdacon
    SelectedCode <- currentCode <- svalue(CodeNameWidget)
if (length(SelectedCode)!=0) {
##Encoding(SelectedCode) <- Encoding(currentCode) <- "UTF-8"
SelectedCode <- currentCode <- enc(currentCode,encoding="UTF-8")
currentCid <- dbGetQuery(con,sprintf("select id from freecode where name=='%s'",SelectedCode))[,1]
SelectedFile <- tryCatch(svalue(.rqda$.root_edit)  ## use root_edit is more reliable
                         ,error=function(e){})
if (!is.null(SelectedFile)) {
  ## Encoding(SelectedFile) <- "UTF-8"
  SelectedFile <- enc(SelectedFile,encoding="UTF-8")
  currentFid <-  dbGetQuery(con,sprintf("select id from source where name=='%s'",SelectedFile))[,1]
  ## following code: Only mark the text chuck according to the current code.
  tryCatch({
    ## widget <- get(h$action$marktxtwidget,.rqda)
    widget <- .rqda$.openfile_gui
    ## if widget is not open, then error;which means no need to highlight anything.
    sel_index <-  dbGetQuery(con,sprintf("select selfirst, selend from coding where
                                                   cid==%i and fid==%i and status==1",currentCid, currentFid))
    Maxindex <- dbGetQuery(con, sprintf("select max(selend) from coding where fid==%i", currentFid))[1,1]
    ClearMark(widget,min=0,max=Maxindex,clear.fore.col = TRUE, clear.back.col =FALSE)
    if (nrow(sel_index)>0){
      HL(widget,index=sel_index,fore.col=.rqda$fore.col,back.col=NULL)}
  },error=function(e){}) # end of mark text chuck
}
}
}
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
            addhandlerdoubleclick(c2infoWidget,handler=function(h,...) retrieval2(CodeNameWidget=c2infoWidget))
            addHandlerClicked(c2infoWidget,handler <- function(h,...){ClickHandlerFun(CodeNameWidget=c2infoWidget)})
          }
        }}}

