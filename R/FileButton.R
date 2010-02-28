ImportFileButton <- function(label="Import", container,...)
{
  ImpFilB <- gbutton(label, contain=container, handler=function(h,...){
      path <- gfile(type="open",filter=list("text files" = list(mime.types = c("text/plain")),
              "All files" = list(patterns = c("*"))))
      if (path!=""){
        Encoding(path) <- "UTF-8" ## have to convert, otherwise, can not find the file.
        ImportFile(path,con=.rqda$qdacon)
         FileNamesUpdate()
      }
    }
          )
  assign("ImpFilB",ImpFilB,env=button)
  gtkWidgetSetSensitive(button$ImpFilB@widget@widget,FALSE)  
}


DeleteFileButton <- function(label="Delete", container,...){
  DelFilB <- gbutton(label,contain=container,handler=function(h,...){
              SelectedFile <- svalue(.rqda$.fnames_rqda)
              Encoding(SelectedFile) <- "UTF-8"
              ## if the project open and a file is selected, then continue the action
              del <- gconfirm(ngettext(length(SelectedFile),
                                       "Really delete the file?",
                                       "Really delete the files?")
                                       ,icon="question")
              if (isTRUE(del)) {
                ## con <- .rqda$qdacon
                  for (i in SelectedFile){
                    i <- enc(i)
                fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",i))$id
                dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",i))
                ## set the status of the selected file to 0
                dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
                dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
                dbGetQuery(.rqda$qdacon, sprintf("update coding set status=0 where fid=%i",fid))
                ## set the status of the related case/f-cat to 0
                }
                  ##FileNamesUpdate()
                  UpdateWidget(".fnames_rqda",from=SelectedFile,to=NULL)
              }
          },
          action=list(env=.rqda,conName="qdacon")
          )
  assign("DelFilB",DelFilB,env=button)
  gtkWidgetSetSensitive(button$DelFilB@widget@widget,FALSE) 
}

ViewFileButton <-  function(label="Open", container,...)
{
  VieFilB <- gbutton(label,contain=container,h=function(h,...)
          {
            ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
          }
          )
  assign("VieFilB",VieFilB,env=button)
  gtkWidgetSetSensitive(button$VieFilB@widget@widget,FALSE) 
}
##           {
##             if (is_projOpen(env=.rqda,conName="qdacon")) {
##               if (length(svalue(.rqda$.fnames_rqda))==0){gmessage("Select a file first.",icon="error",con=TRUE)}
##               else {
##                 tryCatch(dispose(.rqda$.root_edit),error=function(e) {})
##                 ## notice the error handler
##                 SelectedFileName <- svalue(.rqda$.fnames_rqda)
##                 assign(".root_edit",gwindow(title=SelectedFileName, parent=c(370,10),width=600,height=600),env=.rqda)
##                 .root_edit <- get(".root_edit",.rqda)
##                 assign(".openfile_gui",gtext(container=.root_edit,font.attr=c(sizes="large")),env=.rqda)
##                 Encoding(SelectedFileName) <- "unknown"
##                 content<-dbGetQuery(.rqda$qdacon, sprintf("select file from source where name='%s'",SelectedFileName))[1,1]
##                 Encoding(content) <- "UTF-8" ## so it display correct in the gtext widget
##                 ## turn data.frame to 1-length character.
##                 W <- get(".openfile_gui",.rqda)
##                 add(W,content,font.attr=c(sizes="large"))
##                 slot(W,"widget")@widget$SetEditable(FALSE)
##                 ## make sure it is read only file in the text window.
##               }
##     }
##           }
##           )
## }


File_MemoButton <- function(label="Memo", container=.rqda$.files_button,FileWidget=.rqda$.fnames_rqda,...){
  ## memo of selected file.
  FilMemB <- gbutton(label, contain=container, handler=function(h,...) {
      MemoWidget("File",FileWidget,"source")
  }
          )
  assign("FilMemB",FilMemB,env=button)
  gtkWidgetSetSensitive(button$FilMemB@widget@widget,FALSE) 
}

##     if (is_projOpen(env=.rqda,"qdacon")) {
##       ## if project is open, then continue
##       selectedFN <- svalue(FileWidget) ## svalue(.fnames_rqda) is the name of selected file.
##       if (length(selectedFN)==0){
##         ## if no file is selected, then no need to memo.
##         gmessage("Select a file first.",icon="error",con=TRUE)
##       }
##       else {
##         tryCatch(dispose(.rqda$.filememo),error=function(e) {})
##         ## Close the open file memo first, then open a new one
##         ## .filememo is the container of .fmemocontent,widget for the content of memo
##         assign(".filememo",gwindow(title=paste("File Memo",selectedFN,sep=":"),
##                                    parent=c(370,10),width=600,height=400),env=.rqda)
##         .filememo <- .rqda$.filememo
##         .filememo2 <- gpanedgroup(horizontal = FALSE, con=.filememo)
##         ## use .filememo2, so can add a save button to it.
##         gbutton("Save memo",con=.filememo2,handler=function(h,...){
##           ## send the new content of memo back to database
##           newcontent <- svalue(W)
##           Encoding(newcontent) <- "UTF-8"
##           newcontent <- enc(newcontent) ## take care of double quote.
##           dbGetQuery(.rqda$qdacon,sprintf("update source set memo='%s' where name='%s'",newcontent,selectedFN))
##                                  ## have to quote the character in the sql expression
##         }
##                 )
##         assign(".fmemocontent",gtext(container=.filememo2,font.attr=c(sizes="large")),env=.rqda)
##         prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select memo from source where name='%s'",svalue(FileWidget)))[1,1]
##         ## [1,1]turn data.frame to 1-length character. Existing content of memo
##         if (is.na(prvcontent)) prvcontent <- ""
##         Encoding(prvcontent) <- "UTF-8" ## important
##         W <- .rqda$.fmemocontent
##         add(W,prvcontent,font.attr=c(sizes="large"),do.newline=FALSE)
##         ## push the previous content to the widget.
##       }
##     }
##   }
##           )
## }



File_RenameButton <- function(label="Rename", container=.rqda$.files_button,FileWidget=.rqda$.fnames_rqda,...)
{
  ## rename of selected file.
  FilRenB <- gbutton(label, contain=container, handler=function(h,...) {
      selectedFN <- svalue(FileWidget)
      if (length(selectedFN)==0){
        gmessage("Select a file first.",icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewFileName <- ginput("Enter new file name. ",text=selectedFN, icon="info")
        if (!is.na(NewFileName)) {
          Encoding(NewFileName) <- "UTF-8"
          ## otherwise, R transform it into local Encoding rather than keep it as UTF-8
          ## Newfilename <- iconv(codename,from="UTF-8") ## now use UTF-8 for SQLite data set.
          ## update the name in source table by a function
          rename(selectedFN,NewFileName,"source")
          ## FileNamesUpdate()
          UpdateWidget(".fnames_rqda",from=selectedFN,to=NewFileName) ## speed it up by bypassing access the database.
          ## (name is the only field should be modifed, as other table use fid rather than name)
        }
      }
    }
          )
  FilRenB
  assign("FilRenB",FilRenB,env=button)
  gtkWidgetSetSensitive(button$FilRenB@widget@widget,FALSE) 
}


AddNewFileFun <- function(){
  if (is_projOpen(env=.rqda,"qdacon")) {
    tryCatch(eval(parse(text="dispose(.rqda$.AddNewFileWidget")),error=function(e) {}) ## close the widget if open
    gw <- gwindow(title="Add New File.",parent=getOption("widgetCoordinate"),width=600,height=400)
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw@widget@widget$SetIconFromFile(mainIcon)
    assign(".AddNewFileWidget",gw,env=.rqda)
    assign(".AddNewFileWidget2",gpanedgroup(horizontal = FALSE, con=get(".AddNewFileWidget",env=.rqda)),env=.rqda)
    gbutton("Save To Project",con=get(".AddNewFileWidget2",env=.rqda),handler=function(h,...){
      ## require a title for the file
      Ftitle <- ginput("Enter the title", icon="info")
      if (!is.na(Ftitle)) {
        Ftitle <- enc(Ftitle,"UTF-8")
        if (nrow(dbGetQuery(.rqda$qdacon,sprintf("select name from source where name=='%s'",Ftitle)))!=0) {
          Ftitle <- paste("New",Ftitle)
        }## Make sure it is unique
        content <- svalue(textW)
        content <- enc(content,encoding="UTF-8") ## take care of double quote.
        maxid <- dbGetQuery(.rqda$qdacon,"select max(id) from source")[[1]] ## the current one
        nextid <- ifelse(is.na(maxid),0+1, maxid+1) ## the new one/ for the new file
        ans <- dbGetQuery(.rqda$qdacon,sprintf("insert into source (name, file, id, status,date,owner )
                             values ('%s', '%s',%i, %i, '%s', '%s')",
                                               Ftitle,content, nextid, 1,date(),.rqda$owner))
        ## write to the data-base ## what is ans?
        ## rm(.AddNewFileWidget,.AddNewFileWidget2,env=.rqda)
        ## delete .rqda$.AddNewFileWidget and .rqda$.AddNewFileWidget2
        gmessage("Succeed.",con=T)
        FileNamesUpdate()
      }}
            )## end of save button
    tmp <- gtext(container=get(".AddNewFileWidget2",env=.rqda))
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp@widget@widget,font)## set the default fontsize
    assign(".AddNewFileWidgetW",tmp,env=.rqda)
    textW <- get(".AddNewFileWidgetW",env=.rqda)
  }
}


## pop-up menu of add to case and F-cat from Files Tab
FileNamesWidgetMenu <- list()
FileNamesWidgetMenu$"Add New File ..."$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    AddNewFileFun()
    }
  }
FileNamesWidgetMenu$"Add To Case ..."$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      AddFileToCaselinkage()
      UpdateFileofCaseWidget()
    }
  }
FileNamesWidgetMenu$"Add To File Category ..."$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      AddToFileCategory()
      UpdateFileofCatWidget()
    }
  }
FileNamesWidgetMenu$"Add/modify Attributes of Selected File..."$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    Selected <- svalue(.rqda$.fnames_rqda)
    if (length(Selected !=0 )){
    fileId <- RQDAQuery(sprintf("select id from source where status=1 and name='%s'",
                                enc(Selected)))[,1]
    FileAttrFun(fileId=fileId,title=Selected)
  }
}}
FileNamesWidgetMenu$"Add/modify Attributes of The Open File..."$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    Selected <- tryCatch(svalue(RQDA:::.rqda$.root_edit),error=function(e){NULL})
    if (!is.null(Selected)){
      fileId <- RQDAQuery(sprintf("select id from source where status=1 and name='%s'",
                                  enc(Selected)))[,1]
    FileAttrFun(fileId=fileId,title=Selected)
  }
}}
FileNamesWidgetMenu$"View Attributes"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
   viewFileAttr()
  }
}
FileNamesWidgetMenu$"Edit Seleted File"$handler <- function(h, ...) {
  EditFileFun()
}
FileNamesWidgetMenu$"Find a word..."$handler <- function(h, ...) {
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    content <- tryCatch(svalue(.rqda$.openfile_gui),error=function(e){NULL})
    if (!is.null(content)) {
        fname <- svalue(RQDA:::.rqda$.root_edit)
        fid <- RQDAQuery(sprintf("select id from source where name=='%s' and status==1",fname))$id
        word <- ginput("Type the word you intend to find.",con=TRUE)
        Encoding(content) <- Encoding(word) <- "UTF-8"
        idx1 <- gregexpr(word,content)[[1]] -1
        idx2 <- idx1 + attr(idx1,"match.length")
        markidx <- RQDAQuery(sprintf("select coding.selfirst,coding.selend from coding,freecode where coding.fid=%i and coding.status=1 and freecode.id==coding.cid and freecode.status==1",fid))
        anno <- RQDAQuery(sprintf("select position,rowid from annotation where status==1 and fid==%s",fid))
        allidx <- c(unlist(markidx),anno)
        if (!is.null(allidx)){
            idx1 <- idx1 + apply(outer(allidx,idx1,"<="),2,sum)
            idx2 <- idx2 + apply(outer(allidx,idx2,"<="),2,sum)
        }
        idx <- data.frame(idx1,idx2)
        ClearMark(.rqda$.openfile_gui,0,nchar(content),FALSE,TRUE)
        HL(.rqda$.openfile_gui,idx,NULL,"yellow")
    }
}
}
FileNamesWidgetMenu$"File Memo"$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
 MemoWidget("File",.rqda$.fnames_rqda,"source")
## see CodeCatButton.R  for definition of MemoWidget
}
}
FileNamesWidgetMenu$"Open Selected File"$handler <- function(h,...){
  ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
}
FileNamesWidgetMenu$"Open Last Coded File"$handler <- function(h,...){
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fname <- RQDAQuery("select name from source where id in ( select fid from coding where rowid in (select max(rowid) from coding where status==1))")$name
    if (length(fname)!=0)  fname <- enc(fname,"UTF-8")
    ViewFileFunHelper(FileName=fname)
  }}
FileNamesWidgetMenu$"Search Files..."$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    pattern <- ifelse(is.null(.rqda$lastsearch),"file like '%%'",.rqda$lastsearch)
    pattern <- ginput("Please input a search pattern.",text=pattern)
    if (!is.na(pattern)){
    tryCatch(SearchFiles(pattern,Widget=".fnames_rqda",is.UTF8=TRUE),error=function(e) gmessage("Error~~~."),con=TRUE)
    Encoding(pattern) <- "UTF-8"
    assign("lastsearch",pattern,env=.rqda)
    }
    }
  }
FileNamesWidgetMenu$"Show ..."$"Show All Sorted By Imported Time"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
     ##FileNamesUpdate(FileNamesWidget=.rqda$.fnames_rqda)
     FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="all"))
    }
  }
FileNamesWidgetMenu$"Show ..."$"Show Coded Files Sorted by Imported time"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="coded"))
  }
}
FileNamesWidgetMenu$"Show ..."$"Show Uncoded Files Sorted by Imported time"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      ## UncodedFileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
      FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="uncoded"))
      ## By default, the file names in the widget will be sorted.
    }
  }
## FileNamesWidgetMenu$"Show ..."$"Show Last Coded File"$handler <- function(h, ...) {
##     if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
##         fname <- RQDAQuery("select name from source where id in ( select fid from coding where rowid in (select max(rowid) from coding where status==1))")$name
##         if (length(fname)!=0)  fname <- enc(fname,"UTF-8")
##         .rqda$.fnames_rqda[] <- fname
##     }}
FileNamesWidgetMenu$"Show ..."$"Show Files With Memo"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fileid <- dbGetQuery(.rqda$qdacon,"select id from source where memo is not null")
    if (nrow(fileid)!=0) {
    fileid <- fileid[[1]]
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
    } else gmessage("No file with memo.",con=TRUE)
    }
  }
FileNamesWidgetMenu$"Show ..."$"Show Files Without Memo"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fileid <- dbGetQuery(.rqda$qdacon,"select id from source where memo is null")
    if (nrow(fileid)!=0) {
    fileid <- fileid[[1]]
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
    } else gmessage("No file is found.",con=TRUE)
    }
  }
FileNamesWidgetMenu$"Show ..."$"Show Files Without File Category"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
        fileid <- RQDAQuery("select id from source where status==1 and id not in (select fid from treefile where status==1)")
        if (nrow(fileid)!=0) {
            fileid <- fileid[[1]]
            FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
        } else gmessage("All are linked with file category.",con=TRUE)
    }
}
FileNamesWidgetMenu$"Show Selected File Property"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
  ShowFileProperty()
}}
