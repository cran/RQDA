ImportFileButton <- function(label="Import", container,...)
{
  gbutton(label, contain=container, handler=function(h,...){
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      path <- gfile(type="open",filter=list("text files" = list(mime.types = c("text/plain")),
              "All files" = list(patterns = c("*"))))
      if (path!=""){
        Encoding(path) <- "UTF-8" ## have to convert, otherwise, can not find the file.
        ImportFile(path,con=.rqda$qdacon)
         FileNamesUpdate()
      }
    }
  }
          )
}


DeleteFileButton <- function(label="Delete", container,...){
  gbutton(label,contain=container,handler=function(h,...)
          {
            if (is_projOpen(env=.rqda,conName="qdacon") & length(svalue(.rqda$.fnames_rqda))!=0) {
              ## if the project open and a file is selected, then continue the action
              del <- gconfirm("Really delete the file?",icon="question")
              if (isTRUE(del)) {
                con <- .rqda$qdacon
                SelectedFile <- svalue(.rqda$.fnames_rqda)
                Encoding(SelectedFile) <- "UTF-8"
                fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",SelectedFile))$id
                dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",SelectedFile))
                ## set the status of the selected file to 0
                dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
                dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
                ## set the status of the related case/f-cat to 0
                FileNamesUpdate()
              }
            }
          },
          action=list(env=.rqda,conName="qdacon")
          )
}

ViewFileButton <-  function(label="Open", container,...)
{
  gbutton(label,contain=container,h=function(h,...)
          {
            ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
          }
          )
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


File_MemoButton <- function(label="F-Memo", container=.rqda$.files_button,FileWidget=.rqda$.fnames_rqda,...){
  ## memo of selected file.
  gbutton(label, contain=container, handler=function(h,...) {
    if (is_projOpen(env=.rqda,"qdacon")) {
      MemoWidget("File",FileWidget,"source")
    }
  }
          )
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
  gbutton(label, contain=container, handler=function(h,...) {
    if (is_projOpen(env=.rqda,"qdacon")) {
      ## if project is open, then continue
      selectedFN <- svalue(FileWidget)
      if (length(selectedFN)==0){
        gmessage("Select a file first.",icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewFileName <- ginput("Enter new file name. ",text=selectedFN, icon="info")
        if (NewFileName != "") {
          Encoding(NewFileName) <- "UTF-8"
          ## otherwise, R transform it into local Encoding rather than keep it as UTF-8
          ## Newfilename <- iconv(codename,from="UTF-8") ## now use UTF-8 for SQLite data set.
          ## update the name in source table by a function
          rename(selectedFN,NewFileName,"source")
          FileNamesUpdate()
          ## (name is the only field should be modifed, as other table use fid rather than name)
        }
      }
    }
  }
          )
}



## pop-up menu of add to case and F-cat from Files Tab
FileNamesWidgetMenu <- list()
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
FileNamesWidgetMenu$"File Memo"$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
 MemoWidget("File",.rqda$.fnames_rqda,"source")
## see CodeCatButton.R  for definition of MemoWidget
}
}
FileNamesWidgetMenu$"Open Selected File"$handler <- function(h,...){
  ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
}
FileNamesWidgetMenu$"Search Files..."$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    pattern <- ginput("Please input a search pattern.",text="file like '%%'")
    if (pattern!=""){
    tryCatch(SearchFiles(pattern,Widget=.rqda$.fnames_rqda,is.UTF8=TRUE),error=function(e) gmessage("Error~~~."),con=TRUE)
    }
    }
  }
FileNamesWidgetMenu$"Show Uncoded Files Only (Sorted)"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      ## UncodedFileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
      FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="uncoded"))
      ## By default, the file names in the widget will be sorted.
    }
  }
FileNamesWidgetMenu$"Show Coded Files Only (Sorted)"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="coded"))
  }
}
FileNamesWidgetMenu$"Sort All By Imported Time"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
     ##FileNamesUpdate(FileNamesWidget=.rqda$.fnames_rqda)
     FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=GetFileId(condition="unconditional",type="all"))
    }
  }
