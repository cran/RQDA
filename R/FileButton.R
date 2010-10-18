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


File_MemoButton <- function(label="Memo", container=.rqda$.files_button,FileWidget=.rqda$.fnames_rqda,...){
  ## memo of selected file.
  FilMemB <- gbutton(label, contain=container, handler=function(h,...) {
      MemoWidget("File",FileWidget,"source")
  }
          )
  assign("FilMemB",FilMemB,env=button)
  gtkWidgetSetSensitive(button$FilMemB@widget@widget,FALSE)
}

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
    if (exists(".AddNewFileWidget",env=.rqda) && isExtant(.rqda$.AddNewFileWidget)) {
      dispose(.rqda$.AddNewFileWidget)
    } ## close the widget if open
    gw <- gwindow(title="Add a new file", parent=getOption("widgetCoordinate"),
                  width = getOption("widgetSize")[1],
                  height = getOption("widgetSize")[2])
    mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
    gw@widget@widget$SetIconFromFile(mainIcon)
    assign(".AddNewFileWidget",gw,env=.rqda)
    assign(".AddNewFileWidget2",gpanedgroup(horizontal = FALSE, con=get(".AddNewFileWidget",env=.rqda)),env=.rqda)
    saveFileFun <- function() {
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
        if (is.null(ans)){
          svalue(textW) <- "" ## clear the content.
          FileNamesUpdate()
          enabled(button$AddNewFilB) <- FALSE
          enabled(button$AddNewFilB2) <- FALSE
      }
        return(TRUE)
    } else {
        return(FALSE)
    }
  } ## end of saveFileFun

    gl <- glayout(homogeneous=T,con=get(".AddNewFileWidget2",env=.rqda))
    AddNewFilB <- gbutton("Save To Project", handler=function(h,...){saveFileFun()})
    enabled(AddNewFilB) <- FALSE
    assign("AddNewFilB",AddNewFilB,env=button)
    AddNewFilB2 <- gbutton("Save and close", handler=function(h,...){
        suc <- saveFileFun()
        if (suc) dispose(.rqda$.AddNewFileWidget)
    }
                           )
    enabled(AddNewFilB2) <- FALSE
    assign("AddNewFilB2",AddNewFilB2,env=button)
    gl[1,1] <- AddNewFilB
    gl[1,2] <- AddNewFilB2
    tmp <- gtext(container=get(".AddNewFileWidget2",env=.rqda))
    font <- pangoFontDescriptionFromString(.rqda$font)
    gtkWidgetModifyFont(tmp@widget@widget,font) ## set the default fontsize
    assign(".AddNewFileWidgetW",tmp,env=.rqda)
    textW <- get(".AddNewFileWidgetW",env=.rqda)
    addHandlerKeystroke(.rqda$.AddNewFileWidgetW,handler=function(h,...){
      enabled(button$AddNewFilB) <- TRUE
      enabled(button$AddNewFilB2) <- TRUE
    })
    addhandlerunrealize(.rqda$.AddNewFileWidgetW,handler=function(h,...){
      rm("AddNewFilB",envir=button)
      rm(".AddNewFileWidgetW",".AddNewFileWidget",".AddNewFileWidget2",envir=.rqda)
      FALSE
    })
    ## svalue(.rqda$.AddNewFileWidget2) <- 0.03999
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
    if (exists(".openfile_gui",env=.rqda) && isExtant(.rqda$.openfile_gui)) {
        SearchButton(RQDA:::.rqda$.openfile_gui)
    }
}
## a=gtext("this is a test for search a.",con=T)
## b<-a@widget@widget$GetBuffer()
## b$GetIterAtOffset(0)
## i0=b$GetIterAtOffset(0)
## s0=i0$iter$ForwardSearch("a","GTK_TEXT_SEARCH_VISIBLE_ONLY")
## s0$match.start$GetOffset()
## s0$match.end$GetOffset()

FileNamesWidgetMenu$"File Memo"$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
 MemoWidget("File",.rqda$.fnames_rqda,"source")
## see CodeCatButton.R  for definition of MemoWidget
}
}
FileNamesWidgetMenu$"Open Selected File"$handler <- function(h,...){
  ViewFileFun(FileNameWidget=.rqda$.fnames_rqda)
}
FileNamesWidgetMenu$"Open Previous Coded File"$handler <- function(h,...){
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
FileNamesWidgetMenu$"Show ..."$"Show Files With Annotation"$handler <- function(h, ...) {
  fileid <- RQDAQuery("select fid from annotation where status==1 group by fid")$fid
  if (length(fileid)!=0) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
  } else gmessage("No file with memo.",con=TRUE)
}
FileNamesWidgetMenu$"Show ..."$"Show Files With Memo"$handler <- function(h, ...) {
    if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fileid <- dbGetQuery(.rqda$qdacon,"select id from source where memo is not null")
    if (nrow(fileid)!=0) {
    fileid <- fileid[[1]]
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
    } else gmessage("No file with memo.",con=TRUE)
    }
  }
FileNamesWidgetMenu$"Show ..."$"Show Files Without Annotation"$handler <- function(h, ...) {
  fileid <- RQDAQuery("select id from source where status==1 and id not in (select fid from annotation where status==1 group by fid)")$id
  if (length(fileid)!=0) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.fnames_rqda,FileId=fileid)
  } else gmessage("All files have annotation.",con=TRUE)
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
