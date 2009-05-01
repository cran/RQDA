#################
AddFileCatButton <- function(label="ADD"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      item <- ginput("Enter new File Category. ", icon="info")
      if (!is.na(item)){
        Encoding(item) <- "UTF-8"
        AddTodbTable(item,"filecat",Id="catid") ## FILE CATegory
        UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
      }
    }
  }
          )
}


DeleteFileCatButton <- function(label="Delete"){
  gbutton(label,
          handler=function(h,...)
          {
            if (is_projOpen(env=.rqda,conName="qdacon") &
                length(svalue(.rqda$.FileCatWidget))!=0) {
              del <- gconfirm("Really delete the File Category?",icon="question")
              if (isTRUE(del)){
                Selected <- svalue(.rqda$.FileCatWidget)
                Encoding(Selected) <- "UTF-8"
                catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status==1 and name=='%s'",Selected))[,1]
                if (length(catid) ==1){
                  dbGetQuery(.rqda$qdacon,sprintf("update filecat set status=0 where name=='%s'",Selected))
                  ## set status in table freecode to 0
                  UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
                  tryCatch(dbGetQuery(.rqda$qdacon,sprintf("update treefile set status=0 where catid=='%s'",catid)),error=function(e){}) 
                  ## should delete all the related codelists
                  UpdateFileofCatWidget() ## update the filecode of cat widget
                } else gmessage("The Category Name is not unique.",con=TRUE)
                
              }
            }
          }
          )
}


FileCat_RenameButton <- function(label="Rename",Widget=.rqda$.FileCatWidget,...)
{
  ## rename of selected file cat.
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,"qdacon")) {
      ## if project is open, then continue
      OldName <- svalue(Widget)
      if (length(OldName)==0){
        gmessage("Select a File Category first.",icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewName <- ginput("Enter new Cateory name. ",text=OldName, icon="info")
        if (NewName != "") {
          Encoding(NewName) <- "UTF-8"
          rename(OldName,NewName,"filecat")
          UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
        }
      }
    }
  }
          )
}

UpdateFileofCatWidget <- function(con=.rqda$qdacon,Widget=.rqda$.FileofCat){
  SelectedFileCat <- svalue(.rqda$.FileCatWidget)
  if (length(SelectedFileCat)!=0){
    Encoding(SelectedFileCat) <- "UTF-8"
    catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",SelectedFileCat))[,1]
    Total_fid <- dbGetQuery(con,sprintf("select fid from treefile where status==1 and catid==%i",catid))
    if (nrow(Total_fid)!=0){
      items <- dbGetQuery(con,"select name,id,date from source where status==1")
      if (nrow(items)!=0) {
        items <- items[items$id %in% Total_fid$fid,c("name","date")]
        items <- items$name[OrderByTime(items$date)] ## sort by date
        Encoding(items) <- "UTF-8"
      } else items <- NULL
    } else items <- NULL
  } else items <- NULL
    tryCatch(Widget[] <- items,error=function(e){})
}

FileCatAddToButton <- function(label="AddTo",Widget=.rqda$.FileCatWidget,...)
{
  gbutton(label,handler=function(h,...) {
    SelectedFileCat <- svalue(.rqda$.FileCatWidget)
    if (length(SelectedFileCat)==0) {gmessage("Select a file category first.",con=TRUE)} else{
    catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",SelectedFileCat))[,1]
    freefile <-  dbGetQuery(.rqda$qdacon,"select name, id from source where status=1")
    if (nrow(freefile) == 0){gmessage("No files Yet.",cont=.rqda$.FileCatWidget)} else {
    Encoding(SelectedFileCat) <- Encoding(freefile[['name']]) <- "UTF-8"
    fileofcat <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and catid=%i",catid))
    if (nrow(fileofcat)!=0){
    fileoutofcat <- subset(freefile,!(id %in% fileofcat$fid))
  } else  fileoutofcat <- freefile
    Selected <- gselect.list(fileoutofcat[['name']],multiple=TRUE)
    if (Selected != ""){
      ## Selected <- iconv(Selected,to="UTF-8") ## already Encoded as UTF-8.
      fid <- fileoutofcat[fileoutofcat$name %in% Selected,"id"]
      Dat <- data.frame(fid=fid,catid=catid,date=date(),dateM=date(),memo=NA,status=1)
      dbWriteTable(.rqda$qdacon,"treefile",Dat,row.names=FALSE,append=TRUE)
      UpdateFileofCatWidget()
    }
   ## CurrentFrame <- sys.frame(sys.nframe())
    ## sys.frame(): get the frame of n
    ## nframe(): get n of current frame
    ## The value of them depends on where they evaluated, should not placed inside RunOnSelected()
    ##RunOnSelected(fileoutofcat[['name']],multiple=TRUE,expr={
    ##if (length(Selected)!=0){
    ## Selected <- iconv(Selected,to="UTF-8")
    ## fid <- fileoutofcat[fileoutofcat$name %in% Selected,"id"]
    ##Dat <- data.frame(fid=fid,catid=catid,date=date(),dateM=date(),memo="",status=1)
    ##dbWriteTable(.rqda$qdacon,"treefile",Dat,row.names=FALSE,append=TRUE)
    ##UpdateFileofCatWidget()
    ##}},enclos=CurrentFrame)
  }
  }
  }
          )
}

FileCatDropFromButton <- function(label="DropFrom",Widget=.rqda$.FileofCat,...)
{
  gbutton(label,handler=function(h,...) {
    FileOfCat <- svalue(Widget)
    if ((NumofSelected <- length(FileOfCat)) ==0) {
      gmessage("Please select the Files you want to delete.",con=TRUE)} else
    {
      ## Give a confirm msg
      del <- gconfirm(sprintf("Delete %i file(s) from this category. Are you sure?",NumofSelected),con=TRUE,icon="question")
      if (isTRUE(del)){
        SelectedFileCat <- svalue(.rqda$.FileCatWidget)
        Encoding(SelectedFileCat) <- Encoding(FileOfCat)<- "UTF-8"
        catid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where status=1 and name='%s'",SelectedFileCat))[,1]
    for (i in FileOfCat){
      fid <- dbGetQuery(.rqda$qdacon,sprintf("select id from source where status=1 and name='%s'",i))[,1]
      dbGetQuery(.rqda$qdacon,sprintf("update treefile set status==0 where catid==%i and fid==%i",catid,fid))
    }
        ## update .CodeofCat Widget
        .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],FileOfCat)
        ## UpdateFileofCatWidget()
      }
    }
  }
          )
}



## AddToFileCategory<- function(){
## moved to FilesFun.R
##   ## filenames -> fid -> selfirst=0; selend=nchar(filesource)
##   filename <- svalue(.rqda$.fnames_rqda)
##   Encoding(filename) <- "unknown"
##   query <- dbGetQuery(.rqda$qdacon,sprintf("select id, file from source where name in(%s) and status=1",paste("'",filename,"'",sep="",collapse=","))) ## multiple fid
##   fid <- query$id
##   Encoding(query$file) <- "UTF-8"
  
##   ## select a F-cat name -> F-cat id
##   Fcat <- dbGetQuery(.rqda$qdacon,"select catid, name from filecat where status=1")
##   if (nrow(Fcat)==0){gmessage("Add File Categroy first.",con=TRUE)} else{
##     Encoding(Fcat$name) <- "UTF-8"
##     ##ans <- select.list(Fcat$name,multiple=FALSE)
##     CurrentFrame <- sys.frame(sys.nframe())
##     RunOnSelected(Fcat$name,multiple=TRUE,enclos=CurrentFrame,expr={
##     if (Selected!=""){ ## must use Selected to represent the value of selected items. see RunOnSelected() for info.
##       Selected <- iconv(Selected,to="UTF-8")
##       Fcatid <- Fcat$catid[Fcat$name %in% Selected]
##       exist <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and fid in (%s) and catid=%i",paste("'",fid,"'",sep="",collapse=","),Fcatid))
##     if (nrow(exist)!=length(fid)){
##     ## write only when the selected file associated with specific f-cat is not there
##       DAT <- data.frame(fid=fid[!fid %in% exist$fid], catid=Fcatid, date=date(),dateM=date(),memo='',status=1)
##       ## should pay attention to the var order of DAT, must be the same as that of treefile table
##       success <- dbWriteTable(.rqda$qdacon,"treefile",DAT,row.name=FALSE,append=TRUE)
##       ## write to caselinkage table
##       if (success) {
##       UpdateFileofCatWidget()
##       }
##       if (!success) gmessage("Fail to write to database.")
##     }}})}}



FileCatWidgetMenu <- list()
FileCatWidgetMenu$Memo$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
   MemoWidget("FileCat",.rqda$.FileCatWidget,"filecat")
   ## see CodeCatButton.R  for definition of MemoWidget
 }
}
FileCatWidgetMenu$"Delete all files of selected category"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    fid <- GetFileId("file")
    if (length(fid)>0){
      dbGetQuery(.rqda$qdacon,sprintf("update source set status=0 where id in (%s)",paste(shQuote(fid),collapse=",")))
    }
  }
}
FileCatWidgetMenu$"Sort by created time"$handler <- function(h,...)
{
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
  }
}


## popup menu for files of this category
FileofCatWidgetMenu <- list()
FileofCatWidgetMenu$"Add To Case ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
      AddFileToCaselinkage(Widget=.rqda$.FileofCat)
      UpdateFileofCaseWidget()
    }
}
FileofCatWidgetMenu$"Add To File Category ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    AddToFileCategory(Widget=.rqda$.FileofCat,updateWidget=FALSE)
  }
}
FileofCatWidgetMenu$"Move To File Category ..."$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    fcatname <- svalue(.rqda$.FileCatWidget) ## should select one only
    fcatid <- dbGetQuery(.rqda$qdacon,sprintf("select catid from filecat where name='%s'",fcatname))$catid
    fid <- GetFileId("file","select")
    AddToFileCategory(Widget=.rqda$.FileofCat,updateWidget=FALSE)
    dbGetQuery(.rqda$qdacon,sprintf("update treefile set status=0 where fid in (%s) and catid='%s'",
                                    paste(shQuote(fid),collapse=","),
                                    fcatid))
    .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],svalue(.rqda$.FileofCat))
  }
}
FileofCatWidgetMenu$"File Memo"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    MemoWidget("File",.rqda$.FileofCat,"source")
  }
}
FileofCatWidgetMenu$"Open Selected File"$handler <- function(h,...){
  ViewFileFun(FileNameWidget=.rqda$.FileofCat)
}
FileofCatWidgetMenu$"Delete selected File"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    SelectedFile <- svalue(.rqda$.FileofCat)
    Encoding(SelectedFile) <- "UTF-8"
    for (i in SelectedFile){
      fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",i))$id
      dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",i))
      dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
      dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
    }
    ## UpdateFileofCatWidget()
    .rqda$.FileofCat[] <- setdiff(.rqda$.FileofCat[],SelectedFile)
  }
}
## FileofCatWidgetMenu$"Delete selected File Without Updating Widget"$handler <- function(h,...){
##   if (is_projOpen(env=.rqda,conName="qdacon")) {
##     SelectedFile <- svalue(.rqda$.FileofCat)
##     Encoding(SelectedFile) <- "UTF-8"
##     for (i in SelectedFile){
##       fid <- dbGetQuery(.rqda$qdacon, sprintf("select id from source where name='%s'",i))$id
##       dbGetQuery(.rqda$qdacon, sprintf("update source set status=0 where name='%s'",i))
##       dbGetQuery(.rqda$qdacon, sprintf("update caselinkage set status=0 where fid=%i",fid))
##       dbGetQuery(.rqda$qdacon, sprintf("update treefile set status=0 where fid=%i",fid))
##     }
##   }
## }
FileofCatWidgetMenu$"Rename selected File"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    selectedFN <- svalue(.rqda$.FileofCat)
    if (length(selectedFN)==0){
      gmessage("Select a file first.",icon="error",con=TRUE)
    }
    else {
      NewFileName <- ginput("Enter new file name. ",text=selectedFN, icon="info")
      if (!is.na(NewFileName)) {
        Encoding(NewFileName) <- "UTF-8"
        rename(selectedFN,NewFileName,"source")
        ## UpdateFileofCatWidget()
        Fnames <- .rqda$.FileofCat[]
        Fnames[Fnames==selectedFN] <- NewFileName
        .rqda$.FileofCat[] <- Fnames
      }
    }}}
## FileofCatWidgetMenu$"Show ..."$"Show Uncoded Files Only Sorted By Imported Time"$handler <- function(h,...){
##  if (is_projOpen(env=.rqda,conName="qdacon")) {
##    fid <- GetFileId(condition="filecategory",type="uncoded")
##    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
##  }
## }
## FileofCatWidgetMenu$"Show ..."$"Show Coded Files Only Sorted By Imported Time"$handler <- function(h,...){
##   if (is_projOpen(env=.rqda,conName="qdacon")) {
##     fid <- GetFileId(condition="filecategory",type="coded")
##     FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
##   }
## }
FileofCatWidgetMenu$"Show ..."$"Show All By Imported Time"$handler <- function(h,...)
{
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="filecategory",type="all")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
  }
}
FileofCatWidgetMenu$"Show ..."$"Show Coded Files Sorted by Imported time"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=GetFileId(condition="filecat",type="coded"))
  }
}
FileofCatWidgetMenu$"Show ..."$"Show Uncoded Files Sorted by Imported time"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    ## UncodedFileNamesUpdate(FileNamesWidget = .rqda$.fnames_rqda)
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=GetFileId(condition="filecat",type="uncoded"))
    ## By default, the file names in the widget will be sorted.
  }
}
FileofCatWidgetMenu$"Show Selected File Property"$handler <- function(h, ...) {
  if (is_projOpen(env = .rqda, conName = "qdacon", message = FALSE)) {
    Fid <- GetFileId("file","selected")
    Fcat <- RQDAQuery(sprintf("select name from filecat where catid in (select catid from treefile where fid=%i and status=1) and status=1",Fid))$name
    Case <- RQDAQuery(sprintf("select name from cases where id in (select caseid from caselinkage where fid=%i and status=1) and status=1",Fid))$name
    if (!is.null(Fcat)) Encoding(Fcat) <- "UTF-8"
    if (!is.null(Case)) Encoding(Case) <- "UTF-8"
    glabel(sprintf(" File ID is %i \n File Category is %s\n Case is %s",
                   Fid,paste(shQuote(Fcat),collapse=", "),paste(shQuote(Case),collapse=", ")),cont=TRUE)
  }
}

