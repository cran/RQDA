#################
AddFileCatButton <- function(label="ADD"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      item <- ginput("Enter new File Category. ", icon="info")
      if (item != ""){
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
    Encoding(SelectedFileCat) <- Encoding(freefile[['name']]) <- "UTF-8"
    fileofcat <- dbGetQuery(.rqda$qdacon,sprintf("select fid from treefile where status=1 and catid=%i",catid))
    if (nrow(fileofcat)!=0){
    fileoutofcat <- subset(freefile,!(id %in% fileofcat$fid))
  } else  fileoutofcat <- freefile
##    Selected <- select.list(fileoutofcat[['name']],multiple=TRUE)
##     if (length(Selected)!=0){
##       Selected <- iconv(Selected,to="UTF-8")
##       fid <- fileoutofcat[fileoutofcat$name %in% Selected,"id"]
##       Dat <- data.frame(fid=fid,catid=catid,date=date(),dateM=date(),memo="",status=1)
##       dbWriteTable(.rqda$qdacon,"treefile",Dat,row.names=FALSE,append=TRUE)
##       UpdateFileofCatWidget()
##     }
    CurrentFrame <- sys.frame(sys.nframe())
    ## sys.frame(): get the frame of n
    ## nframe(): get n of current frame
    ## The value of them depends on where they evaluated, should not placed inside RunOnSelected()
    RunOnSelected(fileoutofcat[['name']],multiple=TRUE,expr={
    if (length(Selected)!=0){
      Selected <- iconv(Selected,to="UTF-8")
      fid <- fileoutofcat[fileoutofcat$name %in% Selected,"id"]
      Dat <- data.frame(fid=fid,catid=catid,date=date(),dateM=date(),memo="",status=1)
      dbWriteTable(.rqda$qdacon,"treefile",Dat,row.names=FALSE,append=TRUE)
      UpdateFileofCatWidget()
    }},enclos=CurrentFrame)
    
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
        UpdateFileofCatWidget()
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
FileCatWidgetMenu$"Sort by created time"$handler <- function(h,...)
{
 if (is_projOpen(env=.rqda,conName="qdacon")) {
 UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat")
 }
}

## popup menu for files of this category
FileofCatWidgetMenu <- list()
FileofCatWidgetMenu$"File Memo"$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
 MemoWidget("File",.rqda$.FileofCat,"source")
}
}
FileofCatWidgetMenu$"Open Selected File"$handler <- function(h,...){
ViewFileFun(FileNameWidget=.rqda$.FileofCat)
}
FileofCatWidgetMenu$"Show Uncoded Files Only (Sorted)"$handler <- function(h,...){
 if (is_projOpen(env=.rqda,conName="qdacon")) {
   fid <- GetFileId(condition="filecategory",type="uncoded")
   FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
 }
}
FileofCatWidgetMenu$"Show Coded Files Only (Sorted)"$handler <- function(h,...){
  if (is_projOpen(env=.rqda,conName="qdacon")) {
    fid <- GetFileId(condition="filecategory",type="coded")
    FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
  }
}
## FileofCatWidgetMenu$"Sort All By Created Time"$handler <- function(h,...)
## {
##  if (is_projOpen(env=.rqda,conName="qdacon")) {
##       UpdateFileofCatWidget()
##  }
## }
FileofCatWidgetMenu$"Sort All By Created Time"$handler <- function(h,...)
{
 if (is_projOpen(env=.rqda,conName="qdacon")) {
   fid <- GetFileId(condition="filecategory",type="all")
   FileNameWidgetUpdate(FileNamesWidget=.rqda$.FileofCat,FileId=fid)
 }
}
