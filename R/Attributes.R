## change the name of Variables.R to Attributes.R

AttrNamesUpdate <- function(Widget=.rqda$.AttrNamesWidget,sortByTime=FALSE,decreasing=FALSE,...)
{
  if (isIdCurrent(.rqda$qdacon)){
    attr <- dbGetQuery(.rqda$qdacon, "select name, date from attributes where status=1")
    if (nrow(attr)==0) {
      attr <- NULL
    } else {
      attr <- attr$name
      Encoding(attr) <- "UTF-8"
      if (!sortByTime) {attr <- sort(attr)} else {
        attr <- attr[OrderByTime(attr$date,decreasing=decreasing)]
      }
    }
    tryCatch(Widget[] <- attr, error=function(e){})
  }
}

AddAttrNames <- function(name,...) {
  if (name != ""){
    con <- .rqda$qdacon
    dup <- dbGetQuery(con,sprintf("select name from attributes where name=='%s'",name))
    if (nrow(dup)==0) {
      dbGetQuery(con,sprintf("insert into attributes (name,status,date,owner) values ('%s', %i,%s, %s)",
                             name,1, shQuote(date()),shQuote(.rqda$owner)))
    }
  }
}

AddAttrButton <- function(label="ADD"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      AttrName <- ginput("Enter new Attr Name. ", icon="info")
      if (!is.na(AttrName)) {
        AttrName <- enc(AttrName,encoding="UTF-8")
        ## Encoding(AttrName) <- "UTF-8"
        if (AttrName %in% c("fileID","caseID")) gmessage("This is a reserved keyword.",con=TRUE) else{
        AddAttrNames(AttrName)
        AttrNamesUpdate()
      }}
    }
  }
          )
}

DeleteAttrButton <- function(label="Delete"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon") & length(svalue(.rqda$.AttrNamesWidget))!=0)
          {
            del <- gconfirm("Really delete the Attribute?",icon="question")
            if (isTRUE(del)){
              Selected <- svalue(.rqda$.AttrNamesWidget)
              ## Encoding(Selected) <- "UTF-8"
              Selected <- enc(Selected,"UTF-8")
              dbGetQuery(.rqda$qdacon,sprintf("update attributes set status=0 where name=='%s'",Selected))
              dbGetQuery(.rqda$qdacon,sprintf("delete from caseAttr where variable=='%s'",Selected))
              dbGetQuery(.rqda$qdacon,sprintf("update from fileAttr where variable=='%s'",Selected))
              AttrNamesUpdate()
            }
          }
  }
          )
}

RenameAttrButton <- function(label="Rename"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      selected <- svalue(.rqda$.AttrNamesWidget)
      if (length(selected)==0){
        gmessage("Select a attribute first.",icon="error",con=TRUE)
      }
      else {
        ## get the new file names
        NewName <- ginput("Enter new attribute name. ", text=selected, icon="info")
        if (!is.na(NewName)){
          ## Encoding(NewName) <- "UTF-8"
          selected <- enc(selected,encoding="UTF-8")
          NewName<- enc(NewName,encoding="UTF-8")
          exists <- dbGetQuery(.rqda$qdacon, sprintf("select * from attributes where name == '%s' ",NewName))
          if (nrow(exists) > 0 ){
          gmessage("Name duplicated. Please use anaother name.",cont=TRUE)
          } else {
          dbGetQuery(.rqda$qdacon, sprintf("update attributes set name = '%s' where name == '%s' ",NewName,selected))
          dbGetQuery(.rqda$qdacon, sprintf("update caseAttr set variable = '%s' where variable == '%s' ",NewName,selected))
          dbGetQuery(.rqda$qdacon, sprintf("update fileAttr set variable = '%s' where variable == '%s' ",NewName,selected))
          AttrNamesUpdate()
         }
        }
      }
    }
  }
          )
}

AttrMemoButton <- function(label="Memo"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
    MemoWidget("Attributes",.rqda$.AttrNamesWidget,"attributes")
 }
  }
          )
}

viewCaseAttr <- function(){
  DF <- dbGetQuery(.rqda$qdacon,"select variable,value, caseId from caseAttr")
  DF <- reshape(DF,v.name="value",idvar="caseID",direction="wide",timevar="variable")
  names(DF) <- gsub("^value.","",names(DF))
  caseName <- dbGetQuery(.rqda$qdacon,"select name,id from cases where status==1")
  if (nrow(caseName)!=0){
    names(caseName) <- c("case","caseID")
    Encoding(caseName$case) <- "UTF-8"
    DF <- merge(caseName,DF)
    gtable(DF,con=TRUE)
  }
}

viewFileAttr <- function(){
  DF <- dbGetQuery(RQDA:::.rqda$qdacon,"select variable,value, fileId from fileAttr")
  DF <- reshape(DF,v.name="value",idvar="fileID",direction="wide",timevar="variable")
  names(DF) <- gsub("^value.","",names(DF))
  fileName <- dbGetQuery(.rqda$qdacon,"select name,id from source where status==1")
  if (nrow(fileName)!=0){
    names(fileName) <- c("file","fileID")
    Encoding(fileName$case) <- "UTF-8"
    DF <- merge(fileName,DF)
    gtable(DF,con=TRUE)
  }
}


GetAttr <- function(type=c("case","file")){
  if (isIdCurrent(.rqda$qdacon)){
  type <-  match.arg(type)
  if (type == "case"){
    DF <- dbGetQuery(.rqda$qdacon,"select variable,value, caseId from caseAttr")
    if (nrow(DF) > 0 ){
    DF <- reshape(DF,v.name="value",idvar="caseID",direction="wide",timevar="variable")
    names(DF) <- gsub("^value.","",names(DF))
    caseName <- dbGetQuery(.rqda$qdacon,"select name,id from cases where status==1")
    if (nrow(caseName)!=0){
      names(caseName) <- c("case","caseID")
      Encoding(caseName$case) <- "UTF-8"
      DF <- merge(caseName,DF)
      class(DF) <- c("CaseAttr","data.frame")
    }}
  } else if (type=="file"){
    DF <- dbGetQuery(RQDA:::.rqda$qdacon,"select variable,value, fileId from fileAttr")
    if (nrow(DF) > 0 ){
    DF <- reshape(DF,v.name="value",idvar="fileID",direction="wide",timevar="variable")
    names(DF) <- gsub("^value.","",names(DF))
    fileName <- dbGetQuery(.rqda$qdacon,"select name,id from source where status==1")
    if (nrow(fileName)!=0){
      names(fileName) <- c("file","fileID")
      Encoding(fileName$case) <- "UTF-8"
      DF <- merge(fileName,DF)
      class(DF) <- c("FileAttr","data.frame")
    }}
  }
  DF
}}
    


