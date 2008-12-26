new_proj <- function(path, conName="qdacon",assignenv=.rqda,...){
  ## sucess <- file.create(tmpNamme <- tempfile(pattern = "file", tmpdir = dirname(path)))
  sucess <- (file.access(names=dirname(path),mode=2)==0)
  if (!sucess) {
    gmessage("No write permission.",icon="error",container=TRUE) 
  }
  else{
    ## unlink(tmpNamme)
    path <- paste(gsub("\\.rqda$","",path),"rqda",sep=".") ## deal with the ".rqda"
    override <- FALSE
    if (fexist <- file.exists(path)) {
      ## if there exists a file, should ask; and test if have write access to overwrite it.
      override <- gconfirm("Over write existing project?",icon="warning")
      if (file.access(path, 2) != 0 && override) {
        override <- FALSE
        gmessage("You have no write permission to overwrite it.",con=TRUE,icon="error")
      }
    }
    if (!fexist | override ){
      ## close con in assignmenv first.
      tryCatch(close_proj(conName=conName,assignenv=assignenv),error=function(e){})
      
      assign(conName,dbConnect(drv=dbDriver("SQLite"),dbname=path),envir=assignenv)
      con <- get(conName,assignenv)
      
      if (dbExistsTable(con,"source")) dbRemoveTable(con, "source")
      ## interview record
      dbGetQuery(con,"create table source (name text, id integer,
                                           file text, memo text,
                                           owner text, date text, dateM text, status integer)")
      ## dateM means modified date
      if (dbExistsTable(con,"freecode")) dbRemoveTable(con, "freecode")
      ## list of free codes
      dbGetQuery(con,"create table freecode  (name text, memo text,
                                              owner text,date text,dateM text,
                                              id integer, status integer)")
      if (dbExistsTable(con,"treecode")) dbRemoveTable(con, "treecode")
      ## tree-like strcuture of code (relationship between code and code-category[codecat])
      dbGetQuery(con,"create table treecode  (cid integer, catid integer
                                              owner text, date text, dateM text,
                                              memo text, status integer)")
      if (dbExistsTable(con,"treefile")) dbRemoveTable(con, "treefile")
      ## tree-like structure of interview record  (relationship between file and file category [filecat])
      dbGetQuery(con,"create table treefile  (fid integer, catid integer
                                              owner text, date text,dateM text,
                                              memo text, status integer)")
      if (dbExistsTable(con,"filecat")) dbRemoveTable(con, "filecat")
      ## file category
      dbGetQuery(con,"create table filecat  (name text,fid integer, catid integer, owner text,
                                             date text, dateM text,memo text, status integer)")
      if (dbExistsTable(con,"codecat")) dbRemoveTable(con, "codecat")
      ## code category
      dbGetQuery(con,"create table codecat  (name text, cid integer, catid integer, owner text, date text,
                                             dateM text,memo text, status integer)")
      if (dbExistsTable(con,"coding")) dbRemoveTable(con, "coding")
      ## coding: code and its coded text chunks
      dbGetQuery(con,"create table coding  (cid integer, fid integer,seltext text,
                                            selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")
      if (dbExistsTable(con,"project")) dbRemoveTable(con, "project")
      ## coding: information about the project
      dbGetQuery(con,"create table project  (encoding text, detabaseversion text, date text,dateM text,
                                             memo text,BOM integer)")
      dbGetQuery(con,sprintf("insert into project (detabaseversion,date,memo) values ('0.1.5','%s','')",date()))
      if (dbExistsTable(con,"cases")) dbRemoveTable(con, "cases")
      dbGetQuery(con,"create table cases  (name text, memo text,
                                           owner text,date text,dateM text,
                                           id integer, status integer)")
      if (dbExistsTable(con,"caselinkage")) dbRemoveTable(con, "caselinkage")
      dbGetQuery(con,"create table caselinkage  (caseid integer, fid integer,
                                                selfirst real, selend real, status integer,
                                            owner text, date text, memo text)")
    }
  }
}



open_proj <- function(path,conName="qdacon",assignenv=.rqda,...){
  tryCatch({ con <- get(conName,assignenv)
             if (isIdCurrent(con)) dbDisconnect(con)
           },
           error=function(e){})
  ## Fist close the con if it exist, then open a new con.
  if (file.access(path, 2) == 0) {
    assign(conName, dbConnect(drv=dbDriver("SQLite"),dbname=path),envi=assignenv)
  } else if (file.access(path, 4) == 0){
    assign(conName, dbConnect(drv=dbDriver("SQLite"),dbname=path),envi=assignenv)
    gmessage("You don't have write access to the *.rqda file. You can only read the project.",con=TRUE,icon="warning")
  } else {
    gmessage("You don't have read access to the *.rqda file. Fail to open.",con=TRUE,icon="error")
}
}



close_proj <- function(conName="qdacon",assignenv=.rqda,...){
  tryCatch({
    con <- get(conName,assignenv)
    if (isIdCurrent(con)) {
      if (!dbDisconnect(con)) {
        gmessage("Closing project failed.",icon="waring",con=TRUE)
      } 
    }
  } ,error=function(e){})
}



is_projOpen <- function(env=.rqda,conName="qdacon",message=TRUE){
  ## test if any project is open.
  open <- FALSE
  tryCatch({
    con <- get(conName,env)
    open <- open + isIdCurrent(con)
  } ,error=function(e){}) 
  if (!open & message) gmessage("No Project is Open.",icon="warning",con=TRUE)
  return(open)
}

backup_proj <- function(con){
## con=.rqda$qdacon
dbname <- dbGetInfo(con)$dbname
backupname <- sprintf("%s_%s",dbname,format(Sys.time(), "%H%M%S%d%m%Y"))
success <- file.copy(from=dbname, to=backupname , overwrite = FALSE)
if (success) {
gmessage("Succeeded!",con=TRUE,icon="info")
} else{
gmessage("Fail to back up the project.",con=TRUE,icon="error")
}
}
