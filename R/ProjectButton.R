NewProjectButton <- function(container){
  gbutton("New Project",container=container,handler=function(h,...){
    path=gfile(type="save",text = "Type a name for the new project and click OK.")
    if (path!=""){
      ## if path="", then click "cancel".
      Encoding(path) <- "UTF-8"
      new_proj(path,assignenv=.rqda)
      path <- gsub("\\\\","/",dbGetInfo(.rqda$qdacon)$dbname,fixed=TRUE)
      path <- gsub("/","/ ",path,fixed=TRUE)
      svalue(.rqda$.currentProj) <- gsub("/ ","/",paste(strwrap(path,60),collapse="\n"),fixed=TRUE)
      gtkWidgetSetSensitive(button$cloprob@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$BacProjB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$proj_memo@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CleProB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$ImpFilB@widget@widget,TRUE)
      gtkWidgetSetSensitive(RQDA:::.rqda$.fnames_rqda@widget@widget,TRUE)
      enabled(button$AddJouB) <- TRUE
      enabled(.rqda$.JournalNamesWidget) <- TRUE
      enabled(button$AddCodB) <- TRUE
      enabled(.rqda$.SettingsGui) <- TRUE
      enabled(.rqda$.CodeCatWidget) <- TRUE
      enabled(button$AddCodCatB) <- TRUE
      enabled(button$AddCasB) <- TRUE
      enabled(.rqda$.CasesNamesWidget) <- TRUE
      enabled(.rqda$.AttrNamesWidget) <- TRUE
      enabled(button$AddAttB) <- TRUE
      enabled(button$AddFilCatB) <- TRUE
      enabled(.rqda$.FileCatWidget) <- TRUE
    }
  }
          )
}

OpenProjectButton <- function(container){
  gbutton("Open Project",container=container,handler=function(h,...){
    path <- gfile(text = "Select a *.rqda file and click OK.",type="open",
                  filter=list("rqda"=list(patterns = c("*.rqda")),"All files" = list(patterns = c("*"))))
    if (!is.na(path)){
      Encoding(path) <- "UTF-8"
      tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
      tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
      tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
      tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
      tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
      tryCatch(close_proj(assignenv=.rqda),error=function(e){})
      ## close currect project before open a new one.
      svalue(.rqda$.currentProj) <- "Opening ..."
      open_proj(path,assignenv=.rqda)
      UpgradeTables()
      tryCatch(CodeNamesUpdate(sortByTime=FALSE),error=function(e){})
      tryCatch(FileNamesUpdate(),error=function(e){})
      tryCatch(CaseNamesUpdate(),error=function(e){})
      tryCatch(UpdateTableWidget(Widget=.rqda$.CodeCatWidget,FromdbTable="codecat"),error=function(e){})
      tryCatch(UpdateCodeofCatWidget(),error=function(e){})
      tryCatch(UpdateTableWidget(Widget=.rqda$.FileCatWidget,FromdbTable="filecat"),error=function(e){})
      tryCatch(UpdateFileofCatWidget(),error=function(e){})
      tryCatch(AttrNamesUpdate(),error=function(e){})
      tryCatch(JournalNamesUpdate(),error=function(e){})
      path <- gsub("\\\\","/",dbGetInfo(.rqda$qdacon)$dbname)
      path <- gsub("/","/ ",path)
      svalue(.rqda$.currentProj) <- gsub("/ ","/",paste(strwrap(path,50),collapse="\n"))
      gtkWidgetSetSensitive(button$cloprob@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$BacProjB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$proj_memo@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CleProB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,TRUE)
      gtkWidgetSetSensitive(button$ImpFilB@widget@widget,TRUE)
      gtkWidgetSetSensitive(RQDA:::.rqda$.fnames_rqda@widget@widget,TRUE)
      enabled(button$AddJouB) <- TRUE
      enabled(.rqda$.JournalNamesWidget) <- TRUE
      enabled(button$AddCodB) <- TRUE
      enabled(.rqda$.codes_rqda) <- TRUE
      enabled(.rqda$.SettingsGui) <- TRUE
      enabled(.rqda$.CodeCatWidget) <- TRUE
      enabled(button$AddCodCatB) <- TRUE
      enabled(button$AddCasB) <- TRUE
      enabled(.rqda$.CasesNamesWidget) <- TRUE
      enabled(.rqda$.AttrNamesWidget) <- TRUE
      enabled(button$AddAttB) <- TRUE
      enabled(button$AddFilCatB) <- TRUE
      enabled(.rqda$.FileCatWidget) <- TRUE
    }
  }
          )
}


CloseProjectButton <- function(container){
  cloprob <- gbutton("Close Project",container=container,handler=function(h,...){
    svalue(.rqda$.currentProj) <- "Closing ..."
    tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileofCase[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
    tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
    tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
    tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
    close_proj(assignenv=.rqda)
    svalue(.rqda$.currentProj) <- "No project is open."
    gtkWidgetSetSensitive(button$cloprob@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$BacProjB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$proj_memo@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$CleProB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$ImpFilB@widget@widget,FALSE)
    gtkWidgetSetSensitive(RQDA:::.rqda$.fnames_rqda@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$DelFilB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$VieFilB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$FilMemB@widget@widget,FALSE)
    gtkWidgetSetSensitive(button$FilRenB@widget@widget,FALSE)
    enabled(button$AddJouB) <- FALSE
    enabled(.rqda$.JournalNamesWidget) <- FALSE
    enabled(button$DelJouB) <- FALSE
    enabled(button$RenJouB) <- FALSE
    enabled(button$OpeJouB) <- FALSE
    enabled(button$AddCodB) <- FALSE
    enabled(.rqda$.codes_rqda) <- FALSE
    enabled(.rqda$.codes_rqda) <- FALSE
    enabled(button$RetB) <- FALSE
    enabled(button$DelCodB) <- FALSE
    enabled(button$codememobuton) <- FALSE
    enabled(button$FreCodRenB) <- FALSE
    enabled(button$c2memobutton) <- FALSE
    enabled(.rqda$.SettingsGui) <- FALSE
    enabled(.rqda$.CodeCatWidget) <- FALSE
    enabled(.rqda$.CodeofCat) <- FALSE
    enabled(button$AddCodCatB) <- FALSE
    enabled(button$DelCodCatB) <- FALSE
    enabled(button$CodCatMemB) <- FALSE
    enabled(button$CodCatRenB) <- FALSE
    enabled(button$CodCatAddToB) <- FALSE
    enabled(button$CodCatADroFromB) <- FALSE
    enabled(button$AddCasB) <- FALSE
    enabled(button$DelCasB) <- FALSE
    enabled(button$CasRenB) <- FALSE
    enabled(button$CasMarB) <- FALSE
    enabled(button$CasUnMarB) <- FALSE
    enabled(.rqda$.CasesNamesWidget) <- FALSE
    enabled(.rqda$.FileofCase) <- FALSE
    enabled(.rqda$.AttrNamesWidget) <- FALSE
    enabled(button$AddAttB) <- FALSE
    enabled(button$DelAttB) <- FALSE
    enabled(button$RenAttB) <- FALSE
    enabled(button$AttMemB) <- FALSE
    enabled(button$SetAttClsB) <- FALSE
    enabled(.rqda$.FileCatWidget) <- FALSE
    enabled(.rqda$.FileofCat) <- FALSE
    enabled(button$AddFilCatB) <- FALSE
    enabled(button$DelFilCatB) <- FALSE
    enabled(button$FilCatRenB) <- FALSE
    enabled(button$FilCatMemB) <- FALSE
    enabled(button$FilCatAddToB) <- FALSE
    enabled(button$FilCatDroFromB) <- FALSE
  }
                     )
  assign("cloprob",cloprob,env=button)
  gtkWidgetSetSensitive(button$cloprob@widget@widget,FALSE)
}

BackupProjectButton <- function(container){
  BacProjB <- gbutton("Backup Project",container=container,handler=function(h,...){
    backup_proj(con=.rqda$qdacon)
  }
                      )
  assign("BacProjB",BacProjB,env=button)
  gtkWidgetSetSensitive(button$BacProjB@widget@widget,FALSE)
}


Proj_MemoButton <- function(label="Porject Memo",container,...){
  ## Each button a separate function -> more easy to debug, and the main function root_gui is shorter.
  ## The memo in dataset is UTF-8
  ## label of button
  ## name of contaianer or TRUE
  proj_memo <- gbutton(label, contain=container, handler=function(h,...) {
    ProjectMemoWidget()
  }
                       )
  assign("proj_memo",proj_memo,env=button)
  gtkWidgetSetSensitive(button$proj_memo@widget@widget,FALSE)
}


CleanProjButton <- function(label="Clean Project",container,...){
  CleProB <- gbutton(label, contain=container, handler=function(h,...) {
    CleanProject(ask=FALSE)
  }
                     )
  assign("CleProB",CleProB,env=button)
  gtkWidgetSetSensitive(button$CleProB@widget@widget,FALSE)
}

CloseAllCodingsButton <- function(label="Close All Codings",container,...){
  CloAllCodB <- gbutton(label, contain=container, handler=function(h,...) {
    close_AllCodings()
  }
                        )
  assign("CloAllCodB",CloAllCodB,env=button)
  gtkWidgetSetSensitive(button$CloAllCodB@widget@widget,FALSE)
}


####################
## defunct functions
####################
## ProjectInforButton <- function(container){
## gbutton("Current Project",container=container,handler=function(h,...){
##     if (is_projOpen(env=.rqda,conName="qdacon")) {
##       con <- .rqda$qdacon
##       dbname <- dbGetInfo(.rqda$qdacon)$dbname
##       ##substr(dbname, nchar(dbname)-15,nchar(dbname))
##       gmessage(dbname,title="Info about current project.",con=TRUE)
##     }
##   },
##                              action=list(env=.rqda,conName="qdacon")
##                              )
## }
