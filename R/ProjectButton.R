NewProjectButton <- function(container){
gbutton("New Project",container=container,handler=function(h,...){
    path=gfile(type="save") 
    if (path!=""){
      ## if path="", then click "cancel".
      Encoding(path) <- "UTF-8"
      new_proj(path,assignenv=.rqda)}
  }
                            )
}

OpenProjectButton <- function(container){
gbutton("Open Project",container=container,handler=function(h,...){
    path <- gfile(text = "Select a *.rqda file and click OK.",type="open",filter=list("rqda"=list(patterns = c("*.rqda")),
                                          "All files" = list(patterns = c("*"))))
    if (path!=""){
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
    }
  }
                              )
}


CloseProjectButton <- function(container){
gbutton("Close Project",container=container,handler=function(h,...){
      tryCatch(.rqda$.codes_rqda[]<-NULL,error=function(e){})
      tryCatch(.rqda$.fnames_rqda[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CasesNamesWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CodeCatWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.CodeofCat[]<-NULL,error=function(e){})
      tryCatch(.rqda$.FileCatWidget[]<-NULL,error=function(e){})
      tryCatch(.rqda$.FileofCat[]<-NULL,error=function(e){})
      tryCatch(.rqda$.AttrNamesWidget[] <- NULL,error=function(e){})
      tryCatch(.rqda$.JournalNamesWidget[] <- NULL,error=function(e){})
      close_proj(assignenv=.rqda)
      }
                               )

}

ProjectInforButton <- function(container){
gbutton("Current Project",container=container,handler=function(h,...){
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      con <- .rqda$qdacon
      dbname <- dbGetInfo(.rqda$qdacon)$dbname
      ##substr(dbname, nchar(dbname)-15,nchar(dbname))
      gmessage(dbname,title="Info about current project.",con=TRUE)
    }
  },
                             action=list(env=.rqda,conName="qdacon")
                             )
}

BackupProjectButton <- function(container){
gbutton("Backup Project",container=container,handler=function(h,...){
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      backup_proj(con=.rqda$qdacon)
    }
  }
        )
}


Proj_MemoButton <- function(label="Porject Memo",container,...){
#### Each button a separate function -> more easy to debug, and the main function root_gui is shorter.
### The memo in dataset is UTF-8
  ## label of button
  ## name of contaianer or TRUE
  proj_memo <- gbutton(label, contain=container, handler=function(h,...) {
    ProjectMemoWidget()
  }
                       )
}


CleanProjButton <- function(label="Clean Porject",container,...){
  gbutton(label, contain=container, handler=function(h,...) {
    CleanProject(ask=FALSE)
  }
          )
}
