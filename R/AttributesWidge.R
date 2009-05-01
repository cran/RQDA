## AddVarWidget <- function(ExistingItems=NULL,container=NULL,title=NULL,ID=NULL){
##   ## modified from RGtk2 package
##   ## ExistingItems: existing data set for a case/file etc. It is data frame of 2 columns, the first is Variable
##   ## container: similar to that of gWidget package.
##   COLUMN <- c(Variable = 0, Value = 1,  editable = 2)
##   articles <- NULL

##   create.model <- function()
##     {
##       ## create the array of data
##       articles <<- list()
##       ##  create list store
##       model <- gtkListStoreNew( "gchararray", "gchararray", "gboolean")
##       ## add item from ExistingItems
##       ## needs modification
##       if (!is.null(ExistingItems)){
##         articles <<- c(articles,unlist(apply(ExistingItems,1,function(x) list(list(Variable=x[1],Value=x[2],editable=TRUE))),FALSE))
##         for (i in 1:length(articles))
##           {
##             iter <- model$append()$iter
##             model$set(iter, COLUMN["Variable"], articles[[i]]$Variable,
##                       COLUMN["Value"], articles[[i]]$Value,
##                       COLUMN["editable"], articles[[i]]$editable)
##           }
##       }
##       return(model)
##     }
  
##   add.item <- function(button, data)
##     {
##       stopifnot(!is.null(articles))
##       foo <- list(Variable = "New Var Name", Value = "NA", editable = TRUE)
##       articles <<- c(articles, foo)
##       iter <- model$append()$iter
##       model$set(iter, COLUMN["Variable"], foo$Variable,
##                 COLUMN["Value"], foo$Value,
##                 COLUMN["editable"], foo$editable)
##     }
  
##   remove.item <- function(widget, data)
##     {
##       checkPtrType(data, "GtkTreeView")
##       treeview <- data
##       model <- treeview$getModel()
##       selection <- treeview$getSelection()
##       selected <- selection$getSelected()
##       if (selected[[1]])
##         {
##           iter <- selected$iter
##           path <- model$getPath(iter)
##           i <- path$getIndices()[[1]]
##           model$remove(iter)
##           articles <<- articles[-i]
##         }
##     }
  
##   cell.edited <- function(cell, path.string, new.text, data)
##     {
##       checkPtrType(data, "GtkListStore")
##       model <- data
##       path <- gtkTreePathNewFromString(path.string) 
##       column <- cell$getData("column")
##       iter <- model$getIter(path)$iter
##       switch(column+1,
##              {
##                old.text <- model$get(iter, column)
##                i <- path$getIndices()[[1]]+1
##                articles[[i]]$Variable <<- new.text
##                model$set(iter, column, articles[[i]]$Variable)
##              },
##              {
##                i <- path$getIndices()[[1]]+1
##                articles[[i]]$Value <<- new.text
##                model$set(iter, column, articles[[i]]$Value)
##              }
##              )
##     }
  
##   add.columns <- function(treeview)
##     {
##       model <- treeview$getModel()
##       ## Variable column
##       renderer <- gtkCellRendererTextNew()
##       gSignalConnect(renderer, "edited", cell.edited, model)
##       renderer$setData("column", COLUMN["Variable"])
##       treeview$insertColumnWithAttributes(-1, "Variable", renderer,text = COLUMN[["Variable"]], editable = COLUMN[["editable"]])
##       ## Value column
##       renderer <- gtkCellRendererTextNew()
##       gSignalConnect(renderer, "edited", cell.edited, model)
##       renderer$setData("column", COLUMN["Value"])
##       treeview$insertColumnWithAttributes(-1, "Value", renderer, text = COLUMN[["Value"]],editable = COLUMN[["editable"]])
##     }
  
##   save.project <- function(button,data){
##     ## push dataset into project file.
##      IterFirst <- data$getIterFirst()
##      cond <- IterFirst[[1]]
##      iter <- IterFirst$iter
##      ans <- c()
##      while(cond) {
##        dat <- unlist(data$get(iter, 0, 1))
##        ans <- c(ans,dat)
##        cond <- data$iterNext(iter)
##      }
##      n <- length(ans)
##      idx1 <- seq(1,to=n,by=2)
##      idx2 <- seq(2,to=n,by=2)
##      ans <- data.frame(Variable=ans[idx1],Value=ans[idx2])
##      ans <- cbind(ans,ID)
##      dbGetQuery(.rqda$qdacon,sprintf("delete from caseAttr where caseid='%s'",ID))
##      dbWriteTable(.rqda$qdacon, "caseAttr", ans, append = TRUE,row.names=FALSE)
##      window$Destroy()## close
##    }
  
##   ## create window, etc
##   window <- gtkWindowNew("toplevel", show = F)
##   window$setTitle(paste("Var:",title))
##   window$setBorderWidth(5)
##   vbox <- gtkVBoxNew(FALSE, 5)
##   window$add(vbox)
##   sw <- gtkScrolledWindowNew(NULL, NULL)
##   sw$setShadowType("etched-in")
##   sw$setPolicy("automatic", "automatic")
##   vbox$packStart(sw, TRUE, TRUE, 0)
##   ## create model
##   model <- create.model()
##   ## create tree view
##   treeview <- gtkTreeViewNewWithModel(model)
##   treeview$setRulesHint(TRUE)
##   treeview$getSelection()$setMode("single")
##   add.columns(treeview)
##   sw$add(treeview)
##   ## some buttons
##   hbox <- gtkHBoxNew(TRUE, 4)
##   vbox$packStart(hbox, FALSE, FALSE, 0)
##   button <- gtkButtonNewWithLabel("Add")
##   gSignalConnect(button, "clicked", add.item, model)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   button <- gtkButtonNewWithLabel("Remove")
##   gSignalConnect(button, "clicked", remove.item, treeview)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   button <- gtkButtonNewWithLabel("Save")
##   gSignalConnect(button, "clicked",save.project,model)
##   hbox$packStart(button, TRUE, TRUE, 0)
##   window$setDefaultSize(150, 350)
##   window$showAll()
##   invisible(window)
## }


EditVarWidget <- function(ExistingItems=NULL,container=NULL,title=NULL,ID=NULL,saveFUN=NULL,...){
  ## modified from RGtk2 package
  ## ExistingItems: existing data set for a case/file etc. It is data frame of 2 columns, the first is Variable
  ## saveFUN is character.
  ## container: similar to that of gWidget package.
  COLUMN <- c(Variable = 0, Value = 1,  editable = 2)
  articles <- NULL

  create.model <- function()
    {
      ## create the array of data
      articles <<- list()
      ##  create list store
      model <- gtkListStoreNew( "gchararray", "gchararray", "gboolean")
      ## add item from ExistingItems
      ## needs modification
      if (!is.null(ExistingItems)){
        articles <<- c(articles,unlist(apply(ExistingItems,1,function(x) list(list(Variable=x[1],Value=x[2],editable=TRUE))),FALSE))
        for (i in 1:length(articles))
          {
            iter <- model$append()$iter
            model$set(iter, COLUMN["Variable"], articles[[i]]$Variable,
                      COLUMN["Value"], articles[[i]]$Value,
                      COLUMN["editable"], articles[[i]]$editable)
          }
      }
      return(model)
    }

  cell.edited <- function(cell, path.string, new.text, data)
    {
      checkPtrType(data, "GtkListStore")
      model <- data
      path <- gtkTreePathNewFromString(path.string) 
      column <- cell$getData("column")
      iter <- model$getIter(path)$iter
      if (column==1){
               i <- path$getIndices()[[1]]+1
               articles[[i]]$Value <<- new.text
               model$set(iter, column, articles[[i]]$Value)
             }
    }
  
  add.columns <- function(treeview)
    {
      model <- treeview$getModel()
      ## Variable column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Variable"])
      treeview$insertColumnWithAttributes(-1, "Variable", renderer,text = COLUMN[["Variable"]], editable = COLUMN[["editable"]])
      ## Value column
      renderer <- gtkCellRendererTextNew()
      gSignalConnect(renderer, "edited", cell.edited, model)
      renderer$setData("column", COLUMN["Value"])
      treeview$insertColumnWithAttributes(-1, "Value", renderer, text = COLUMN[["Value"]],editable = COLUMN[["editable"]])
    }

    saveFUN <- get(saveFUN,mode="function")
 
  ## create window, etc
  window <- gtkWindowNew("toplevel", show = F)
  window$setTitle(paste("Var:",title))
  window$setBorderWidth(5)
  vbox <- gtkVBoxNew(FALSE, 5)
  window$add(vbox)
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setShadowType("etched-in")
  sw$setPolicy("automatic", "automatic")
  vbox$packStart(sw, TRUE, TRUE, 0)
  ## create model
  model <- create.model()
  ## create tree view
  treeview <- gtkTreeViewNewWithModel(model)
  treeview$setRulesHint(TRUE)
  treeview$getSelection()$setMode("single")
  add.columns(treeview)
  sw$add(treeview)
  ## some buttons
  hbox <- gtkHBoxNew(TRUE, 4)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  button <- gtkButtonNewWithLabel("Save and Close")
  gSignalConnect(button, "clicked",saveFUN,list(model,window,ExistingItems,list(...)))
  hbox$packStart(button, TRUE, TRUE, 0)
  window$setDefaultSize(200, 350)
  window$showAll()
  invisible(window)
}

saveFUN4CaseAttr <- function(button,data){
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans,dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2){
    idx1 <- seq(1,to=n,by=2)
    idx2 <- seq(2,to=n,by=2)
    ans <- data.frame(Variable=ans[idx1],Value=ans[idx2],stringsAsFactors=FALSE)
    ## cal which variable is added and which is modified
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value!= "NA")
    new_idx <- change_idx & (! mod_idx)   
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx,]
    apply(vars,1,FUN=function(x) dbGetQuery(.rqda$qdacon,sprintf("update caseAttr set value == '%s' where variable == '%s' and caseID=='%s'",x[2],x[1],MoreArgs$caseId)))
    }
    if (any(new_idx)){
    ## add the new variable to table
    vars <- data.frame(variable=ans[new_idx,1],value=ans[new_idx,2],caseID=MoreArgs$caseId,date=date(),dateM=NA,owner=.rqda$owner)
    dbWriteTable(.rqda$qdacon, "caseAttr", vars, append = TRUE,row.names=FALSE)
    }
  }
  window$Destroy()## close
}

CaseAttrFun <- function(caseId,title=NULL){
  attrs <-  dbGetQuery(.rqda$qdacon,"select name from attributes where status==1")$name
  if (is.null(attrs)) gmessage("add attribute in Attrs Tabe first.",con=T) else{
    attrs2 <- data.frame(variable=attrs,value="NA",stringsAsFactors=FALSE)
    variables <- dbGetQuery(.rqda$qdacon,sprintf("select variable, value from caseAttr where caseID==%i",caseId))
    if (nrow(variables)!=0){
      idx <- match(variables[[1]],attrs2[[1]])
      attrs2[idx,] <- variables
    }
    EditVarWidget(ExistingItems=attrs2,saveFUN="saveFUN4CaseAttr",title=title,caseId=caseId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}

saveFUN4FileAttr <- function(button,data){
  ## the first arg must button, and data as second.
  ## push dataset into project file.
  model <- data[[1]]
  window <- data[[2]]
  ExistingItems <- data[[3]]
  MoreArgs <- data[[4]]
  IterFirst <- model$getIterFirst()
  cond <- IterFirst[[1]]
  iter <- IterFirst$iter
  ans <- c()
  while(cond) {
    dat <- unlist(model$get(iter, 0, 1))
    ans <- c(ans,dat)
    cond <- model$iterNext(iter)
  }
  n <- length(ans)
  if (n >= 2){
    idx1 <- seq(1,to=n,by=2)
    idx2 <- seq(2,to=n,by=2)
    ans <- data.frame(Variable=ans[idx1],Value=ans[idx2],stringsAsFactors=FALSE)
    ## cal which variable is added and which is modified
    change_idx <- ans$Value != ExistingItems$value
    mod_idx <- change_idx & (ExistingItems$value!= "NA")
    new_idx <- change_idx & (! mod_idx)   
    if (any(mod_idx)) {
    ## alter the table for the modified variable
    vars <- ans[mod_idx,]
    apply(vars,1,FUN=function(x) dbGetQuery(.rqda$qdacon,sprintf("update fileAttr set value == '%s' where variable == '%s' and fileID=='%s'",x[2],x[1],MoreArgs$fileId)))
    }
    if (any(new_idx)){
    ## add the new variable to table
    vars <- data.frame(variable=ans[new_idx,1],value=ans[new_idx,2],fileID=MoreArgs$fileId,date=date(),dateM=NA,owner=.rqda$owner)
    dbWriteTable(.rqda$qdacon, "fileAttr", vars, append = TRUE,row.names=FALSE)
    }
  }
  window$Destroy()## close
}

FileAttrFun <- function(fileId,title=NULL){
  attrs <-  dbGetQuery(.rqda$qdacon,"select name from attributes where status==1")$name
  if (is.null(attrs)) gmessage("add attribute in Attrs Tabe first.",con=T) else{
    attrs2 <- data.frame(variable=attrs,value="NA",stringsAsFactors=FALSE)
    variables <- dbGetQuery(.rqda$qdacon,sprintf("select variable, value from fileAttr where fileID==%i",fileId))
    if (nrow(variables)!=0){
      idx <- match(variables[[1]],attrs2[[1]])
      attrs2[idx,] <- variables
    }
    EditVarWidget(ExistingItems=attrs2,saveFUN="saveFUN4FileAttr",title=title,fileId=fileId)
    ## get attrs list and turn it to a data frame, pass it to ExistingItems, then call EditVarWidget
  }
}
