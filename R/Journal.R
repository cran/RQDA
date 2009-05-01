AddJournalButton <- function(label="ADD"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      AddNewJournalFun()
    }
  }
          )
}

DeleteJournalButton <- function(label="Delete"){
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon") & length(svalue(.rqda$.JournalNamesWidget))!=0)
      {
        del <- gconfirm("Really delete the journal?",icon="question")
        if (isTRUE(del)){
          Selected <- svalue(.rqda$.JournalNamesWidget)
          Encoding(Selected) <- "UTF-8"
          dbGetQuery(.rqda$qdacon,sprintf("update journal set status=0 where name=='%s'",Selected))
          JournalNamesUpdate()
        }
      }
  }
          )
}

RenameJournalButton <- function(label="Rename")
{
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      selected <- svalue(.rqda$.JournalNamesWidget)
      if (length(selected)==0){
        gmessage("Select one first.",icon="error",con=TRUE)
      }
      else {
        NewName <- ginput("Enter new journal name. ",text=selected, icon="info")
        if (!is.na(NewName)) {
          NewName <- enc(NewName,"UTF-8")
          rename(from=selected,to=NewName,"journal")
          JournalNamesUpdate()
        }
    }
  }
  }
          )
}


OpenJournalButton <- function(label="Open")
{
  gbutton(label,handler=function(h,...) {
    if (is_projOpen(env=.rqda,conName="qdacon")) {
      ViewJournalWidget()
    }
  })
}

JournalNamesUpdate <- function(Widget=.rqda$.JournalNamesWidget,decreasing=FALSE,...)
{
  if (isIdCurrent(.rqda$qdacon)){
    journal <- dbGetQuery(.rqda$qdacon, "select name from journal where status=1")
    if (nrow(journal)==0) {
      journal <- NULL
    } else {
      journal <- journal[,1]
      journal <- journal[OrderByTime(journal,decreasing=decreasing)]
    }
    tryCatch(Widget[] <- journal, error=function(e){})
  }
}

AddNewJournalFun <- function(){
    if (is_projOpen(env=.rqda,"qdacon")) {
        tryCatch(eval(parse(text="dispose(.rqda$.AddNewJournalWidget")),error=function(e) {}) ## close the widget if open
        gw <- gwindow(title="Add New Journal.",parent=getOption("widgetCoordinate"),width=600,height=400)
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw@widget@widget$SetIconFromFile(mainIcon) 
        assign(".AddNewJournalWidget",gw,env=.rqda)
        assign(".AddNewJournalWidget2",gpanedgroup(horizontal = FALSE, con=get(".AddNewJournalWidget",env=.rqda)),env=.rqda)
        gbutton("Save Journal",con=get(".AddNewJournalWidget2",env=.rqda),handler=function(h,...){
            title <- Sys.time()
            if (nrow(dbGetQuery(.rqda$qdacon,sprintf("select name from journal where name=='%s'",title)))!=0) {
                title <- paste("New",title)
            }## Make sure it is unique
            content <- svalue(textW)
            content <- enc(content,encoding="UTF-8") ## take care of double quote.
            ans <- dbGetQuery(.rqda$qdacon,sprintf("insert into journal (name, journal,date,owner, status)
                             values ('%s', '%s', '%s', '%s', %i)",
                                                   title, content, date(),.rqda$owner,1))
            if (is.null(ans)) {
                dispose(.rqda$.AddNewJournalWidget)
                ##gmessage("Succeed.",con=T)
            }
            ## must put here rather than in AddJournalButton()
            JournalNamesUpdate()

        }
                )## end of save button
        tmp <- gtext(container=get(".AddNewJournalWidget2",env=.rqda))
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp@widget@widget,font)
        assign(".AddNewJournalWidgetW", tmp, env=.rqda)
        textW <- get(".AddNewJournalWidgetW",env=.rqda)
    }
}

ViewJournalWidget <- function(prefix="Journal",widget=.rqda$.JournalNamesWidget,dbTable="journal"){
  if (is_projOpen(env=.rqda,"qdacon")) {
      Selected <- svalue(widget)
      if (length(Selected)==0){
        gmessage("Select first.",icon="error",con=TRUE)
      }
      else {
        tryCatch(eval(parse(text=sprintf("dispose(.rqda$.%smemo)",prefix))),error=function(e) {})
        gw <- gwindow(title=sprintf("%s:%s",prefix,Selected),parent=getOption("widgetCoordinate"),width=600,height=400)
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw@widget@widget$SetIconFromFile(mainIcon) 
        assign(sprintf(".%smemo",prefix),gw,env=.rqda)
        assign(sprintf(".%smemo2",prefix),
               gpanedgroup(horizontal = FALSE, con=get(sprintf(".%smemo",prefix),env=.rqda)),
               env=.rqda)
        gbutton("Save Journal",con=get(sprintf(".%smemo2",prefix),env=.rqda),handler=function(h,...){
          newcontent <- svalue(W)
          newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
          Encoding(Selected) <- "UTF-8"
          dbGetQuery(.rqda$qdacon,sprintf("update %s set journal='%s' where name='%s'",dbTable,newcontent,Selected))
        }
                )## end of save button
        tmp <- gtext(container=get(sprintf(".%smemo2",prefix),env=.rqda))
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp@widget@widget,font)## set the default fontsize
        assign(sprintf(".%smemoW",prefix),tmp,env=.rqda)
        prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select journal from %s where name='%s'",dbTable,Selected))[1,1]
        if (is.na(prvcontent)) prvcontent <- ""
        Encoding(prvcontent) <- "UTF-8"
        W <- get(sprintf(".%smemoW",prefix),env=.rqda)
        add(W,prvcontent,do.newline=FALSE)
        addHandlerUnrealize(get(sprintf(".%smemo",prefix),env=.rqda),handler <- function(h,...){
            withinWidget <- svalue(get(sprintf(".%smemoW",prefix),env=.rqda))
            InRQDA <- dbGetQuery(.rqda$qdacon, sprintf("select journal from %s where name='%s'",dbTable, Selected))[1, 1]
            if (isTRUE(all.equal(withinWidget,InRQDA))) {
                return(FALSE) } else {
                    val <- gconfirm("The Journal has bee change, Close anyway?",con=TRUE)
                    return(!val)
                }
        }
                            )
    }
  }
}
