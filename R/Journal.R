AddJournalButton <- function(label="ADD"){
  AddJouB <- gbutton(label,handler=function(h,...) {
        AddNewJournalFun()
    }
          )
  assign("AddJouB",AddJouB,env=button)
  enabled(AddJouB) <- FALSE
  AddJouB
}

DeleteJournalButton <- function(label="Delete"){
  DelJouB <- gbutton(label,handler=function(h,...) {
        del <- gconfirm("Really delete the journal?",icon="question")
        if (isTRUE(del)){
          Selected <- svalue(.rqda$.JournalNamesWidget)
          Encoding(Selected) <- "UTF-8"
          RQDAQuery(sprintf("update journal set status=0 where name=='%s'",enc(Selected)))
          JournalNamesUpdate()
        }
      }
          )
  assign("DelJouB",DelJouB,env=button)
  enabled(DelJouB) <- FALSE
  DelJouB
}

RenameJournalButton <- function(label="Rename")
{
  RenJouB <- gbutton(label,handler=function(h,...) {
      selected <- svalue(.rqda$.JournalNamesWidget)
        NewName <- ginput("Enter new journal name. ",text=selected, icon="info")
        if (!is.na(NewName)) {
          rename(from=selected,to=NewName,"journal")
          JournalNamesUpdate()
        }
    }
          )
  assign("RenJouB",RenJouB,env=button)
  enabled(RenJouB) <- FALSE
  RenJouB
}


OpenJournalButton <- function(label="Open")
{
  OpeJouB <- gbutton(label,handler=function(h,...) {
      ViewJournalWidget()
  })
  assign("OpeJouB",OpeJouB,env=button)
  enabled(OpeJouB) <- FALSE
  OpeJouB
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
        gw <- gwindow(title="Add New Journal.",parent=getOption("widgetCoordinate"),
                      width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                           )
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw@widget@widget$SetIconFromFile(mainIcon)
        assign(".AddNewJournalWidget",gw,env=.rqda)
        assign(".AddNewJournalWidget2",gpanedgroup(horizontal = FALSE, con=get(".AddNewJournalWidget",env=.rqda)),env=.rqda)
        gbutton("Save Journal",con=get(".AddNewJournalWidget2",env=.rqda),handler=function(h,...){
            title <- ginput("Enter new file name. ",text=Sys.time(), icon="info")
            if (!is.na(title)){
            if (nrow(dbGetQuery(.rqda$qdacon,sprintf("select name from journal where name=='%s'",enc(title))))!=0) {
                title <- paste("New",title)
            }## Make sure it is unique
            content <- svalue(textW)
            content <- enc(content,encoding="UTF-8") ## take care of double quote.
            ans <- dbGetQuery(.rqda$qdacon,sprintf("insert into journal (name, journal,date,owner, status)
                             values ('%s', '%s', '%s', '%s', %i)",
                                                   enc(title), content, date(),.rqda$owner,1))
            if (is.null(ans)) {
                dispose(.rqda$.AddNewJournalWidget)
                ##gmessage("Succeed.",con=T)
            }
            ## must put here rather than in AddJournalButton()
            JournalNamesUpdate()
        }}
                )## end of save button
        tmp <- gtext(container=get(".AddNewJournalWidget2",env=.rqda))
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp@widget@widget,font)
        assign(".AddNewJournalWidgetW", tmp, env=.rqda)
        textW <- get(".AddNewJournalWidgetW",env=.rqda)
    }}

ViewJournalWidget <- function(prefix="Journal",widget=.rqda$.JournalNamesWidget,dbTable="journal"){
  if (is_projOpen(env=.rqda,"qdacon")) {
      Selected <- svalue(widget)
      if (length(Selected)==0){
        gmessage("Select first.",icon="error",con=TRUE)
      }
      else {
        tryCatch(eval(parse(text=sprintf("dispose(.rqda$.%smemo)",prefix))),error=function(e) {})
        gw <- gwindow(title=sprintf("%s:%s",prefix,Selected),parent=getOption("widgetCoordinate"),
                     width = getOption("widgetSize")[1], height = getOption("widgetSize")[2]
                           )
        mainIcon <- system.file("icon", "mainIcon.png", package = "RQDA")
        gw@widget@widget$SetIconFromFile(mainIcon)
        assign(sprintf(".%smemo",prefix),gw,env=.rqda)
        assign(sprintf(".%smemo2",prefix),
               gpanedgroup(horizontal = FALSE, con=get(sprintf(".%smemo",prefix),env=.rqda)),
               env=.rqda)
        saveJournalButton <- gbutton("Save Journal",con=get(sprintf(".%smemo2",prefix),env=.rqda),handler=function(h,...){
            newcontent <- svalue(W)
            newcontent <- enc(newcontent,encoding="UTF-8") ## take care of double quote.
            Encoding(Selected) <- "UTF-8"
            dbGetQuery(.rqda$qdacon,sprintf("update %s set journal='%s' where name='%s'",dbTable,newcontent,enc(Selected)))
            enabled(button$saveJournalB) <- FALSE
        }
                                     )## end of save button
        assign("saveJournalB",saveJournalButton,env=button)
        enabled(saveJournalButton) <- FALSE
        tmp <- gtext(container=get(sprintf(".%smemo2",prefix),env=.rqda))
        font <- pangoFontDescriptionFromString(.rqda$font)
        gtkWidgetModifyFont(tmp@widget@widget,font)## set the default fontsize
        assign(sprintf(".%smemoW",prefix),tmp,env=.rqda)
        prvcontent <- dbGetQuery(.rqda$qdacon, sprintf("select journal from %s where name='%s'",dbTable,enc(Selected)))[1,1]
        if (is.na(prvcontent)) prvcontent <- ""
        Encoding(prvcontent) <- "UTF-8"
        W <- get(sprintf(".%smemoW",prefix),env=.rqda)
        add(W,prvcontent,do.newline=FALSE)
        addHandlerKeystroke(tmp,handler=function(h,...){
            enabled(button$saveJournalB) <- TRUE
        })
        addHandlerUnrealize(get(sprintf(".%smemo",prefix),env=.rqda),handler <- function(h,...){
            withinWidget <- svalue(get(sprintf(".%smemoW",prefix),env=.rqda))
            InRQDA <- dbGetQuery(.rqda$qdacon, sprintf("select journal from %s where name='%s'",dbTable, enc(Selected)))[1, 1]
            if (isTRUE(all.equal(withinWidget,InRQDA))) {
                return(FALSE) } else {
                    val <- gconfirm("The Journal has bee changed, Close anyway?",con=TRUE)
                    return(!val)
                }
        }
                            )
    }
  }
}
