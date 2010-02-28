addSettingGUI <- function(container,width=12){
  colorsList <- colors()
  if (Sys.info()["user"]!="") assign("owner",Sys.info()["user"],env=.rqda)
  Setting <- list(type = "ggroup",
                  horizontal = FALSE,
                  children = list(
                    list(type="fieldset",
                         columns = 1,
                         label = "Settings",
                         label.pos = "top",
                         label.font = c(weight="bold"),
                         children = list(
                           list(name = "owner",
                                label = "Name of Coder",
                                type = "gedit",width=width,
                                text = .rqda$owner
                                ),
                           list(name = "encoding",
                                label = "File Encoding",
                                type = "gedit",width=width,
                                text = .rqda$encoding
                                ),
                           list(name = "fore.col",
                                label = "Color for Coding",
                                ## type = "gedit",width=width,
                                ## text = .rqda$fore.col
                                type = "gcombobox",
                                items=c(.rqda$fore.col,colorsList)
                                ),
                           list(name = "back.col",
                                label = "Color for Case",
                                ## type = "gedit",width=width,
                                ## text = .rqda$back.col
                                type = "gcombobox",
                                items=c(.rqda$back.col,colorsList)
                                ),
##                            list(name = "codeMark.col",
##                                 label = "Global Code Marker Color",
##                                 ## type = "gedit",width=width,
##                                 ## text = .rqda$codeMark.col
##                                 type = "gcombobox",
##                                 items=c(.rqda$codeMark.col,colorsList)
##                                 ),
                           list(name = "BOM",
                                label = "Byte Order Mark",
                                type = "gcombobox",## width=width,
                                items = c(FALSE, TRUE)
                                ),
                           list(name = "SFP",
                                label = "Show File Property",
                                type = "gcombobox",## width=width,
                                items = c(FALSE, TRUE)
                                ),
                           list(name = "TOR",
                                type="gcombobox",
                                label = "Type of Retrieval",
                                items = c(.rqda$TOR, "case", "filecategory","both")
                                )
                           )
                         )
                    )
                  )

  ans <- gbutton("Set Font",cont = container,handler=function(h,...) setFont(default=.rqda$font))## set font for widget
  gtkTooltips()$setTip(ans@widget@widget,"Set fonts for memo widgets.")

  SettingFL <- gformlayout(Setting, cont = container, expand=TRUE)

  ButtonContainer <- ggroup(cont = container) ##, width=100) ## not necessary to set width here
  addSpring(ButtonContainer)
  resetButton <- gbutton("Default", cont = ButtonContainer)
  okButton <- gbutton("OK", cont = ButtonContainer)

  addHandlerChanged(okButton, function(h,...) {
    out <- svalue(SettingFL)
    tryCatch(ClearMark(.rqda$.root_edit,0,nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),error=function(e){})
    for (i in names(out)) assign(i,out[[i]],env=.rqda)
  })

  addHandlerChanged(resetButton, function(h,...) {
    tryCatch(ClearMark(.rqda$.root_edit,0,nchar(svalue(.rqda$.openfile_gui)),TRUE,TRUE),error=function(e){})
    tryCatch(svalue(SettingFL[]$BOM) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$SFP) <- FALSE,error=function(e){})
    tryCatch(svalue(SettingFL[]$encoding) <- "unknown",error=function(e){})
    tryCatch(svalue(SettingFL[]$owner) <- "default",error=function(e){})
    tryCatch(svalue(SettingFL[]$back.col) <- "gold",error=function(e){})
    tryCatch(svalue(SettingFL[]$fore.col) <- "blue",error=function(e){})
##    tryCatch(svalue(SettingFL[]$codeMark.col) <- "green",error=function(e){})
    tryCatch(svalue(SettingFL[]$TOR) <- "unconditional",error=function(e){})
    assign("BOM",FALSE,env=.rqda)
    assign("SFP",FALSE,env=.rqda)
    assign("encoding","unknown",env=.rqda)
    assign("owner","default",env=.rqda)
    assign("back.col","gold",env=.rqda)
    assign("fore.col","blue",env=.rqda)
##    assign("codeMark.col","green",env=.rqda)
    assign("TOR","unconditional",env=.rqda)
    assign("font","Sans 11",env=.rqda)
  })}

setFont <- function(default="Sans 11"){
  font <- gtkFontButtonNew()
  gtkFontButtonSetFontName(font,default)
  g <-glayout(con=gwindow(wid=50,hei=30,parent=getOption("widgetCoordinate")),hom=T)
  g[1,1:2] <- font
  g[2,1] <- gbutton("Ok",handler=function(h,...){
    ans <- font$GetFontName()
    assign("font",ans,env=.rqda)
    dispose(g)
  })
  g[2,2] <- gbutton("Cancle",handler=function(h,...) dispose(g))
}
