setColor <- function(currentColor="gold"){
  currentColor <- gdkColorParse(currentColor)$color
  colorDA <- gtkDrawingAreaNew()
  colorDA$modifyBg("normal", currentColor)
  g <-glayout(con=gwindow(wid=50,hei=20,parent=getOption("widgetCoordinate")),hom=T,title="Change color.")
  g[1,1:3] <- colorDA
  g[2,1] <- gbutton("Select Color",handler=function(h,...){
  dialog <- gtkColorSelectionDialogNew("Changing color", show=T)
  colorsel <- dialog[["colorsel"]]
  colorsel$setPreviousColor(currentColor)
  colorsel$setCurrentColor(currentColor)
  colorsel$setHasPalette(TRUE)
  response <- dialog$run()
  if (response == GtkResponseType["ok"])
    {
      currentColor <- colorsel$getCurrentColor()$color
      colorString <- gdkColorToString(currentColor)     
      colorDA$modifyBg("normal", currentColor)
    }
  dialog$destroy()
  })
  g[2,2] <- gbutton("OK",handler=function(h,...) {
   dispose(g)})
  g[2,3] <- gbutton("Cancle",handler=function(h,...) {
   dispose(g)})
}
