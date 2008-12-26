RQDA <- function() {
########################### aux functions
########################### 
  NI <- function(...){
    gmessage("Not Implemented Yet.",con=TRUE)
  }


  
########################### GUI FOR ROOT
########################### 
  ".root_rqdagui" <- gwindow(title = "RQDA: Qualitative Data Analysis.",parent=c(2,10),
                             width=300,height=600,visible=FALSE,handler=function(h,...){
                               tryCatch(dispose(.rqda$.root_edit),error=function(e){})
                               close_proj(assignenv=.rqda)
                             }
                             )

  
  ".nb_rqdagui" <- gnotebook(4,container=.root_rqdagui,closebuttons=FALSE)
  
  
  
########################### GUI FOR PROJECT
########################### 
  ".proj_gui" <- ggroup(container=.nb_rqdagui,horizontal=FALSE,label="Project")
  NewProjectButton(container=.proj_gui)
  OpenProjectButton(container=.proj_gui)
  CloseProjectButton(container=.proj_gui)
  Proj_MemoButton(label = "Project Memo", container = .proj_gui)
  ## project memo button
  ProjectInforButton(container=.proj_gui)
  BackupProjectButton(container=.proj_gui)
  gbutton("About",container=.proj_gui, handler=function(h,...) {browseURL("http://rqda.r-forge.r-project.org/")})

  glabel(
"Author: <ronggui.huang@gmail.com>\n
License: New style BSD License\n
Version: 0.1-6\n",
         container=.proj_gui
        )



########################### GUI for FILES 
###########################
  ".files_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="Files")
  ".files_button" <- ggroup(container=.files_pan,horizontal=TRUE)
  ".fnames_rqda" <- gtable("Click Here to see the File list.",container=.files_pan,multiple=TRUE)
  .fnames_rqda[] <-NULL # get around of the text argument.
  names(.fnames_rqda) <- "Files"
  ImportFileButton("Import",con=.files_button)
  DeleteFileButton("Delete",con=.files_button)
  ViewFileButton("Open",con=.files_button)
  File_MemoButton(label="F-Memo", container=.files_button,FileWidget=.fnames_rqda)
  ## memo button of selected file. The code of File_Memo buttion has been moved into memo.R
  File_RenameButton(label="Rename", container=.files_button,FileWidget=.fnames_rqda)
  ## rename a selected file.

 
########################### GUI for CODES
###########################
  ".codes_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="Codes")
  ".codes_button" <- glayout(container=.codes_pan)
  ".codes_rqda" <- gtable("Please click Update",container=.codes_pan)
  .codes_rqda[] <- NULL ;names(.codes_rqda) <- "Codes List"
  .codes_button[1,1]<- AddCodeButton()
  .codes_button[1,2]<- DeleteCodeButton()
  .codes_button[1,3] <- FreeCode_RenameButton(label="Rename",CodeNamesWidget=.codes_rqda)
  .codes_button[1,4] <- CodeMemoButton(label="C-Memo")
  .codes_button[1,5]<- CodingMemoButton(label="C2Memo")
  .codes_button[2,1]<- CodingInfoButton()
  .codes_button[2,2]<- HL_ALLButton()
 # .codes_button[2,2]<- RetrievalButton("Retrieval")
  .codes_button[2,3]<- ExtendButton("Retrieval")
  .codes_button[2,4]<- Unmark_Button()
  .codes_button[2,5]<- Mark_Button()

######################### GUI  for cases
#########################
  ".case_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="Cases")
  ".case_buttons" <- glayout(container=.case_pan)
  ".case_PW" <- ggroup(cont=.case_pan,horizontal = FALSE)
  ".CasesNamesWidget" <- gtable("Please click Update",container=.case_PW,expand=TRUE,multiple=FALSE)
  .CasesNamesWidget[] <- NULL ; names(.CasesNamesWidget) <- "Cases"
  ".FileofCase" <- gtable("Please click Update",container=.case_PW,expand=TRUE,multiple=TRUE)
  .FileofCase[] <- NULL;names(.FileofCase)<-"Files of This Case"

  .case_buttons[1,1] <- AddCaseButton()
  .case_buttons[1,2] <- DeleteCaseButton()
  .case_buttons[1,3] <- Case_RenameButton()
  ##.case_buttons[1,4] <- CaseMemoButton()
  .case_buttons[1,4] <- CaseUnMark_Button()
  .case_buttons[1,5] <- CaseMark_Button()
  ##.case_buttons[2,3] <- AddWebSearchButton("WebSearch") # use popup menu instead
  

######################### GUI  for C-cat
#########################
  ".codecat_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="C-Cat")
  ".codecat_buttons" <- glayout(container=.codecat_pan)
  ".Ccat_PW" <- ggroup(cont=.codecat_pan,horizontal = FALSE)## parent Widget of C-cat
  ".CodeCatWidget" <- gtable("Please click Update",container=.Ccat_PW,expand=TRUE)
   .CodeCatWidget[] <- NULL; names(.CodeCatWidget)<-"Code Category"
   ".CodeofCat" <- gtable("Please click Update",container=.Ccat_PW,expand=TRUE,multiple=TRUE)
   .CodeofCat[] <- NULL;names(.CodeofCat)<-"Codes of This Category"
   .codecat_buttons[1,1] <- AddCodeCatButton("Add")
   .codecat_buttons[1,2] <- DeleteCodeCatButton("Delete") ## should take care of treecode table
   .codecat_buttons[1,3] <- CodeCat_RenameButton("Rename")
   .codecat_buttons[1,4] <- CodeCatAddToButton("AddTo")
   .codecat_buttons[1,5] <- CodeCatDropFromButton("DropFrom")

######################### GUI  for F-cat
#########################
  ".filecat_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="F-Cat")
  ".filecat_buttons" <- glayout(container=.filecat_pan)
  ".Fcat_PW" <- ggroup(cont=.filecat_pan,horizontal = FALSE)## parent Widget of F-cat
  ".FileCatWidget" <- gtable("Please click Update",container=.Fcat_PW,expand=TRUE)
   .FileCatWidget[] <- NULL; names(.FileCatWidget)<-"File Category"
   ".FileofCat" <- gtable("Please click Update",container=.Fcat_PW,expand=TRUE,multiple=TRUE)
   .FileofCat[] <- NULL;names(.FileofCat)<-"Files of This Category"
   .filecat_buttons[1,1] <- AddFileCatButton("Add")
   .filecat_buttons[1,2] <- DeleteFileCatButton("Delete") ## should take care of treecode table
   .filecat_buttons[1,3] <- FileCat_RenameButton("Rename")
   .filecat_buttons[1,4] <- FileCatAddToButton("AddTo")
   .filecat_buttons[1,5] <- FileCatDropFromButton("DropFrom")


########################### GUI for Search 
###########################
##   ".fsearch_pan" <- gpanedgroup(container=.nb_rqdagui,horizontal=FALSE,label="F-Search")
##  ".fsearch_rqda" <- glabel("Use SearchFiles function to search files.\nSee ?SearchFiles for more.",container=.fsearch_pan)
##  ".fsearch_rqda" <- gtable("Click Here to see the File list.",container=.fsearch_pan,multiple=TRUE,expand=TRUE)
##  .fsearch_rqda[] <-NULL # get around of the text argument.
##  names(.fsearch_rqda) <- "Files Search"

 
######################### GUI  for settings
#########################
   ".settings_gui" <- ggroup(container=.nb_rqdagui,horizontal=FALSE,label="Settings")
   addSettingGUI(cont=.settings_gui)
  
######################### Put them together
#########################
  visible(.root_rqdagui) <- TRUE
  svalue(.nb_rqdagui) <- 1 ## make sure the project tab gain the focus.

##########################
## add documentation here
assign(".root_rqdagui",.root_rqdagui,env=.rqda)
assign(".files_button",.files_button,env=.rqda)
assign(".codes_rqda",.codes_rqda,env=.rqda)
assign(".fnames_rqda",.fnames_rqda,env=.rqda)
##assign(".fsearch_rqda",.fsearch_rqda,env=.rqda)
assign(".CasesNamesWidget",.CasesNamesWidget,env=.rqda)
assign(".FileofCase",.FileofCase,env=.rqda)
assign(".CodeCatWidget",.CodeCatWidget,env=.rqda)
assign(".CodeofCat",.CodeofCat,env=.rqda)
assign(".FileCatWidget",.FileCatWidget,env=.rqda)
assign(".FileofCat",.FileofCat,env=.rqda)

##########################
### set the positions
svalue(.codes_pan) <- 0.13
svalue(.codecat_pan)<-0.07
svalue(.filecat_pan)<-0.07
svalue(.case_pan)<-0.07

##########################
Handler()
}
## end of function RQDA

