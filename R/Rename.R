rename <- function(from,to,table=c("source","freecode","cases","codecat","filecat")){
  ## rename name field in table source and freecode (other tables can be added futher)
  ## source is the file name, freecode is the free code name
  ## should check it there is any dupliation in the table????????????????????????????????
  table <- match.arg(table)
  if (to!=""){## if to is "", makes no sense to rename
    dbGetQuery(.rqda$qdacon, sprintf("update %s set name = %s where name == %s ",
                                     table,
                                     paste("'",to,"'",collapse="",sep=""),
                                     paste("'",from,"'",collapse="",sep="")
                                     )
               )
  }
}
