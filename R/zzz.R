.onLoad <- function(...) {
  ## use .onLoad rather than .First.lib when there is namespace
  cat("\nUse 'RQDA()' to start the programe.\n",fill=TRUE)
  if (interactive()) RQDA()
}
