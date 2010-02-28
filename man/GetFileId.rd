\name{GetFileId}
\alias{GetFileId}
\alias{GetFileName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get the ids or names of files list }
\description{
  Get the ids or names of files list.
}
\usage{
GetFileId(condition = c("unconditional", "case", "filecategory","both"), 
          type = c("all", "coded", "uncoded","selected"))

GetFileName(fid = GetFileId())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{condition}{Any one of "unconditional", "case", "filecategory" or "both".}
  \item{type}{Any one of "all", "coded" or "uncoded","selected".}
  \item{fid}{integer vector, the id of files.}
}
\details{
  The imported files are stored in a data base table (called source) in
  the *.rqda file. Every file in the source table has a unique
  id. Besides, every file can be assigned to a case or file category.

  Given that files meet the \code{condition}, the \code{type} argument
  "all" means all files, "coded" means the coded files, "uncoded" means
  the uncoded files and "selected" means the selected files; in "files"
  widget, "files of case" widget and "files of category" widget respectively.

  When \code{condition} is "both", the result is intersection of File Id of "case" and "filecategory".
  	  
  \code{GetFileId} returns the id of files which fit the combined
  criterion of \code{condition} and \code{type}.
}
\value{
Normally, it is a numeric vector of file id. If condition is "case" or "filecategory" but no case or file category is selected, it retuns NULL.
}
\author{ HUANG Ronggui}
\seealso{ \code{\link{retrieval}}, \code{\link{GetFileIdSets}}}
\examples{
\dontrun{
GetFileId() ## Id of all files
GetFileId("unconditional","coded") ## id of all coded files.
GetFileId("case","uncoded") ## id of uncoded files for the selected case.
GetFileId("filecategory","all") ## id of all files in the selected file category.
}
}
