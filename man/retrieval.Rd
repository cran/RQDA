\name{retrieval}
\alias{retrieval}
\title{Retrieval of codings conditional on the file id.}
\description{
To retrieve the codings of a selected code from specific set of files.
}
\usage{
retrieval(Fid = NULL, CodeNameWidget = .rqda$.codes_rqda)
}
\arguments{
  \item{Fid}{Numeric vector, the file id.}
  \item{CodeNameWidget}{The name of code list widget.}
}
\details{
  This function retrievs the codings of a selected code from
  \code{CodeNameWidget}, given that all the codings are from a set of
  files which are determined by \code{Fid}.
}
\value{
A \code{\link[gWidgets]{gext}} widget is open and all the codings are pushed into that widget.
}
\author{HUANG Ronggui}
\seealso{\code{\link{GetFileId}}}
