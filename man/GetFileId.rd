\name{GetFileId}
\alias{GetFileId}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Get the id of files list }
\description{
  Get the id of files list.
}
\usage{
GetFileId(condition = c("unconditional", "case", "filecategory"), type = c("all", "coded", "uncoded"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{condition}{Any one of "unconditional", "case" or "filecategory".}
  \item{type}{Any one of "all", "coded" or "uncoded".}
}
\details{
  The imported files are stored in a data base table (called source) in the *.rqda file. Every file in the source table has a unique id. Besides, a: every file can be assigned to a case or file category. b: they may be either coded or uncoded. \code{GetFileId} return the id of files which fit the combined criterion of a and b. The argument "condition" describe the criterion of a, and "type" describes that of b. When "condition" is "case" or "filecategory", \code{GetFileId} returns the id of files associated with the selected case or file category, which means you have to select a case or file category; otherwise the result is NULL.
}
\value{
Normally, it is a numeric vector of file id. If condition is "case" or "filecategory" but no case or file category is selected, it retuns NULL.
}
\author{ HUANG Ronggui}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
\dontrun{
GetFileId() ## Id of all files
GetFileId("unconditional","coded") ## id of all coded files.
GetFileId("case","uncoded") ## id of uncoded files for the selected case.
GetFileId("filecategory","all") ## id of all files in the selected file category.
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
