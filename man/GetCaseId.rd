\name{GetCaseId}
\Rdversion{1.1}
\alias{GetCaseId}
\alias{GetCaseName}
\title{
Get the Case ID and Case Name.
}
\description{
  \code{GetCaseId} returns the case IDs which a file belongs to given
  the file IDs. \code{GetCaseName} returns the case Names given the case
  IDs.
}
\usage{
GetCaseId(fid = GetFileId(), nFiles = FALSE)

GetCaseName(caseId = GetCaseId(nFiles = FALSE))
}

\arguments{
  \item{fid}{
    numeric vector, the file IDs.
  }
  \item{nFiles}{
    logical, return the number of files that belong to a case.
  }
  \item{caseId}{
    numeric vector, the case IDs.
  }
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
  \code{GetCaseId} returns a data frame of two columns when nFiles is
  TRUE, and a numeric vector when FALSE.

  \code{GetCaseName} returns a character vector or NULL if no cases are
  associated with the file IDs.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
  HUANG Ronggui
}
\seealso{
  See Also \code{\link{GetFileId}}
}
\examples{
\dontrun{
GetCaseName(GetCaseId(GetFileId("filecategory")))
}
}