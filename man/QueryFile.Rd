\name{QueryFile}
\Rdversion{1.1}
\alias{QueryFile}
\title{
Retrieval of file names according to their codings.
}
\description{
To retrieve file names according to their codings and the relationship of codings.
}
\usage{
QueryFile(or, and = NULL, not = NULL, names = TRUE)
}
\arguments{
  \item{or}{ code id.}
  \item{and}{ code id.}
  \item{not}{code id.}
  \item{names}{logical, returns file names when TRUE.}
}
\details{
This function is experimental, and may be changed in future.

For arguments of 'or', 'and' and 'not', all of them have the same form of input. Examples are 'fid1 or fid2 or fid3' or "fid1, fid2, fid3", where fid1,fid2 and fid3 are file IDs.
}
\value{
A vector of file names, with the side effect of updating files widget
with these file names.
}
\author{
HUANG Ronggui
}

\examples{
\dontrun{
QueryFile(or="1,2") ## files coded to code 1 or 2.
QueryFile(or="1", and="2") ## files coded to code 1 and 2.
QueryFile(or="1", not="2,3") ## files coded to code 1 but not either 2 or 2.
}
}
