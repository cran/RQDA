\name{Query}
\Rdversion{1.1}
\alias{Query}
\title{
Retrieval of file names according to their codings.
}
\description{
To retrieve file names according to their codings and the relationship of codings.
}
\usage{
Query(or, and = NULL, not = NULL)
}
\arguments{
  \item{or}{ code id.}
  \item{and}{ code id.}
  \item{not}{code id.}
}
\details{
This function is experimental, and may be changed in future.

For arguments of 'or', 'and' and 'not', all of them have the same form of input. Examples are '(fid1 or fid2 or fid3)' or "(fid1, fid2, fid3)", where fid1,fid2 and fid3 are file IDs.
}
\value{
A vector.
}
\author{
HUANG Ronggui
}

\examples{
\dontrun{
Query(or=(1,2)) ## files coded to code 1 or 2.

Query(or=(1), and=(2)) ## files coded to code 1 and 2.

Query(or=(1), not=(2,3)) ## files coded to code 1 but not either 2 or 2.
}
}

