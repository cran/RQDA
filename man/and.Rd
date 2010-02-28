\name{and}
\alias{and}
\title{
  Boolean operation on codings.
}
\description{
  Return codings belong to two codes.
}
\usage{
and(CT1, CT2, showCoding = FALSE, method = c("overlap", "exact", "inclusion"))
}
\arguments{
  \item{CT1}{
    Coding of code one.
  }
  \item{CT2}{
    Coding of code two.
  }
  \item{showCoding}{
    Logic, return resulting coding if TRUE.
  }
  \item{method}{
    a character vector, used to define the operation of and.
  }
}
\details{
  When the relation of two coding is element of method argument, then
  the overlapping coding is the result of the and operator.
}
\value{
  a data frame if there is overlapping coding. Otherwise, it is NULL.
}
\author{
  Ronggui HUANG
}
\seealso{
  \code{\link{relation}}, \code{\link{GetCodingTable}}
}
\examples{
\dontrun{
a <- GetCodingTable()
c1 <- subset(a,cid==6)
c2 <- subset(a,cid==24)
and(c1,c2,T)
}}
