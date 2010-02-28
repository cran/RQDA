\name{RQDA2tm}
\alias{RQDA2tm}
\title{RQDA to Corpus}
\description{
  Constructs a text document collection (corpus) from RQDA codings.
}
\usage{
RQDA2tm(Code, language = "french")
}
\arguments{
  \item{Code}{character vector of length 1. name of a code in RQDA.}
  \item{language}{argument passed to \code{\link[tm]{Corpus}}}
}
\details{
To retrieve the codings of \code{Code}, and turn the codings into a corpus.
}
\value{
  An S4 object of class \code{Corpus}.
}
\author{J-P Mueller and documented by HUANG Ronggui}
\seealso{\code{\link[tm]{Corpus}}}
