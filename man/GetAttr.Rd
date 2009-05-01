\name{GetAttr}
\alias{GetAttr}
\alias{ShowSubset}
\title{Attributes}
\description{
Get the attributes of case or file.
}
\usage{
GetAttr(type = c("case", "file"))
ShowSubset(x)
}
\arguments{
  \item{type}{Type of attributes.}
  \item{x}{an object from \code{GetAttr}}
}
\details{
You can add and modify the attributes of cases or files. \code{GetAttr}
returns this attributes as a data frame.

Sometimes, you only want to show a subset of files or cases according to
their attributes. You can do the subset operation of the result from
\code{GetAttr} and pass it to \code{ShowSubset}.
}
\value{
  For \code{GetAttr}, when type is "case", it is a data frame with class
  of "CaseAttr"; when type is "file", it is a data frame with class of
  "FileAttr". For \code{ShowSubset}, no value is returned, the
  side-effect is to change the file list or case list in respective
  widget.
}
\author{HUANG Ronggui}
\note{
All the variables in the data frame is of class "character", you need to
convert to suitable class when conduct statistical analysis.
}
\examples{
\dontrun{
attr <- GetAttr("case")
ShowSubset(subset(attr,attribute1==1)) ## assuming there is a variable
named atttribute1 in attr.
}
}

