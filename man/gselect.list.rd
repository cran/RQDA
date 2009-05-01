\name{gselect.list}
\alias{gselect.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Select Items from a List}
\description{
  Select item(s) from a character vector.
}
\usage{
gselect.list(list, multiple = TRUE, title = NULL, width = 200, height = 500,x=420,y=2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{ character vector. A list of items.}
  \item{multiple}{logical: can more than one item be selected?}
  \item{title}{optional character string for window title.}
  \item{width}{integer. width of the widget.}
  \item{height}{integer. heighth of the widget.}
  \item{x}{}
  \item{y}{}
  \item{\dots}{Not used currently.}
}
\details{
GTK version of \code{\link[utils]{select.list}}.
}
\note{
The license of this function is subject to interpretation of the first author.
}
\value{
  A character vector of selected items with encoding of UTF-8. If no item was selected (or
  'Cancel' was used), '""' is returned.
}
\author{John Verzani and Ronggui HUANG}
\seealso{\code{\link[utils]{select.list}}}
\examples{
\dontrun{
select.list(sort(.packages(all.available = TRUE)))
}
}

