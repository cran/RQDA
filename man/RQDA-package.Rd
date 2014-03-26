\name{RQDA-package}
\alias{RQDA-package}
\alias{RQDA}
\docType{package}
\title{
  R-based Qualitative Data Analysis package
}
\description{
  Qualitative Data Analysis based on R language. Current version
  supports plain text. In addition, it can import PDF highlights.
}
\details{
\tabular{ll}{
Package: \tab RQDA\cr
Type: \tab Package\cr
Version: \tab 0.2-5\cr
Date: \tab 2014-03-26\cr
Depends: \tab R (>= 2.8.0), DBI, RSQLite, gWidgets (>= 0.0-31),
gWidgetsRGtk2 (>= 0.0-36), igraph \cr
Imports: \tab RGtk2 (>= 2.20), methods \cr
Enhances: \tab RMySQL, rjpod, rpdfclown, tcltk \cr
License: \tab New style BSD License\cr
LazyLoad: \tab yes\cr
URL: \tab \url{http://rqda.r-forge.r-project.org/} \cr
}
The workhorse function for end-user is the RQDA(), you can use RQDA() to
start the GUI after library(QRDA). Please Refer to the documentation
section of the project homepage for the usage of RQDA.

The position of ViewFile widget can be controlled by "widgetCoordinate"
options, with default value c(400,2). You can change it by
options("widgetCoordinate"=c(x,y)), where x and y is integer specifying the
position.

The size of many widgets (e.g. ViewFile widgets) can be controlled by "widgetSize"
options, with default value c(550,700). You can change it by
options("widgetSize"=c(x,y)), where x and y is integer specifying width and height.
}
\author{
  Huang Ronggui
  
  Maintainer: Huang <ronggui.huang@gmail.com>
}

\references{
Kelle, U. (ed). 1995. "Computer-aided qualitative data analysis: theory,
methods and practice." Sage Publications.

Lewins, A. & Silver, C.2007. Using Software in Qualitative Research : A
Step-by-Step Guide. Sage Publications.
}
\keyword{ package }
\examples{
\dontrun{library(RQDA)
RQDA()
}
}
