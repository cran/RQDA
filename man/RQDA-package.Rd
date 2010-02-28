\name{RQDA-package}
\alias{RQDA-package}
\alias{RQDA}
\docType{package}
\title{
  R-based Qualitative Data Analysis package
}
\description{
  Qualitative Data Analysis based on R language. Current version only supports plain text.
}
\details{
\tabular{ll}{
Package: \tab RQDA\cr
Type: \tab Package\cr
Version: \tab 0.1-9\cr
Date: \tab 2010-02-28\cr
Depends: \tab R (>= 2.8.0), DBI, RSQLite, gWidgets (>= 0.0-31),
gWidgetsRGtk2 (>= 0.0-36), igraph \cr
Imports: \tab RGtk2 (>= 2.12.12), methods \cr
Suggests: \tab tm, tcltk \cr
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
}
\author{
  Huang Ronggui
  
  Maintainer: Huang <ronggui.huang@gmail.com>
}

%%\references{
%% reference here
%%}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS ~~
%%~~ in the R documentation directory ~~
\keyword{ package }
\examples{
\dontrun{library(RQDA)
RQDA()
}
}
