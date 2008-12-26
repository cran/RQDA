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
Version: \tab 0.1-6\cr
Date: \tab 2008-12-26\cr
Depends: \tab DBI, RSQLite, RGtk2, gWidgets, gWidgetsRGtk2\cr
License: \tab New style BSD License\cr
LazyLoad: \tab yes\cr
URL: \tab \url{http://rqda.r-forge.r-project.org/} \cr
}
%% description of the package
%% main functions.
The workhorse function for end-user is the RQDA(), you can use RQDA() to
start the GUI after library(QRDA). Please Refer to the documentation
section of the project homepage for the usage of RQDA.}
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
%%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%%}
\examples{
\dontrun{library(RQDA)
RQDA()
}
}
