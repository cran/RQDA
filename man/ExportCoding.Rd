\name{ExportCoding}
\Rdversion{1.1}
\alias{ExportCoding}
\title{
Export codings to a HTML file.
}
\description{
To export retrieved codings to a HTML file.
}
\usage{
ExportCoding(file = "Exported Codings.html", Fid = NULL, order = c("fname", "ftime", "ctime"), append = FALSE)
}
\arguments{
  \item{file}{
Length-one character vector, specify the name of exported file.
}
  \item{Fid}{
Integer vector of file id.The retrieved codings are from this subset of files. When is NULL, it means all the files.
}
  \item{order}{
Specify the order of retrieved codings. see details for the meanings.
}
  \item{append}{
Logical, when TRUE the exported codings are appended to the existing file (if exists); otherwise, it overwrites the existing file.
}
}
\details{
"fname" means order the codings by file names, "ftime" by file imported time, and "ctime" by time of coding.
}
\value{
A html file.
}
\author{
HUANG Ronggui
}
