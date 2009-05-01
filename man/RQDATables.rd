\name{RQDATables}
\alias{RQDATables}
\title{Data Tables in rqda file}
\description{
The internal data table structures in rqda file, which is a SQLite data base.
}
\details{
  Table "attributes" contatins information about the name list of attributes.
  \tabular{ll}{
    name:\tab name of attributes. \cr 
    status:\tab .\cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    owner:\tab . \cr 
    memo:\tab . \cr 
  }
  
  Table "caseAttr" contatins information about attributes of cases.
  \tabular{ll}{
    variable:\tab name of case attributes, coresponding to name in
  attributes table. \cr
    value:\tab . \cr 
    caseID:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    owner:\tab . \cr 
  }
  
  Table "caselinkage" contatins information about the relationship
  between case and files of case.
  \tabular{ll}{
    caseid:\tab . \cr 
    fid:\tab . \cr 
    selfirst:\tab . \cr 
    selend:\tab . \cr 
    status:\tab . \cr 
    owner:\tab . \cr 
    date:\tab . \cr 
    memo:\tab . \cr 
  }
  
  Table "cases" contatins information about case list.
  \tabular{ll}{
    name:\tab . \cr 
    memo:\tab . \cr 
    owner:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    id:\tab . \cr 
    status:\tab . \cr }
  
  Table "codecat" contatins information about upper-level of code list.
  \tabular{ll}{
    name:\tab . \cr 
    cid:\tab . \cr 
    catid:\tab . \cr 
    owner:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    memo:\tab . \cr 
    status:\tab . \cr 
  }
  
  Table "coding" contains information on codings.
  \tabular{ll}{
    cid :\tab \cr 
    fid :\tab \cr 
    seltext :\tab \cr 
    selfirst :\tab \cr 
    selend :\tab \cr 
    status :\tab \cr 
    owner :\tab \cr 
    date :\tab \cr 
    memo :\tab \cr 
  }

  Table "fileAttr" contatins information about attributes of files.
  \tabular{ll}{
    variable:\tab . \cr 
    value:\tab . \cr 
    fileID:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    owner:\tab . \cr 
  }
  
  Table "filecat" contains information on the file categorization.
  \tabular{ll}{
    name:\tab name of the file category.\cr
    fid:\tab Not used.\cr
    catid:\tab if of file category.\cr
    owner:\tab creator of file-category.\cr
    date:\tab \cr
    dateM:\tab \cr
    memo:\tab \cr
    status:\tab \cr
  }

  Table "freecode" contains information on the codes list.
  \tabular{ll}{
    name :\tab \cr 
    memo :\tab \cr 
    owner :\tab \cr 
    date :\tab \cr 
    dateM :\tab \cr 
    id :\tab \cr 
    status :\tab \cr
  }
  
  Table "journal" contatins information about field work journal.
  \tabular{ll}{
    name:\tab . \cr 
    journal:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    owner:\tab . \cr 
    status:\tab . \cr 
  }
  
  Table "project" contatins information about the project and *.rqda file.
  \tabular{ll}{
    encoding:\tab . \cr 
    databaseversion:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    memo:\tab . \cr 
    BOM:\tab . \cr 
  }
  
  Table "source" contains the content of files.
  \tabular{ll}{
    name:\tab name of the file.\cr
    id:\tab id of the file.\cr
    file:\tab content of a file.\cr
    memo:\tab memo of the file.\cr
    owner:\tab creator the the file.\cr
    date:\tab the date of the file-import.\cr
    dataM:\tab not used now.\cr
    status:\tab 1 for standard status and 0 for temporarily deleted file.\cr
  }
  
  The "treecode" table contains information on the codes categorization
  (relationship between codes and the codecat).
  \tabular{ll}{
    cid:\tab . \cr 
    catid:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    memo:\tab . \cr 
    status:\tab . \cr 
  }

  Table "treefile" contatins information about file categorization
  (relation between source files and filecat).
  \tabular{ll}{
    fid:\tab . \cr 
    catid:\tab . \cr 
    date:\tab . \cr 
    dateM:\tab . \cr 
    memo:\tab . \cr 
    status:\tab . \cr 
  }
  
}
\author{ HUANG Ronggui }
