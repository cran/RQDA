getCodingsOfCodes <- function(fid = NULL, codingTable = c("coding", "coding2")){
    codes <- RQDA:::.rqda$.codes_rqda[]
    Encoding(codes) <- "UTF-8"
    selected <- gselect.list(codes)
    selected <- RQDA:::enc(selected)
    cid <- RQDAQuery(sprintf("select id from freecode where status=1 and name in (%s)",paste( paste("'", selected, "'", sep=""), collapse = ",")))$id
    codingTable <- match.arg(codingTable)
    if (codingTable == "coding") {
        ct <- RQDAQuery(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding.selfirst as index1,
coding.selend as index2, coding.seltext as coding, coding.selend - coding.selfirst as CodingLength from coding
 left join freecode on (coding.cid=freecode.id) left join source on (coding.fid=source.id)
where coding.status=1 and source.status=1 and freecode.status=1 and coding.cid in (%s)", paste(cid,collapse=",")))
    }
    if (codingTable == "coding2") {
        ct <- RQDAQuery(sprintf("select coding.rowid as rowid, coding.cid, coding.fid,
freecode.name as codename, source.name as filename, coding2.selfirst as index1,
coding2.selend as index2, coding2.seltext as coding2, coding2.selend - coding2.selfirst as CodingLength from coding2
 left join freecode on (coding2.cid=freecode.id) left join source on (coding2.fid=source.id)
where coding2.status=1 and source.status=1 and freecode.status=1 and coding2.cid in (%s)", paste(cid,collapse=",")))
    }
    if (nrow(ct) != 0) {
        Encoding(ct$codename) <- Encoding(ct$filename) <- Encoding(ct$coding) <- "UTF-8"
        if (!is.null(fid))
            ct <- ct[ct$fid %in% fid, ]
    }
    ct ## can be printed via print.CodingsByOne
}
