and <- function(CT1,CT2,showCoding=FALSE, method= c("overlap","exact","inclusion")){
    ## CT1 and CT2 is from GetCodingTable
    ## for one code and one file only

    and_helper <- function(CT1,CT2){
        ridx <- vector()
        idx <- vector()
        for (i in 1:nrow(CT1)) {
            for (j in 1:nrow(CT2)){
                rel <- relation(as.numeric(CT1[i,c("index1","index2")]),as.numeric(CT2[j,c("index1","index2")]))
                if (rel$Relation %in% method){
                    ridx <- c(ridx,i,j)
                    idx <- rel$OverlapIndex
                }
            }
        }
        if (length(ridx) >=2){
            rf <- ridx[seq(from=1,to=length(ridx),by=2)] ## first row index
            rs <- ridx[seq(from=2,to=length(ridx),by=2)] ## second row index
            index1 <- idx[seq(from=1,to=length(idx),by=2)]
            index2 <- idx[seq(from=2,to=length(idx),by=2)]
            ans <- cbind(CT1[rf,c("fid","filename")],index1=index1,index2=index2)
            ans
        }}

    fid <- intersect(CT1$fid,CT2$fid)
    if (length(fid)>0) {
        ans <- lapply(fid,FUN=function(x) and_helper(CT1=subset(CT1,fid==x),CT2=subset(CT2,fid==x)))
        ans <- do.call(rbind,ans)
        if (showCoding && !is.null(ans)){
        txt <- apply(ans,1,function(x){
            txt <- RQDAQuery(sprintf("select file from source where id==%s",x[["fid"]]))[1,1]
            Encoding(txt) <- "UTF-8"
            ans <- substr(txt, as.numeric(x[["index1"]]), as.numeric(x[["index2"]]))
            ans
        })
        ans$coding <- txt
    }
        ans
    } else NULL
}

##a<- GetCodingTable()
##c1 <- subset(a,cid==6)
##c2 <- subset(a,cid==24)
##and(c1,c2,T)
