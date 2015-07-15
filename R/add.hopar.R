#' Add prey information to a dataframe
#'
#' Add prey information to a dataframe
#'
#' @param faeduhopur xxx
#' @param flokkur xxx
#' @param col.names xxx
#' @param isl xxx
#' @param return.on.error xxx
#' @param hopur xxx
#' @param tmptable xxx
#' @param oracle xxx
#' @param hopar.col xxx
#' @export

add.hopar <- function(faeduhopur,
                      flokkur,
                      col.names=c("thyngd"),
                      isl=F,
                      return.on.error=F,
                      hopur=NULL,
                      tmptable=F,
                      oracle=fjolstOracle,
                      hopar.col=hopar.col){

					# 	Baeta seinna vid meltingarstigi o.fl.
  if(is.null(hopur)) {
    x <- match(col.names,hopar.col)
    ind <- c(1:length(x));ind<- ind[is.na(x)]
    if(length(ind) > 0) {
      x <- col.names(x)
      txt <- paste("Villa i add.hopar. Dalkur",x,"ekki til")
      print(txt)
      if(return.on.error) return(invisible())
    }

    x <- lesa.hopar(faeduhopur,flokkur$flokk.id,col.names=col.names,tmptable=tmptable,oracle=oracle)
  }
  else {
    x <- hopur
    faeduhopur <- unique(x$faeduhopur)
    i <- match("thyngd",names(x))
    i1 <- match("fjoldi",names(x))
    if(!is.na(i) & !is.na(i1)){
      col.names <- c("thyngd","fjoldi")
      x <- apply.shrink.dataframe(x,c("thyngd","fjoldi"),c("flokk.id","faeduhopur"),sum)
      names(x) <- c("flokk.id","faeduhopur","thyngd","fjoldi")
    }
    if(!is.na(i) & is.na(i1)) {
      col.names <- c("thyngd")
      x <- apply.shrink.dataframe(x,c("thyngd"),c("flokk.id","faeduhopur"),sum)
      names(x) <- c("flokk.id","faeduhopur","thyngd")
    }
    if(is.na(i) & !is.na(i1)){
      col.names <- c("fjoldi")
      x <- apply.shrink.dataframe(x,c("fjoldi"),c("flokk.id","faeduhopur"),sum)
      names(x) <- c("flokk.id","faeduhopur","fjoldi")
    }
    if(is.na(i) & is.na(i1)) {
      print("warning neither column thyngd nor fjoldi do exist")
      return(invisible())
    }
  }

  if(is.null(x)){
    x <- data.frame(matrix(0,2,length(col.names)+2))
    x[,1] <- c(flokkur[1,"flokk.id"],flokkur[1,"flokk.id"])
    x[,2] <- c(faeduhopur[1],faeduhopur[1])
  }
  ind <- match(x$flokk.id,flokkur$flokk.id)
  ind1 <- c(1:length(ind))
  ind1 <- ind1[!is.na(ind)]
  x <- x[ind1,] # adeins tau sem passa vid flokk
  ind <- ind[!is.na(ind)]
  rownr <- ind

					# 	finnar dalknr.

  ind1 <- match(x$faeduhopur,faeduhopur)

  if(!is.na(match("melt",col.names))) {
    ind2 <- x$melt;ind2[is.na(ind2)] <- 3
    ind <- match(c("thyngd","fjoldi"),col.names)
    inda <- ind[!is.na(ind)]; l <- length(inda)
    dlk.pr.tegund <- l*4
    outcome <- matrix(0,nrow(flokkur),length(faeduhopur)*dlk.pr.tegund)
    dalknr <- (ind1-1)*dlk.pr.tegund+ind2*l+1
    if(l == 2) { #baedi thyngd og fjoldi
      outcome <- fill.matrix(outcome,x$thyngd,rownr,dalknr)
      outcome <- fill.matrix(outcome,x$fjoldi,rownr,dalknr+1)
    }
    else if(!is.na(ind[1])) # bara thyngd
    outcome <- fill.matrix(outcome,x$thyngd,rownr,dalknr)
    else if(!is.na(ind[2]))
      outcome <- fill.matrix(outcome,x$fjoldi,rownr,dalknr)

  }
  else { # ekki meltingarstig
    ind <- match(c("thyngd","fjoldi"),col.names)
    inda <- ind[!is.na(ind)]; l <- length(inda)
    dalknr <- l*(ind1-1)+1
    dalk.pr.tegund <- l
    outcome <- matrix(0,nrow(flokkur),length(faeduhopur)*dalk.pr.tegund)
    if(l == 2) { # baedi thyngd og fjoldi
      outcome <- fill.matrix(outcome,x$thyngd,rownr,dalknr)
      outcome <- fill.matrix(outcome,x$fjoldi,rownr,dalknr+1)
    }
    else if(!is.na(ind[1])) # bara thyngd
    outcome <- fill.matrix(outcome,x$thyngd,rownr,dalknr)
    else if(!is.na(ind[2]))
      outcome <- fill.matrix(outcome,x$fjoldi,rownr,dalknr)
  }
  if(!is.na(match("melt",col.names))){
    for( i in 1:length(faeduhopur)){
      cnr <- rep(0,4)
      cnr[1] <- dlk.pr.tegund*(i-1)+4*l-1
      cnr[2] <- cnr[1]-l;cnr[3] <- cnr[1]-2*l;cnr[4] <- cnr[1]-3*l
      outcome <- dalk.sum(outcome,cnr[1],cnr[2:4])
      if(l==2)
	cnr <- cnr+1
      outcome <- dalk.sum(outcome,cnr[1],cnr[2:4])
    }
  }


					#	Finna dalkanofn

  if(!is.na(match("melt",col.names))) {
    ind <- match(c("thyngd","fjoldi"),col.names)
    meltext <- c(".0",".fj.0",".1",".fj.1",".2",".fj.2","",".fj")
    if(is.na(match("fjoldi",col.names)))meltext <- meltext[c(1,3,5,7)]
    if(is.na(match("thyngd",col.names)))meltext <- meltext[c(2,4,6,8)]
  }
  else {
    meltext <- c("",".fj")
    if(is.na(match("fjoldi",col.names)))meltext <- meltext[1]
    if(is.na(match("thyngd",col.names)))meltext <- meltext[2]
  }
  if(isl) faeduhopur <- translate(faeduhopur,allir.hopar) # nafn a islensku
  txt <- rep(faeduhopur,length(meltext))
  txt <- c(t(matrix(faeduhopur,length(faeduhopur),length(meltext))))
  txt1 <- rep(meltext,length(faeduhopur))
  txt <- paste(txt,txt1,sep="")

  outcome <- data.frame(outcome)
  names(outcome) <- txt

  flokkur <- join.data.frame(flokkur,outcome)
  row.names(flokkur) <- flokkur$flokk.id
  return(flokkur)
}
