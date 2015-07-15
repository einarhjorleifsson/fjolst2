#' Read from the table faeda.f_hopar or the dataframe fhopar
#'
#' @param faeduhopur xxx
#' @param flokk.id xxx
#' @param col.names xxx
#' @param return.on.error xxx
#' @param tmptable xxx
#' @param oracle xxx
#'
#' @export

lesa.hopar <- function(faeduhopur,
                       flokk.id=NULL,
                       col.names=NULL,
                       return.on.error=F,
                       tmptable=F,
                       oracle=fjolstOracle) {
  if(!oracle) {
    if(is.null(col.names)) col.names <- c("flokk.id","faeduhopur","thyngd","fjoldi")
    fjth <- c("thyngd","fjoldi")
    if(is.na(match("fjoldi",col.names))) fjth <- fjth[-2]
    if(is.na(match("thyngd",col.names))) fjth <- fjth[-1]

    result <- fhopar[!is.na(match(fhopar$faeduhopur,faeduhopur)),]
    print(nrow(result))
    print(fjth)
    if(!is.null(flokk.id)) result <- result[!is.na(match(result$flokk.id,flokk.id)),]
    result$fjoldi[is.na(result$fjoldi)] <- 0
    result$thyngd[is.na(result$thyngd)] <- 0
    print(nrow(result))

    if(!is.na(match("meltingarstig",col.names))) {
      x <- apply.shrink.dataframe(result,fjth,c("flokk.id","faeduhopur","meltingarstig"),sum)
      names(x)[4:(4+length(fjth)-1)] <- fjth
    }
    x <- apply.shrink.dataframe(result,fjth,c("flokk.id","faeduhopur"),sum)
    names(x)[3:(3+length(fjth)-1)] <- fjth
    return(x)
  }
  nafn <- c("flokk.id","faeduhopur","fjoldi","thyngd","meltingarstig")
  if(is.null(col.names) || !is.na(match("meltingarstig",col.names)))
    melt <- T
  else
    melt <- F
  nafn1 <- c("flokk_id","faeduhopur","sum(NVL(fjoldi,0)) fjoldi","sum(NVL(thyngd,0)) thyngd","meltingarstig")
  if(!is.null(col.names)){
    x <- match(col.names,nafn)
    ind <- c(1:length(x));ind<- ind[is.na(x)]
    if(length(ind) > 0) {
      x <- col.names(x)
      txt <- paste("Villa i lesa.hopar. Dalkar",x,"ekki til")
      print(txt)
      return(invisible())
    }
    ind <- match(col.names,nafn)
    ind <- ind[!is.na(ind)]
    txt1 <- unique(c(c("flokk_id","faeduhopur"),nafn1[ind]))
    txt <- unique(c(c("flokk.id","faeduhopur"),nafn[ind]))
  }
  else {txt1 <- nafn1;txt	<- nafn}

  i <- match("thyngd",txt1)
  if(!is.na(i)) txt1[i] <- "sum(NVL(thyngd,0)) thyngd"
  i <- match("fjoldi",txt1)
  if(!is.na(i)) txt1[i] <- "sum(NVL(fjoldi,0)) fjoldi"


  txt1 <- paste(txt1,collapse=",")
  x <- match(faeduhopur,allir.hopar$faeduhopur)
  ind <- c(1:length(x));ind<- ind[is.na(x)]
  if(length(ind) > 0) {
    txt <- paste("Villa i lesa.hopar. Hopar",faeduhopur[ind],"ekki til")
    print(txt)
    if(return.on.error) return(invisible())
  }
  fh <- paste(faeduhopur,collapse="','")
  fh <- paste("'",fh,"'",sep="")
  if(tmptable)
    table <- "faeda.f_hopar_tmp"
  else
    table <- "faeda.f_hopar"
  skipun <- paste("select",txt1,"from",table,"where faeduhopur in (",fh,")")

  if(melt) txt2 <- "group by flokk_id,faeduhopur,meltingarstig"
  else txt2 <- "group by flokk_id,faeduhopur"
  x <- run.sqlskipun(skipun,flokk.id,"flokk_id","and",txt2)


  return(x)
}
