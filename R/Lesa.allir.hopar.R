#' Read all records from the table f_hopar corresponding to the selected
#' samples.
#'
#' @param flokk.id xxx
#' @param oracle xxx
#'
#' @export

Lesa.allir.hopar <- function(flokk.id,
                             tmptable=F,
                             oracle=fjolstOracle) {
  result <- NULL
  if(!oracle) {
    if(!is.null(flokk.id)) result <- fhopar[!is.na(match(fhopar$flokk.id,flokk.id)),]
    result$fjoldi[is.na(result$fjoldi)] <- 0
    result$thyngd[is.na(result$thyngd)] <- 0
    x <- apply.shrink.dataframe(result,c("thyngd","fjoldi"),c("flokk.id","faeduhopur"),sum)
    names(x)[3:4] <- c("thyngd","fjoldi")
    x$faeduhopur <- as.character(x$faeduhopur)
    return(x)
  }

  flokk.id <- sort(flokk.id)
  if(tmptable)
    skipun <-  "select flokk_id,sum(NVL(thyngd,0)) thyngd,sum(NVL(fjoldi,0)) fjoldi,faeduhopur from faeda.f_hopar_tmp "
  else
    skipun <-  "select flokk_id,sum(NVL(thyngd,0)) thyngd,sum(NVL(fjoldi,0)) fjoldi,faeduhopur from faeda.f_hopar "

  x <- run.sqlskipun(skipun,flokk.id,"flokk_id","where","group by faeduhopur,flokk_id")
  x$faeduhopur <- as.character(x$faeduhopur)
  return(x)
}
