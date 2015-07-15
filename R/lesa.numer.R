#' Read fro the table fiskar.numer or the dataframe all.nu.
#'
#' @param synis.id xxx
#' @param teg xxx
#' @param vigt xxx
#' @param col.names xxx
#' @param leidrett xxx
#' @param oracle xxx
#' @param numer.col xxx
#' @param numer.col.oracle xxx
#'
#' @export

lesa.numer <- function(synis.id,
                       teg,
                       vigt=F,
                       col.names=NULL,
                       leidrett=F,
                       oracle=fjolstOracle,
                       numer.col = numer.col,
                       numer.col.oracle = numer.col.oracle) {
  if(!oracle) {
    if(is.null(col.names)) colnames <- c("synis.id","fj.maelt","fj.talid")
    if(!is.null(synis.id))
      nu <- all.nu[!is.na(match(all.nu$synis.id,synis.id)) & all.nu$tegund==teg,]
    else
      nu <- all.nu[all.nu$tegund==teg,]
    return(nu)
  }
  col.names <- unique(c("synis.id","fj.maelt","fj.talid",col.names))
  if(leidrett) table <- "fiskar.leidr_numer"
  if(!leidrett) table <- "fiskar.numer"
  if(!is.null(synis.id)) synis.id <- as.numeric(synis.id)
  if(vigt){
    txt <- c("synis_id","fj_maelt","fj_talid",
             "fj_vigtad","vigt_synis","afli")
    txt <- paste(txt,collapse=",")
    txt1 <- c("synis.id","fj.maelt","fj.talid",
              "fj.vigtad","vigt.synis","afli")
  }
  else {
    txt <- c("synis_id","fj_maelt","fj_talid")
    txt <- paste(txt,collapse=",")
    txt1 <- c("synis.id","fj.maelt","fj.talid")
  }
  if( ! is.null(col.names)) { # velja dalka
    ind <- match(col.names,numer.col)
    if(length(ind[is.na(ind)]) >0) { # dalkur ekki til
      ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
      print(paste(col.names[ind]," er ekki til"))
      return(invisible())
    }
    col.names <- unique(c(c("synis.id","fj.talid","fj.maelt"),col.names))
    ind <- match(col.names,numer.col)
    txt <- numer.col.oracle[ind]
    txt1 <- col.names
    txt <- paste(txt,collapse=",")
  }
  skipun <- paste("select",txt,"from",table ,"where tegund = ",teg)
  x <- run.sqlskipun(skipun,synis.id,"synis_id")
  x$fj.talid[is.na(x$fj.talid)] <- 0
  row.names(x) <- x$synis.id
  return(x)
}
