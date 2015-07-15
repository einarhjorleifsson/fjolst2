#' Read from the table fiskar.kvarnir or the dataframe all.kv.
#'
#' @param synis.id xxx
#' @param teg xxx
#' @param col.names xxx
#' @param aldur xxx
#' @param kyn xxx
#' @param kynthroski xxx
#' @param oslaegt xxx
#' @param slaegt xxx
#' @param lifur xxx
#' @param kynfaeri xxx
#' @param lengd xxx
#' @param leidrett xxx
#' @param oracle xxx
#' @param kvarnir.col xxx
#' @param kvarnir.col.oracle xxx
#'
#' @export

lesa.kvarnir <- function(synis.id,
                         teg,
                         col.names=c("synis.id","aldur","lengd"),
                         aldur=NULL,
                         kyn=NULL,
                         kynthroski=NULL,
                         oslaegt=F,
                         slaegt=F,
                         lifur=F,
                         kynfaeri=F,
                         lengd=NULL,
                         leidrett=F,
                         oracle=fjolstOracle,
                         kvarnir.col=kvarnir.col,
                         kvarnir.col.oracle = kvarnir.col.oracle) {
  col.names=unique(c("synis.id","lengd","aldur",col.names))
  if(!oracle) {
    #if(!exists("all.kv"))
    #  attach(paste(fjolstlib,"/ExtraStuff/all.kv.rda",sep=""))
    if(is.data.frame(all.kv)){
      if(!is.null(synis.id))
        kv <- all.kv[all.kv$tegund %in% teg & all.kv$synis.id %in% synis.id ,col.names]
      else
        kv <- all.kv[all.kv$tegund %in% teg,col.names]
    }
    else if(is.list(all.kv)) {
      if(!is.null(synis.id))
        kv <- all.kv[[as.character(teg)]][!is.na(match(all.kv[[as.character(teg)]]$synis.id,synis.id)),col.names]
      else
        kv <- all.kv[[as.character(teg)]][,col.names]
    }
    return(kv)

  }
  if(leidrett) table <- "fiskar.leidr_kvarnir"
  if(!leidrett) table <- "fiskar.kvarnir"
  col.names<-unique(c(c("synis.id","lengd","aldur"),col.names))
  if(!is.null(synis.id)) synis.id <- as.numeric(synis.id)

  if(!is.null(aldur)) aldur <- paste(aldur,collapse=",")
  if(!is.null(kynthroski)) {
    kynthroski <- paste(kynthroski,collapse=",")
  }
  if(!is.null(lengd)) lengd <- paste(lengd,collapse=",")
  if(oslaegt)
    col.names <- unique(c(col.names,"oslaegt"))
  if(slaegt)
    col.names <- unique(c(col.names,"slaegt"))
  if(lifur)
    col.names <- unique(c(col.names,"lifur"))
  if(kynfaeri)
    col.names <- unique(c(col.names,"kynfaeri"))
  ind <- match(col.names,kvarnir.col)
  if(length(ind[is.na(ind)]) >0) { # dalkur ekki til
    ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
    print(paste(col.names[ind]," er ekki til"))
    return(invisible())
  }
  cn <- kvarnir.col.oracle[ind]
  cn <- paste(cn,collapse=",")
  teg <- paste("'",teg,"'",sep="")
  skipun <- paste("select",cn,"from",table,"where tegund =",teg)
  if(!is.null(aldur)) skipun <- paste(skipun,"and aldur is not NULL and aldur in (",aldur,")")
  if(!is.null(kynthroski)) skipun <- paste(skipun,"and kynthroski is not NULL and kynthroski in (",kynthroski,")")
  if(!is.null(lengd)) skipun <- paste(skipun,"and lengd is not NULL and lengd in (",lengd,")")
  if(!is.null(kyn)) skipun <- paste(skipun,"and kyn is not NULL and kyn =",kyn)
  if(oslaegt) skipun <- paste(skipun,"and oslaegt is not NULL")
  if(slaegt) skipun <- paste(skipun,"and slaegt is not NULL")
  if(lifur) skipun <- paste(skipun,"and lifur is not NULL")
  if(kynfaeri) skipun <- paste(skipun,"and kynfaeri is not NULL")

  x <- run.sqlskipun(skipun,synis.id,"synis_id")
  return(x)
}
