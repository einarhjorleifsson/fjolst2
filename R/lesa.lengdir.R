lesa.lengdir <-
function(synis.id,teg,col.names=c("synis.id","lengd","fjoldi"),kyn=NULL,kynthroski=NULL,lengd=NULL,leidrett=F,oracle=fjolstOracle) {
  if(!oracle) {
    if(!exists("all.le"))
      attach(paste(fjolstlib,"/ExtraStuff/all.le.rda",sep=""))    
    if(!is.null(kyn)) col.names <- c(col.names,"kyn")
    if(!is.null(kynthroski)) col.names=c(col.names,"kynthroski")
    col.names <- unique(c("synis.id","lengd","fjoldi",col.names))
    if(is.data.frame(all.le)){
      if(!is.null(synis.id)) 
        le <- all.le[all.le$tegund %in% teg & all.le$synis.id %in% synis.id ,col.names]
      else 
        le <- all.le[all.le$tegund %in% teg,col.names]
    }
    else if(is.list(all.le)) {
      if(!is.null(synis.id)) 
        le <- all.le[[as.character(teg)]][!is.na(match(all.le[[as.character(teg)]]$synis.id,synis.id)),col.names]
      else 
        le <- all.le[[as.character(teg)]][,col.names]
    }
    return(le)

  }
              
  if(leidrett) table <- "fiskar.leidr_lengdir"
  if(!leidrett) table <- "fiskar.lengdir"
  col.names <- unique(c("synis.id","lengd","fjoldi",col.names))
  if(!is.null(kynthroski)) {
    kynthroski <- paste(kynthroski,collapse=",")
  }
    
  if(!is.null(lengd)) lengd <- paste(lengd,collapse=",")

  if(!is.null(synis.id)) synis.id <- sort(as.numeric(synis.id))
  
  ind <- match(col.names,lengdir.col) 
  if(length(ind[is.na(ind)])>0) {
    ind1 <-c(1:length(ind));ind1 <- ind1[is.na(ind)]
    print(paste("dálkur",col.names[ind],"er ekki til"))
    return(invisible())
  }
  cn <- lengdir.col.oracle[ind]
  cn <- paste(cn,collapse=",")
  skipun <- paste("select",cn,"from",table,"where tegund =",teg)  
# Sleppa þessu línum þær henta ekki.  
#  if(!is.null(kynthroski)) skipun <- paste(skipun,"and kynthroski is not NULL and kynthroski in (",kynthroski,")")
  if(!is.null(lengd)) skipun <- paste(skipun,"and lengd is not NULL and lengd in (",lengd,")")

#  if(!is.null(kyn)) skipun <- paste(skipun,"and kyn is not NULL and kyn =",kyn)
  
  x <- run.sqlskipun(skipun,synis.id,"synis_id") 

  return(x)
}
