#' Calculate number of fish behind (not in) each food sample.
#'
#' @param flokkur xxx
#' @param teg xxx
#' @param option xxx
#' @param lengdir xxx
#' @param numer xxx
#' @param lenfl xxx
#'
#' @export

fjoldi.i.flokkum  <- function(flokkur,
                              teg,
                              option=1,
                              lengdir,
                              numer,
                              lenfl=old.stomach.breaks$breaks,
                              oracle=fjolstOracle) {
  synis.id <- unique(flokkur$synis.id)
  if(missing(lengdir)) lengdir <- lesa.lengdir(synis.id,teg,oracle=oracle)
  if(length(lengdir)==0) return(flokkur)
  if(missing(numer)) numer <- lesa.numer(synis.id,teg,oracle=oracle)
  numer$rat <- (numer$fj.maelt+numer$fj.talid)/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 0
  lengdir <- fjolst:::join(lengdir,numer,"synis.id")
  lengdir$fjoldi <- lengdir$fjoldi*lengdir$rat
  flokkur$fjoldi <- rep(-999999,nrow(flokkur)) # var na en vandræði
  if(option==1) {
    lengdir$fjoldi[is.na(lengdir$fjoldi)] <- 0
    ind <- cut(lengdir$lengd,lenfl)
    lengdir <- lengdir[!is.na(ind),]
    lengdir <- tapply(lengdir$fjoldi, list(lengdir$synis.id, cut(lengdir$lengd, lenfl)), sum)
  }
  else  lengdir <- tapply(lengdir$fjoldi, list(lengdir$synis.id, lengdir$lengd), sum)
  cn <- dimnames(lengdir)[[2]]
  rn<- dimnames(lengdir)[[1]]
  ind <- match(rn,synis.id)
  rn <- synis.id[ind]
  lr <- length(rn);lc <- length(cn)
  rn <- c(matrix(rn,lr,lc))
  cn <- c(t(matrix(cn,lc,lr)))
  rn <- paste(rn,cn,sep="")
  lengdir <- c(lengdir)
  if(option==1) 	l <- paste(flokkur$synis.id,flokkur$lenfl,sep="")
  else  l <- paste(flokkur$synis.id,flokkur$lengd,sep="")
  ind <- match(l,rn)
  ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
  ind <- ind[!is.na(ind)]
  flokkur$fjoldi[ind1] <- lengdir[ind]
  i <- flokkur$fjoldi==-999999;flokkur$fjoldi[i] <- NA
  return(flokkur)
}
