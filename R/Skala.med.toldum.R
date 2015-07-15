#' Scale length measurements by number counted.
#'
#' @param lengdir xxx
#' @param numer xxx
#' @param tegund xxx
#'
#' @export

Skala.med.toldum <- function(lengdir,
                             numer,
                             tegund) {
  if(missing(numer))
    numer <- lesa.numer(synis.id=unique(lengdir$synis.id),teg=tegund)
  numer$rat <- 1 + numer$fj.talid/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 1
  i <- match("rat",names(lengdir))
  if(!is.na(i)) lengdir <- lengdir[,-i]
  l1 <- join(lengdir,numer[,c("synis.id","rat")],"synis.id")
  i <- is.na(l1$rat)
  if(any(i)) l1$rat[i] <- 1
  lengdir$fj.alls <- l1$fjoldi*l1$rat
  return(lengdir)
}
