#' catch per station
#'
#' @param sfile xxx
#' @param lwcoeff xxx
#' @param teg xxx
#' @param lfile xxx
#' @param minl xxx
#' @param maxl xxx
#' @param oracle xxx
#' @param name xxx
#' @export afli.per.stod

afli.per.stod <- function(sfile,
                          lwcoeff = c(0.01, 3.),
                          teg = 1.,
                          lfile,
                          minl,
                          maxl,
                          oracle=fjolstOracle,
                          name="afli")
{
  if(missing(lfile))
    lfile <- lesa.lengdir(sfile$synis.id, teg,oracle=oracle)
  nfile <- lesa.numer(sfile$synis.id, teg,oracle=oracle)
  if(nrow(nfile)==0) {
    sfile[,name] <- 0
    return(sfile)
  }
  nfile$rat <- 1. + nfile$fj.talid/nfile$fj.maelt
  nfile$rat[is.na(nfile$rat)] <- 1.
  lfile <- join(lfile, nfile[, c("synis.id", "rat")], "synis.id")
  lfile$wt <- (lfile$fjoldi * lfile$rat * lwcoeff[1.] * lfile$lengd^
                 lwcoeff[2.])/1000.
  if(!missing(minl)) lfile <- lfile[lfile$lengd >= minl,]
  if(!missing(maxl)) lfile <- lfile[lfile$lengd <= maxl,]

  tmp <- apply.shrink(lfile$wt, lfile$synis.id, sum)
  names(tmp) <- c("synis.id", name)
  sfile <- join(sfile, tmp, "synis.id", set = 0.)
  return(sfile)
}
