#' Calculate the number of a species at the list of stations.
#'
#' @param data xxx
#' @param tegund xxx
#' @param name xxx
#' @param oracle xxx
#'
#' @export

fjperstod <- function(data,
                      tegund,
                      name = "fjoldi",
                      oracle=Oracle)
{
  tmp <- lesa.numer(data$synis.id, tegund,oracle=oracle)
  i <- is.na(tmp$fj.talid)
  if(any(i))
    tmp$fj.talid[i] <- 0
  i <- is.na(tmp$fj.maelt)
  if(any(i))
    tmp$fj.maelt[i] <- 0
  tmp$fjoldi <- tmp$fj.maelt + tmp$fj.talid
  x <- apply.shrink(tmp$fjoldi, tmp$synis.id, sum, names = c("synis.id",
                                                             name))
  data <- fjolst:::join(data, x, "synis.id", set = 0)
  return(data)
}
