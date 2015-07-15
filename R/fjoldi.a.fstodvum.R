#' Calculate number in each lengthgroup at some stations.  (out of date
#' function)
#'
#' @param fstodvar xxx
#' @param teg xxx
#' @export

fjoldi.a.fstodvum <- function(fstodvar,
                              teg) {

  lengdir <- lengdar.tafla(fstodvar$synis.id,teg)
  row.names(lengdir)<- c(1:nrow(lengdir))
  len <- join.data.frame(fstodvar,lengdir)
  return(len)
}
