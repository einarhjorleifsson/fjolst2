#' Internal use.
#'
#' @param skipun xxx
#' @param listi xxx
#' @param colname xxx
#' @param txt1 xxx
#' @param txt2 xxx
#'
#' @export

run.sqlskipun <- function(skipun,
                          listi,
                          colname="synis_id",
                          txt1="and",
                          txt2="") {
  x <- NULL
  if(is.null(listi) || length(listi) < 800) {
    if(!is.null(listi))
      skipun <- paste(skipun, txt1, colname, "in (", paste(
        listi, collapse = ","), ")", txt2)
    x <- ora::sql(skipun,dots=T)
  }
  else {
    listi <- sort(listi)
    len <- floor(length(listi)/800) + 1
    ind <- as.numeric(cut(c(1:length(listi)), len))
    for(i in 1:len) {
      s <- listi[ind == i]
      s <- paste(s, collapse = ",")
      skipun1 <- paste(skipun,txt1,colname,"in (",s,")",txt2)
      x1 <- ora::sql(skipun1)
      if(nrow(x1) > 0) x <- rbind(x,x1)
    }
  }
  return(x)
}
