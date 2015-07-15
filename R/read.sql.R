#' Read a file from sql++ with columns seperated by |
#'
#' @param file xxx
#' @param col.names xxx
#' @param stop.on.error xxx
#' @param ... xxx
#'
#' @export

read.sql <- function (file,
                      col.names,
                      stop.on.error = T,
                      ...)
{
  n <- count.fields(file, sep = "|")
  if (length(n) == 0) {
    if (stop.on.error)
      stop("Engin g\366gn fyrir read.sql.  Vantar sennilega g\366gn \355 Oracle e\360a skipun var r\366ng.  ")
    else return(NULL)
  }
  x <- read.table(file, col.names = c(col.names, "xxx"), sep = "|",
                  as.is = T, ...)
  x <- x[, 1:(ncol(x) - 1)]
  return(invisible(x))
}
