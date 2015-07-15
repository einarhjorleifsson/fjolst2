# All stuff borrowed from geo

#' Apply a function to a vector for a combination of categories.
#'
#' Apply a function to a vector for a combination of categories.
#'
#'
#' @param X Input data to \code{FUN}
#' @param INDICES list of categories to be combined
#' @param FUN Function to be applied
#' @param names Column names for the resulting dataframe
#' @param \dots Additional arguments to \code{FUN}
#' @return Dataframe of outcomes applying \code{FUN} to \code{X} for the
#' combination of categories in \code{INDICES}
#' @note Needs elaboration, or could be dropped/hidden, use merge instead.
#' @seealso \code{\link{apply.shrink.dataframe}}, \code{\link{merge}}
#' @keywords manip
#'
apply.shrink <-
  function(X, INDICES, FUN = NULL, names, ...)
  {
    # GJ 9/94.
    # 'apply.shrink' is identical to 'tapply' (see tapply).
    # But it returns a data.frame were each 'index' represent a column
    # and an extra column for the result of evaluating FUN for the partation
    # on X given by the INDICES.
    if(missing(FUN)) stop(
      "No function to apply to data given (missing argument FUN)"
    )
    if(!is.list(INDICES))
      INDICES <- list(INDICES)
    len.data <- length(X)
    all.indices <- rep(0., len.data)
    for(i in rev(INDICES)) {
      # combine all indices to one
      if(length(i) != len.data) stop(
        "Data and all indices must have same length")
      i <- as.factor(i)
      #		i <- as.category(i)
      all.indices <- all.indices * length(levels(i)) + (as.vector(
        unclass(i)) - 1.)
    }
    # one-origin
    all.indices <- all.indices + 1.
    INDICES <- as.data.frame(INDICES)
    INDICES <- INDICES[match(sort(unique(all.indices)), all.indices,
                             nomatch = 0.),  ]
    if(is.character(FUN))
      FUN <- getFunction(FUN)
    else if(mode(FUN) != "function") {
      farg <- substitute(FUN)
      if(mode(farg) == "name")
        FUN <- getFunction(farg)
      else stop(paste("\"", farg, "\" is not a function", sep = ""))
    }
    X <- split(X, all.indices)
    X.apply <- lapply(X, FUN, ...)
    numb.FUN.value <- length(X.apply[[1.]])
    if(numb.FUN.value == 1.)
      X.apply <- data.frame(X = unlist(X.apply))
    else X.apply <- data.frame(matrix(unlist(X.apply), ncol =
                                        numb.FUN.value, byrow = T, dimnames = list(NULL, names(
                                          X.apply[[1.]]))))
    X.apply <- cbind(INDICES, X.apply)
    if(!missing(names))
      names(X.apply) <- names
    return(X.apply)
  }

#' Apply functions to columns in a dataframe
#'
#' \code{apply.shrink} for a dataframe, different functions can be applied to
#' different columns, one for each column (?).
#'
#'
#' @param data Input dataframe
#' @param name.x Input value columns
#' @param name.ind Category columns
#' @param FUNS Functions to apply
#' @param NA.rm na-action, default FALSE
#' @param resp.name ???
#' @param full.data.frame ???
#' @param Set ?
#' @param name.res User selected values for the results columns
#' @param \dots Additional arguments (to what???
#' @return Dataframe of variouse outcomes.
#' @note Needs elaboration, although \dots{} are included in the arguments list
#' they don't seem to be used anywhere.
#' @seealso \code{\link{apply.shrink}}, \code{\link{merge}}
#' @keywords manip

apply.shrink.dataframe <-
  function(data, name.x, name.ind, FUNS = NULL, NA.rm = FALSE, resp.name = NULL,
           full.data.frame = FALSE, Set = NA, name.res, ...)
  {
    COUNT <- function(x)
      return(length(x))
    FUNS <- as.character(substitute(FUNS))
    if(!is.na(match(FUNS[1], "c")))
      FUNS <- FUNS[2:length(FUNS)]
    i <- match(name.ind, names(data))
    if(any(is.na(i))) {
      i1 <- c(1.:length(i))
      i1 <- i1[is.na(i)]
      stop(paste("Column", name.ind[i1], "does not exist"))
    }
    i <- match(name.x, c(names(data), "NR"))
    if(any(is.na(i))) {
      i1 <- c(1.:length(i))
      i1 <- i1[is.na(i)]
      stop(paste("Column", name.x[i1], "does not exist"))
    }
    data$NR <- rep(1., nrow(data))
    i <- match("", name.x)
    # Remove NA values
    if(!is.na(i)) name.x[i] <- "NR"
    i <- rep(1., nrow(data))
    if(NA.rm) {
      k <- match(name.x, names(data))
      for(j in 1.:length(name.x)) {
        if(is.numeric(data[, k[j]])) {
          i <- i & !is.na(data[, k[j]])
        }
      }
      data <- data[i,  ]
    }
    if(length(name.x) > 1 && length(FUNS) == 1)
      FUNS <- rep(FUNS, length(name.x))
    if(length(name.x)== 1 & length(FUNS) > 1) name.x <- rep(name.x,length(FUNS))
    if(missing(name.res))
      name.res <- paste(name.x, FUNS, sep = ".")
    name.res <- c(name.ind, name.res)
    indices <- list()
    for(i in 1:length(name.ind))
      indices[[i]] <- data[, name.ind[i]]
    if(full.data.frame) {
      x <- tapply(rep(1, nrow(data)), indices, sum)
      result <- expand.grid(dimnames(x))
      x <- c(x)
      j <- is.na(x)
      for(i in 1:length(FUNS)) {
        x <- c(tapply(data[, name.x[i]], indices, FUNS[i]))
        if(any(j))
          x[j] <- Set
        result <- cbind(result, x)
      }
    }
    else {
      for(i in 1:length(FUNS)) {
        x <- apply.shrink(data[, name.x[i]], indices, FUNS[
          i])
        if(i == 1)
          result <- x
        else result <- cbind(result, x[, ncol(x)])
      }
    }
    names(result) <- name.res
    return(result)
  }



#' Translate characters in column names
#'
#' Translate characters in column names
#'
#'
#' @param txt Character vector of inputs
#' @param char Character to be replaced
#' @param replchar Character to replace with
#' @return A character vector of the same length as \code{txt}.
#' @note Could be deprecated, use \code{\link{chartr}} instead.
#' @keywords manip

skipta.texta <-
  function(txt, char = "_", replchar = ".")
  {
    backsl <- "\\"
    tmpfile1 <- tempfile("splusskipt")
    tmpfile2 <- tempfile("splusskipt")
    tmpskipanaskra <- tempfile("splusskipun")
    on.exit(unlink(tmpfile1))
    on.exit(unlink(tmpfile2))
    on.exit(unlink(tmpskipanaskra))
    txt <- paste(txt, collapse = "\n")
    write(txt, file = tmpfile1)
    skipun <- paste("sed 's/", backsl, char, "/", backsl, replchar, "/g' <", tmpfile1, ">", tmpfile2, sep = "")
    write(skipun, file = tmpskipanaskra)
    system(paste("chmod u+x", tmpskipanaskra))
    system(tmpskipanaskra)
    txt <- scan(tmpfile2, what = character(), sep = "\t")
    return(txt)
    print(skipun)
  }

#' Relace elements of a matrix
#'
#' Replace (fill) elements of a matrix (or data.frame) with a value for given
#' pairs of row and column indices.
#'
#'
#' @param outcome Input matrix/data.frame
#' @param x Value or values to replace/fill with
#' @param rownr Row index/indices
#' @param dalknr Column index/indices
#' @return Matrix or data.frame with given values replaced.
#' @note Probably redundant, not called by any geo-function, the same effect
#' could be achieved with an assignment to a matrix with an index-matrix of the
#' values in rownr and dalknr: \code{mat[matrix(c(rownr, dalknr), ncol = 2)] <-
#' x}
#' @keywords manip

fill.matrix <-
  function(outcome, x, rownr, dalknr)
  {
    ind <- nrow(outcome) * (dalknr - 1) + rownr
    outcome[ind] <- x
    return(outcome)
  }

