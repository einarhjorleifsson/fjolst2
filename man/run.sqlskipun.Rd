\name{run.sqlskipun}
\alias{run.sqlskipun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal use.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.sqlskipun(skipun, listi, colname = "synis_id", tmpskipanaskra, tmpskra, txt1 = "and", txt2 = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{skipun}{
%%     ~~Describe \code{skipun} here~~
}
  \item{listi}{
%%     ~~Describe \code{listi} here~~
}
  \item{colname}{
%%     ~~Describe \code{colname} here~~
}
  \item{tmpskipanaskra}{
%%     ~~Describe \code{tmpskipanaskra} here~~
}
  \item{tmpskra}{
%%     ~~Describe \code{tmpskra} here~~
}
  \item{txt1}{
%%     ~~Describe \code{txt1} here~~
}
  \item{txt2}{
%%     ~~Describe \code{txt2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(skipun,listi,colname="synis_id",tmpskipanaskra,tmpskra,txt1="and",txt2="") {
  if(is.null(listi)|| length(listi) < 150) {
    if(!is.null(listi)) skipun <- paste(skipun,txt1,colname,"in (",paste(listi,collapse=","),")",txt2)
    write(skipun,file=tmpskipanaskra)
    skipun <- paste("sql++ <",tmpskipanaskra,">",tmpskra)
    system(skipun)
  }
  else {
    listi <- sort(listi)
    len <- floor(length(listi)/150) + 1
    ind <- as.numeric(cut(c(1:length(listi)), len))
    for(i in 1:len) {
      s <- listi[ind == i]
      s <- paste(s, collapse = ",")
      skipun1 <- paste(skipun,txt1,colname,"in (",s,")",txt2)
      write(skipun1,file=tmpskipanaskra)
      skipun1 <- paste("sql++  <",tmpskipanaskra,">>",tmpskra)
      system(skipun1)                  
    }
  }
  return(invisible())
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
