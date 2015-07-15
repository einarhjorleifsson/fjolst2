\name{Lesa.allir.hopar}
\alias{Lesa.allir.hopar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read all records from the table f_hopar corresponding to the selected
samples.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Lesa.allir.hopar(flokk.id, tmptable = F, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flokk.id}{
%%     ~~Describe \code{flokk.id} here~~
}
  \item{tmptable}{
%%     ~~Describe \code{tmptable} here~~
}
  \item{oracle}{
%%     ~~Describe \code{oracle} here~~
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
function(flokk.id,tmptable=F,oracle=Oracle) {
  result <- NULL
  if(!oracle) {
    if(!is.null(flokk.id)) result <- fhopar[!is.na(match(fhopar$flokk.id,flokk.id)),]
    result$fjoldi[is.na(result$fjoldi)] <- 0
    result$thyngd[is.na(result$thyngd)] <- 0
    x <- apply.shrink.dataframe(result,c("thyngd","fjoldi"),c("flokk.id","faeduhopur"),sum)
    names(x)[3:4] <- c("thyngd","fjoldi")
    x$faeduhopur <- as.character(x$faeduhopur)
    return(x)
  }  
  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  
  flokk.id <- sort(flokk.id)
  if(tmptable)
    skipun <-  "select flokk_id,sum(NVL(thyngd,0)),sum(NVL(fjoldi,0)),faeduhopur from faeda.f_hopar_tmp "
  else
    skipun <-  "select flokk_id,sum(NVL(thyngd,0)),sum(NVL(fjoldi,0)),faeduhopur from faeda.f_hopar "

  run.sqlskipun(skipun,flokk.id,"flokk_id",tmpskipanaskra,tmpskra,"where","group by faeduhopur,flokk_id")
  x <- read.sql(tmpskra,c("flokk.id","thyngd","fjoldi","faeduhopur"))
  x$faeduhopur <- as.character(x$faeduhopur)
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
