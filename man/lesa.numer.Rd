\name{lesa.numer}
\alias{lesa.numer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read fro the table fiskar.numer or the dataframe all.nu.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.numer(synis.id, teg, vigt = F, col.names = NULL, leidrett = F, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synis.id}{
%%     ~~Describe \code{synis.id} here~~
}
  \item{teg}{
%%     ~~Describe \code{teg} here~~
}
  \item{vigt}{
%%     ~~Describe \code{vigt} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{leidrett}{
%%     ~~Describe \code{leidrett} here~~
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
function(synis.id,teg,vigt=F,col.names=NULL,leidrett=F,oracle=Oracle){
  if(!oracle) { 
    if(is.null(col.names)) colnames <- c("synis.id","fj.maelt","fj.talid")
    if(!is.null(synis.id)) 
      nu <- all.nu[!is.na(match(all.nu$synis.id,synis.id)) & all.nu$tegund==teg,]
    else 
      nu <- all.nu[all.nu$tegund==teg,]
    return(nu)
  }
  col.namess <- unique(c("synis.id","fj.maelt","fj.talid",col.names))
  if(leidrett) table <- "fiskar.leidr_numer"
  if(!leidrett) table <- "fiskar.numer"
  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  if(!is.null(synis.id)) synis.id <- as.numeric(synis.id)
  if(vigt){
    txt <- c("synis_id","fj_maelt","fj_talid",
	     "fj_vigtad","vigt_synis","afli")
    txt <- paste(txt,collapse=",")
    txt1 <- c("synis.id","fj.maelt","fj.talid",
	      "fj.vigtad","vigt.synis","afli")
  }
    else {	
      txt <- c("synis_id","fj_maelt","fj_talid")
      txt <- paste(txt,collapse=",")
      txt1 <- c("synis.id","fj.maelt","fj.talid")
    }
  if( ! is.null(col.names)) { # velja dalka
    ind <- match(col.names,numer.col) 
    if(length(ind[is.na(ind)]) >0) { # dalkur ekki til
      ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
      print(paste(col.names[ind]," er ekki til"))
      return(invisible())
    }
    col.names <- unique(c(c("synis.id","fj.talid","fj.maelt"),col.names))
    ind <- match(col.names,numer.col) 
    txt <- numer.col.oracle[ind]
    txt1 <- col.names
    txt <- paste(txt,collapse=",")
  }
  skipun <- paste("select",txt,"from",table ,"where tegund = ",teg)
  run.sqlskipun(skipun,synis.id,"synis_id",tmpskipanaskra,tmpskra) 
  x <- read.sql(tmpskra,txt1)
  x$fj.talid[is.na(x$fj.talid)] <- 0
  row.names(x) <- x$synis.id
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
