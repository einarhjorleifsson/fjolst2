\name{lesa.lengdir}
\alias{lesa.lengdir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read from the table fiskar.lengdir or the dataframe all.le
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.lengdir(synis.id, teg, col.names = c("synis.id", "lengd", "fjoldi"), kyn = NULL, kynthroski = NULL, lengd = NULL, leidrett = F, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synis.id}{
%%     ~~Describe \code{synis.id} here~~
}
  \item{teg}{
%%     ~~Describe \code{teg} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{kyn}{
%%     ~~Describe \code{kyn} here~~
}
  \item{kynthroski}{
%%     ~~Describe \code{kynthroski} here~~
}
  \item{lengd}{
%%     ~~Describe \code{lengd} here~~
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
function(synis.id,teg,col.names=c("synis.id","lengd","fjoldi"),kyn=NULL,kynthroski=NULL,lengd=NULL,leidrett=F,oracle=Oracle) {
  if(!oracle) {
    #if(!exists("all.le"))
    #  attach(paste(fjolstlib,"/ExtraStuff/all.le.rda",sep=""))    
    if(!is.null(kyn)) col.names <- c(col.names,"kyn")
    if(!is.null(kynthroski)) col.names=c(col.names,"kynthroski")
    col.names <- unique(c("synis.id","lengd","fjoldi",col.names))
    if(is.data.frame(all.le)){
      if(!is.null(synis.id)) 
        le <- all.le[all.le$tegund \%in\% teg & all.le$synis.id \%in\% synis.id ,col.names]
      else 
        le <- all.le[all.le$tegund \%in\% teg,col.names]
    }
    else if(is.list(all.le)) {
      if(!is.null(synis.id)) 
        le <- all.le[[as.character(teg)]][!is.na(match(all.le[[as.character(teg)]]$synis.id,synis.id)),col.names]
      else 
        le <- all.le[[as.character(teg)]][,col.names]
    }
    return(le)

  }
              
  if(leidrett) table <- "fiskar.leidr_lengdir"
  if(!leidrett) table <- "fiskar.lengdir"
  col.names <- unique(c("synis.id","lengd","fjoldi",col.names))
  tmpskipanaskra <- tempfile("splussql++skipun")
  tmpskra <- tempfile("splus")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskra))
  if(!is.null(kynthroski)) {
    kynthroski <- paste(kynthroski,collapse=",")
  }
    
  if(!is.null(lengd)) lengd <- paste(lengd,collapse=",")

  if(!is.null(synis.id)) synis.id <- sort(as.numeric(synis.id))
  
  ind <- match(col.names,lengdir.col) 
  if(length(ind[is.na(ind)])>0) {
    ind1 <-c(1:length(ind));ind1 <- ind1[is.na(ind)]
    print(paste("dálkur",col.names[ind],"er ekki til"))
    return(invisible())
  }
  cn <- lengdir.col.oracle[ind]
  cn <- paste(cn,collapse=",")
  skipun <- paste("select",cn,"from",table,"where tegund =",teg)  
# Sleppa þessu línum þær henta ekki.  
#  if(!is.null(kynthroski)) skipun <- paste(skipun,"and kynthroski is not NULL and kynthroski in (",kynthroski,")")
  if(!is.null(lengd)) skipun <- paste(skipun,"and lengd is not NULL and lengd in (",lengd,")")

#  if(!is.null(kyn)) skipun <- paste(skipun,"and kyn is not NULL and kyn =",kyn)
  
  run.sqlskipun(skipun,synis.id,"synis_id",tmpskipanaskra,tmpskra) 

  x <- read.sql(tmpskra,col.names) 
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
