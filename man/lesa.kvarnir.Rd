\name{lesa.kvarnir}
\alias{lesa.kvarnir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read from the table fiskar.kvarnir or the dataframe all.kv.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.kvarnir(synis.id, teg, col.names = c("synis.id", "aldur", "lengd"), aldur = NULL, kyn = NULL, kynthroski = NULL, oslaegt = F, slaegt = F, lifur = F, kynfaeri = F, lengd = NULL, leidrett = F, oracle = Oracle)
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
  \item{aldur}{
%%     ~~Describe \code{aldur} here~~
}
  \item{kyn}{
%%     ~~Describe \code{kyn} here~~
}
  \item{kynthroski}{
%%     ~~Describe \code{kynthroski} here~~
}
  \item{oslaegt}{
%%     ~~Describe \code{oslaegt} here~~
}
  \item{slaegt}{
%%     ~~Describe \code{slaegt} here~~
}
  \item{lifur}{
%%     ~~Describe \code{lifur} here~~
}
  \item{kynfaeri}{
%%     ~~Describe \code{kynfaeri} here~~
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
function(synis.id,teg,col.names=c("synis.id","aldur","lengd"),aldur=NULL,kyn=NULL,kynthroski=NULL,oslaegt=F,slaegt=F,lifur=F,kynfaeri=F,lengd=NULL,leidrett=F,oracle=Oracle) {
  col.names=unique(c("synis.id","lengd","aldur",col.names))
  if(!oracle) {
#    if(!exists("all.kv"))
#      attach(paste(fjolstlib,"/ExtraStuff/all.kv.rda",sep=""))    
     if(is.data.frame(all.kv)){
      if(!is.null(synis.id)) 
        kv <- all.kv[all.kv$tegund \%in\% teg & all.kv$synis.id \%in\% synis.id ,col.names]
      else 
        kv <- all.kv[all.kv$tegund \%in\% teg,col.names]
    }
    else if(is.list(all.kv)) {
      if(!is.null(synis.id)) 
        kv <- all.kv[[as.character(teg)]][!is.na(match(all.kv[[as.character(teg)]]$synis.id,synis.id)),col.names]
      else 
        kv <- all.kv[[as.character(teg)]][,col.names]
    }
    return(kv)

  }             
   if(leidrett) table <- "fiskar.leidr_kvarnir"
  if(!leidrett) table <- "fiskar.kvarnir"
  col.names<-unique(c(c("synis.id","lengd","aldur"),col.names))
  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  if(!is.null(synis.id)) synis.id <- as.numeric(synis.id)
  
  if(!is.null(aldur)) aldur <- paste(aldur,collapse=",")
  if(!is.null(kynthroski)) {
    kynthroski <- paste(kynthroski,collapse=",")
  }
  if(!is.null(lengd)) lengd <- paste(lengd,collapse=",")
  if(oslaegt) 
    col.names <- unique(c(col.names,"oslaegt"))
  if(slaegt) 
    col.names <- unique(c(col.names,"slaegt"))
  if(lifur) 
    col.names <- unique(c(col.names,"lifur"))
  if(kynfaeri) 
    col.names <- unique(c(col.names,"kynfaeri"))
  ind <- match(col.names,kvarnir.col) 
  if(length(ind[is.na(ind)]) >0) { # dalkur ekki til
    ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
    print(paste(col.names[ind]," er ekki til"))
    return(invisible())
  }
  cn <- kvarnir.col.oracle[ind]
  cn <- paste(cn,collapse=",")
  teg <- paste("'",teg,"'",sep="")
  skipun <- paste("select",cn,"from",table,"where tegund =",teg)
  if(!is.null(aldur)) skipun <- paste(skipun,"and aldur is not NULL and aldur in (",aldur,")")
  if(!is.null(kynthroski)) skipun <- paste(skipun,"and kynthroski is not NULL and kynthroski in (",kynthroski,")")
  if(!is.null(lengd)) skipun <- paste(skipun,"and lengd is not NULL and lengd in (",lengd,")")
  if(!is.null(kyn)) skipun <- paste(skipun,"and kyn is not NULL and kyn =",kyn)
  if(oslaegt) skipun <- paste(skipun,"and oslaegt is not NULL")
  if(slaegt) skipun <- paste(skipun,"and slaegt is not NULL")
  if(lifur) skipun <- paste(skipun,"and lifur is not NULL")
  if(kynfaeri) skipun <- paste(skipun,"and kynfaeri is not NULL")

  run.sqlskipun(skipun,synis.id,"synis_id",tmpskipanaskra,tmpskra) 
    
  x <- read.sql(tmpskra,col.names)
  names(x) <- col.names
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
