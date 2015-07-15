\name{fjoldi.i.flokkum}
\alias{fjoldi.i.flokkum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate number of fish behind (not in) each food sample.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fjoldi.i.flokkum(flokkur, teg, option = 1, lengdir, numer, lenfl = old.stomach.breaks$breaks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flokkur}{
%%     ~~Describe \code{flokkur} here~~
}
  \item{teg}{
%%     ~~Describe \code{teg} here~~
}
  \item{option}{
%%     ~~Describe \code{option} here~~
}
  \item{lengdir}{
%%     ~~Describe \code{lengdir} here~~
}
  \item{numer}{
%%     ~~Describe \code{numer} here~~
}
  \item{lenfl}{
%%     ~~Describe \code{lenfl} here~~
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
function(flokkur,teg,option=1,lengdir,numer,lenfl=old.stomach.breaks$breaks) {
  synis.id <- unique(flokkur$synis.id)
  if(missing(lengdir)) lengdir <- lesa.lengdir(synis.id,teg)
  if(length(lengdir)==0) return(flokkur)
  if(missing(numer)) numer <- lesa.numer(synis.id,teg)
  numer$rat <- (numer$fj.maelt+numer$fj.talid)/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 0
  lengdir <- join(lengdir,numer,"synis.id")
  lengdir$fjoldi <- lengdir$fjoldi*lengdir$rat
  flokkur$fjoldi <- rep(-999999,nrow(flokkur)) # var na en vandræði 
  if(option==1) {
    lengdir$fjoldi[is.na(lengdir$fjoldi)] <- 0
    ind <- cut(lengdir$lengd,lenfl)
    lengdir <- lengdir[!is.na(ind),]
    lengdir <- capply(lengdir$fjoldi, lengdir$synis.id, cut(lengdir$lengd, lenfl), 1)
  }
    else  lengdir <- capply(lengdir$fjoldi, lengdir$synis.id, lengdir$lengd, 1)
  cn <- dimnames(lengdir)[[2]]
  rn<- dimnames(lengdir)[[1]]
  ind <- match(rn,synis.id)
  rn <- synis.id[ind]
  lr <- length(rn);lc <- length(cn)
  rn <- c(matrix(rn,lr,lc))
  cn <- c(t(matrix(cn,lc,lr)))
  rn <- paste(rn,cn,sep="")
  lengdir <- c(lengdir)
  if(option==1) 	l <- paste(flokkur$synis.id,flokkur$lenfl,sep="")
    else  l <- paste(flokkur$synis.id,flokkur$lengd,sep="")
  ind <- match(l,rn)
  ind1 <- c(1:length(ind));ind1 <- ind1[!is.na(ind)]
  ind <- ind[!is.na(ind)]
  flokkur$fjoldi[ind1] <- lengdir[ind]
  i <- flokkur$fjoldi==-999999;flokkur$fjoldi[i] <- NA
  return(flokkur)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
