\name{Skala.med.toldum}
\alias{Skala.med.toldum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale length measurements by number counted.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Skala.med.toldum(lengdir, numer, tegund)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lengdir}{
%%     ~~Describe \code{lengdir} here~~
}
  \item{numer}{
%%     ~~Describe \code{numer} here~~
}
  \item{tegund}{
%%     ~~Describe \code{tegund} here~~
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
function(lengdir,numer,tegund) {
  if(missing(numer))
    numer <- lesa.numer(synis.id=unique(lengdir$synis.id),tegund=tegund)
  numer$rat <- 1 + numer$fj.talid/numer$fj.maelt
  numer$rat[is.na(numer$rat)] <- 1
  l1 <- join(lengdir,numer[,c("synis.id","rat")],"synis.id")
  i <- is.na(l1$rat)
  if(any(i)) l1$rat[i] <- 1
  lengdir$fj.alls <- l1$fjoldi*l1$rat
  return(lengdir)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
