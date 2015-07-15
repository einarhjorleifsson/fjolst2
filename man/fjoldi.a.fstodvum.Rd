\name{fjoldi.a.fstodvum}
\alias{fjoldi.a.fstodvum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate number in each lengthgroup at some stations.   (out of date function) 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fjoldi.a.fstodvum(fstodvar, teg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fstodvar}{
%%     ~~Describe \code{fstodvar} here~~
}
  \item{teg}{
%%     ~~Describe \code{teg} here~~
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
function(fstodvar,teg) {
  
  lengdir <- lengdar.tafla(fstodvar$synis.id,teg)
  row.names(lengdir)<- c(1:nrow(lengdir))
  len <- join.data.frame(fstodvar,lengdir)
  return(len)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
