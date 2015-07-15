\name{na.mean}
\alias{na.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
na.mean(v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v1}{
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{
%%     ~~Describe \code{v2} here~~
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
function(v1, v2)
{
        ind <- c(1:length(v1))
        ind1 <- ind[is.na(v1) & !is.na(v2)]
        ind2 <- ind[is.na(v2) & !is.na(v1)]
        v <- (v1 + v2)/2
        if(length(ind1) > 0)
                v[ind1] <- v2[ind1]
        if(length(ind2) > 0)
                v[ind2] <- v1[ind2]
        return(v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
