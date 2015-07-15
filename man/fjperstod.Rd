\name{fjperstod}
\alias{fjperstod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the number of a species at the list of stations.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fjperstod(data, tegund, name = "fjoldi", oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{tegund}{
%%     ~~Describe \code{tegund} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
function(data, tegund, name = "fjoldi",oracle=Oracle)
{
        tmp <- lesa.numer(data$synis.id, tegund,oracle=oracle)
        i <- is.na(tmp$fj.talid)
        if(any(i))
                tmp$fj.talid[i] <- 0
        i <- is.na(tmp$fj.maelt)
        if(any(i))
                tmp$fj.maelt[i] <- 0
        tmp$fjoldi <- tmp$fj.maelt + tmp$fj.talid
        x <- apply.shrink(tmp$fjoldi, tmp$synis.id, sum, names = c("synis.id",
                name))
        data <- join(data, x, "synis.id", set = 0)
        return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
