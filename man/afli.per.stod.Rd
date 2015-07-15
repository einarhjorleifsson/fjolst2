\name{afli.per.stod}
\alias{afli.per.stod}
catch per station 
\title{
catch per station 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
afli.per.stod(sfile, lwcoeff = c(0.01, 3), teg = 1, lfile, minl, maxl, oracle = Oracle, name = "afli")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sfile}{
%%     ~~Describe \code{sfile} here~~
}
  \item{lwcoeff}{
%%     ~~Describe \code{lwcoeff} here~~
}
  \item{teg}{
%%     ~~Describe \code{teg} here~~
}
  \item{lfile}{
%%     ~~Describe \code{lfile} here~~
}
  \item{minl}{
%%     ~~Describe \code{minl} here~~
}
  \item{maxl}{
%%     ~~Describe \code{maxl} here~~
}
  \item{oracle}{
%%     ~~Describe \code{oracle} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
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
function(sfile, lwcoeff = c(0.01, 3.), teg = 1., lfile,minl,maxl,oracle=Oracle,name="afli")
{
        if(missing(lfile))
                lfile <- lesa.lengdir(sfile$synis.id, teg,oracle=oracle)
        print("read nfile") 
        nfile <- lesa.numer(sfile$synis.id, teg,oracle=oracle)
        nfile$rat <- 1. + nfile$fj.talid/nfile$fj.maelt
        nfile$rat[is.na(nfile$rat)] <- 1.
        print("join") 
        lfile <- join(lfile, nfile[, c("synis.id", "rat")], "synis.id")
        lfile$wt <- (lfile$fjoldi * lfile$rat * lwcoeff[1.] * lfile$lengd^
                lwcoeff[2.])/1000.
        print("appl.shrink")
        if(!missing(minl)) lfile <- lfile[lfile$lengd >= minl,]
        if(!missing(maxl)) lfile <- lfile[lfile$lengd <= maxl,]

        tmp <- apply.shrink(lfile$wt, lfile$synis.id, sum)
        print("appl.shrink.buid")
        names(tmp) <- c("synis.id", name)
        sfile <- join(sfile, tmp, "synis.id", set = 0.)
        return(sfile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
