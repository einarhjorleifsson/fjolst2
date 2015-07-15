\name{lesa.hopar}
\alias{lesa.hopar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read from the table faeda.f_hopar or the dataframe fhopar
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.hopar(faeduhopur, flokk.id = NULL, col.names = NULL, return.on.error = F, tmptable = F, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{faeduhopur}{
%%     ~~Describe \code{faeduhopur} here~~
}
  \item{flokk.id}{
%%     ~~Describe \code{flokk.id} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{return.on.error}{
%%     ~~Describe \code{return.on.error} here~~
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
function(faeduhopur,flokk.id=NULL,col.names=NULL,return.on.error=F,tmptable=F,oracle=Oracle) {
  if(!oracle) {
    if(is.null(col.names)) col.names <- c("flokk.id","faeduhopur","thyngd","fjoldi")
    fjth <- c("thyngd","fjoldi") 
    if(is.na(match("fjoldi",col.names))) fjth <- fjth[-2]
    if(is.na(match("thyngd",col.names))) fjth <- fjth[-1]
    
    result <- fhopar[!is.na(match(fhopar$faeduhopur,faeduhopur)),]
    print(nrow(result)) 
    print(fjth)
    if(!is.null(flokk.id)) result <- result[!is.na(match(result$flokk.id,flokk.id)),]
    result$fjoldi[is.na(result$fjoldi)] <- 0
    result$thyngd[is.na(result$thyngd)] <- 0
    print(nrow(result)) 

    if(!is.na(match("meltingarstig",col.names))) {
      x <- apply.shrink.dataframe(result,fjth,c("flokk.id","faeduhopur","meltingarstig"),sum)
      names(x)[4:(4+length(fjth)-1)] <- fjth
    }
    x <- apply.shrink.dataframe(result,fjth,c("flokk.id","faeduhopur"),sum)
    names(x)[3:(3+length(fjth)-1)] <- fjth 
    return(x)
  }  
  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  nafn <- c("flokk.id","faeduhopur","fjoldi","thyngd","meltingarstig")
  if(is.null(col.names) || !is.na(match("meltingarstig",col.names)))
    melt <- T
  else
    melt <- F
  nafn1 <- c("flokk_id","faeduhopur","sum(NVL(fjoldi,0))","sum(NVL(thyngd,0))","meltingarstig")
  if(!is.null(col.names)){
    x <- match(col.names,nafn)
    ind <- c(1:length(x));ind<- ind[is.na(x)]
    if(length(ind) > 0) {
      x <- col.names(x)
      txt <- paste("Villa í lesa.hopar. Dálkar",x,"ekki til")
      print(txt)
      return(invisible())
    }
    ind <- match(col.names,nafn)
    ind <- ind[!is.na(ind)]
    txt1 <- unique(c(c("flokk_id","faeduhopur"),nafn1[ind]))
    txt <- unique(c(c("flokk.id","faeduhopur"),nafn[ind]))
  }
  else {txt1 <- nafn1;txt	<- nafn}
  
  i <- match("thyngd",txt1) 
  if(!is.na(i)) txt1[i] <- "sum(NVL(thyngd,0))"
  i <- match("fjoldi",txt1) 
  if(!is.na(i)) txt1[i] <- "sum(NVL(fjoldi,0))"

  
  txt1 <- paste(txt1,collapse=",")
  x <- match(faeduhopur,allir.hopar$faeduhopur)
  ind <- c(1:length(x));ind<- ind[is.na(x)]
  if(length(ind) > 0) {
    txt <- paste("Villa i lesa.hopar. Hopar",faeduhopur[ind],"ekki til")
    print(txt)
    if(return.on.error) return(invisible())
  }
  fh <- paste(faeduhopur,collapse="','")
  fh <- paste("'",fh,"'",sep="")
  if(tmptable) 
    table <- "faeda.f_hopar_tmp"
  else
    table <- "faeda.f_hopar"
  skipun <- paste("select",txt1,"from",table,"where faeduhopur in (",fh,")")
  
  if(melt) txt2 <- "group by flokk_id,faeduhopur,meltingarstig"
  else txt2 <- "group by flokk_id,faeduhopur"
  run.sqlskipun(skipun,flokk.id,"flokk_id",tmpskipanaskra,tmpskra,"and",txt2)
  x <- read.sql(tmpskra,txt,stop.on.error=F)
  
  
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
