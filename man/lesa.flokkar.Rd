\name{lesa.flokkar}
\alias{lesa.flokkar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read from the table faeda.f_flokkar or the dataframe fflokkar.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.flokkar(ranfiskur, lenfl = NULL, synis.id = NULL, fjoldi = T, total = T, flokk.id = NULL, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ranfiskur}{
%%     ~~Describe \code{ranfiskur} here~~
}
  \item{lenfl}{
%%     ~~Describe \code{lenfl} here~~
}
  \item{synis.id}{
%%     ~~Describe \code{synis.id} here~~
}
  \item{fjoldi}{
%%     ~~Describe \code{fjoldi} here~~
}
  \item{total}{
%%     ~~Describe \code{total} here~~
}
  \item{flokk.id}{
%%     ~~Describe \code{flokk.id} here~~
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
function(ranfiskur,lenfl=NULL,synis.id=NULL,fjoldi=T,total=T
			 ,flokk.id=NULL,oracle=Oracle){

  if(!oracle) {
    result <- fflokkar[,c("synis.id","flokk.id","ranfiskur","lenfl","fj.fmaga","fj.omelt","fj.tomra","fj.aelt","faerslunumer")]
    result <- result[result$ranfiskur==ranfiskur,]
    if(!is.null(synis.id)) result <- result[!is.na(match(result$synis.id,synis.id)),]
    if(!is.null(lenfl)) result <- result[!is.na(match(result$lenfl,lenfl)),]
    i <- is.na(result$fj.aelt);if(any(i)) result$fj.aelt[i] <- 0
    i <- is.na(result$fj.omelt);if(any(i)) result$fj.omelt[i] <- 0
    print(nrow(result))
    cat("start total")
    if(total) {
       if(!is.null(synis.id))
         tmp <- fhopar[!is.na(match(fhopar$flokk.id,result$flokk.id)),]
       else
         tmp <- fhopar
       x <- apply.shrink(tmp$thyngd,tmp$flokk.id,sum,names=c("flokk.id","total") )
       result <- join(result,x,"flokk.id",set=0)
    }
    cat("start fjoldi")
    if(fjoldi) {
      s.id=unique(result$synis.id)
      tmp <- lesa.lengdir(s.id,ranfiskur) 
      tmp1 <- lesa.numer(s.id,ranfiskur) 
      tmp <- Skala.med.toldum(tmp,tmp1) 
      tmp <- tmp[,c("synis.id","lengd","fj.alls")]
      tmp$lenfl <- as.numeric(cut(tmp$lengd,old.stomach.breaks$breaks))
      tmp <- apply.shrink(tmp$fj.alls,list(tmp$synis.id,tmp$lenfl),sum,names=c("synis.id","lenfl","fjoldi"))
      tmp$index <- paste(tmp$synis.id,tmp$lenfl)
      result$index <- paste(result$synis.id,result$lenfl) 
      tmp <- tmp[!is.na(match(tmp$index,result$index)),]
      result <- join(result,tmp[,c("index","fjoldi")],"index",set=0)
      j <- match("index",names(result));result <- result[,-j]
      result$fjoldi[is.na(result$fjoldi)] <- 0
    }
    i <- is.na(result$fj.aelt); if(any(i)) result$fj.aelt[i] <- 0

    return(result) 
  }

  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  ranf <- paste(ranfiskur,collapse=",")
  
					# 	Ekki stakir.  
  
  lfl <- c(5, 8, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80,90, 100, 120, 200);x <- 0
  nafn <- c("synis.id","flokk.id","ranfiskur","lenfl","fj.fmaga","fj.omelt","fj.tomra",
	    "fj.aelt","faerslunumer")
  nafn1 <- c("synis_id","flokk_id","ranfiskur","len_fl","fj_fmaga","fj_omelt","fj_tomra",
	     "fj_aelt","faerslunumer")
  dn <- nafn1;dn2 <- nafn
  dn1 <- paste(dn,collapse=",")
  skipun <- paste("select",dn1,"from faeda.f_flokkar where ranfiskur in (",ranf,")")
  if(!is.null(synis.id)) 
    skipun <- paste(skipun,"and synis_id in (",paste(synis.id,collapse=","),")")
  if(!is.null(flokk.id))
    skipun <- paste(skipun,"and flokk_id > ",flokk.id)
  if(!is.null(lenfl)) 
    skipun <- paste(skipun,"and lenfl in (",paste(lenfl,collapse=","),")")
  write(skipun,file=tmpskipanaskra)
  skipun <- paste("sql++  <",tmpskipanaskra,">",tmpskra)
  unix(skipun)
  ind <- match(dn,nafn1)
  nafn <- nafn[ind]
  x <- read.sql(tmpskra,nafn)

  
  #	Finna heildar magainnihald a syni
  if(total) {
    print("total")
    tmpskra1 <- tempfile("splus")
    on.exit(unlink(tmpskra1),add=T)
    #on.error(unlink(tmpskra1),add=T)
    table <- "faeda.f_hopar"
    skipun <- paste("select sum(NVL(thyngd,0)),flokk_id from",table)
    run.sqlskipun(skipun,x$flokk.id,"flokk_id",tmpskipanaskra,tmpskra1,"where","group by flokk_id") 
    y <- read.sql(tmpskra1,c("total","flokk.id"))
    x <- join(x,y,"flokk.id",set=0);
  }
  row.names(x) <- x$flokk.id
  print("fjoldi")
  if(fjoldi) x <- fjoldi.i.flokkum(x,ranfiskur)
  i <- is.na(x$fj.aelt);if(any(i)) x$fj.aelt[i] <- 0
  i <- is.na(x$fj.omelt);if(any(i)) x$fj.omelt[i] <- 0
  
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
