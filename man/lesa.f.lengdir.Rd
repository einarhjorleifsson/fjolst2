\name{lesa.f.lengdir}
\alias{lesa.f.lengdir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read all length measurement of selected prey in selected predator at
given stations.  
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.f.lengdir(synis.id = NULL, ranfiskur, lenfl = NULL, faeduhopur, table = "f_lengdir", haed = F, kyn = F, faerslunumer = T, oracle = Oracle)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{synis.id}{
%%     ~~Describe \code{synis.id} here~~
}
  \item{ranfiskur}{
%%     ~~Describe \code{ranfiskur} here~~
}
  \item{lenfl}{
%%     ~~Describe \code{lenfl} here~~
}
  \item{faeduhopur}{
%%     ~~Describe \code{faeduhopur} here~~
}
  \item{table}{
%%     ~~Describe \code{table} here~~
}
  \item{haed}{
%%     ~~Describe \code{haed} here~~
}
  \item{kyn}{
%%     ~~Describe \code{kyn} here~~
}
  \item{faerslunumer}{
%%     ~~Describe \code{faerslunumer} here~~
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
function(synis.id=NULL,ranfiskur,lenfl=NULL,faeduhopur,table="f_lengdir",haed=F,kyn=F,faerslunumer=T,oracle=Oracle){
  if(!oracle) {
    if(table=="f_lengdir")result <- flengdir
    if(table=="f_staerdir")result <- fstaerdir
    if(table=="f_kynthroski")result <- fkynthroski
    result <- result[!is.na(match(result$ranfiskur,ranfiskur)),]
    result <- result[!is.na(match(result$faeduhopur,faeduhopur)),]
    if(!is.null(synis.id))
      result <- result[!is.na(match(result$synis.id,synis.id)),]
    i <- match("brad.lengd",names(result)) 
    if(!is.na(i)) names(result)[i] <- "lengd"
    return(result)
  }




  if(!is.null(synis.id)) 
    synis.id <- sort(unique(synis.id))
  tmpskra <- tempfile("splus")
  tmpskipanaskra <- tempfile("splussql++skipun")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  on.exit(unlink(tmpskipanaskra),add=T);#on.error(unlink(tmpskipanaskra),add=T)
  

  if(table=="f_lengdir")dalkur<-"len_fl"
  else dalkur<-"lengd_rf"

  if(table=="f_lengdir") {
    nafn <- c("synis.id","ranfiskur","lenfl","lengd","fjoldi")
    nafn1 <- c("synis_id","ranfiskur","len_fl","lengd","fjoldi")
  }
  else if(!is.na(match(table,c("f_staerdir","f_staerdir_tmp")))) {
    nafn <- c("synis.id","ranfiskur","lengd.rf","lengd","fjoldi")
    nafn1 <- c("synis_id","ranfiskur","lengd_rf","lengd","fjoldi")
  }
  else if(!is.na(match(table,c("f_kynthroski","f_kynthroski_tmp")))) {
    nafn <- c("synis.id","ranfiskur","lengd.rf","brad.lengd","brad.thyngd","brad.kyn","brad.kynthroski")
    nafn1 <- c("synis_id","ranfiskur","lengd_rf","brad_lengd","brad_thyngd","brad_kyn","brad_kynth")
  }    

  if(haed) { nafn <- c(nafn,"haed");nafn1 <- c(nafn1,"haed") }
  if(kyn){
    if(table=="f_kynthroski"){ 
      nafn <- c(nafn,c("brad.kyn","brad.kynth"))
      nafn1 <- c(nafn1,c("brad_kyn","brad_kynth"))
    }
    else{
      nafn <- c(nafn,"brad.kyn")
      nafn1 <- c(nafn1,"brad_kyn")
    }
  }
  if(faerslunumer){ nafn<-c(nafn,"faerslunumer");nafn1 <- c(nafn1,"faerslunumer")}
  if(length(grep("faeda",table)) == 0)
     table <- paste("faeda",table,sep=".")
    
  txt1 <- paste(nafn1,collapse=",")
  rf <- paste(ranfiskur,collapse=",")
  lf <- paste(lenfl,collapse=",")
  x <- match(faeduhopur,allir.hopar$faeduhopur)
  ind <- c(1:length(x));ind<- ind[is.na(x)]
  if(length(ind) > 0) {
    txt <- paste("Villa i lesa.hopar. Hopar",faeduhopur[ind],"ekki til")
    print(txt)
    return(invisible())
  }
  fh <- paste(faeduhopur,collapse="','")
  fh <- paste("'",fh,"'",sep="")
  skipun <- paste("select",txt1,"from",table,"where ranfiskur in (",rf,") and faeduhopur in (",fh,")")
  if(!is.null(lenfl)) 
    skipun <- paste(skipun,"and",dalkur,"in (",paste(lenfl,collapse=","),")")
  run.sqlskipun(skipun,synis.id,"synis_id",tmpskipanaskra,tmpskra)
  x <- read.sql(tmpskra,nafn,F)
  
  return(x)
  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
