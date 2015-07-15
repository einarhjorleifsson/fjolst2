\name{lesa.leidr.stodvar}
\alias{lesa.leidr.stodvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read from the table fiskar.leidr_stodvar or the dataframe leidr.stodvar
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lesa.leidr.stodvar(col.names = stodvar.std, ar = NULL, veidarfaeri = NULL, man = NULL, reitur = NULL, leidangur = NULL, synis.id = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{ar}{
%%     ~~Describe \code{ar} here~~
}
  \item{veidarfaeri}{
%%     ~~Describe \code{veidarfaeri} here~~
}
  \item{man}{
%%     ~~Describe \code{man} here~~
}
  \item{reitur}{
%%     ~~Describe \code{reitur} here~~
}
  \item{leidangur}{
%%     ~~Describe \code{leidangur} here~~
}
  \item{synis.id}{
%%     ~~Describe \code{synis.id} here~~
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
function(col.names=stodvar.std,ar=NULL,veidarfaeri=NULL,man=NULL,reitur=NULL,leidangur=NULL,synis.id=NULL) {
  if(!is.null(veidarfaeri)){
    veidarfaeri <- paste(veidarfaeri,collapse=",")
  }
  
  x <- match(col.names,stodvar.col)
  ind <- c(1:length(x));ind<- ind[is.na(x)]
  if(length(ind) > 0) {
    txt <- paste("Villa � lesa.stodvar. D�lkar",col.names[ind],"ekki til")
    print(txt)
    return(invisible())
  }
  if(!is.null(ar)){ 
    for(i in 1:length(ar)) if(ar[i] < 1900) ar[i] <- ar[i] + 1900
    ar <- paste(ar,collapse=",")
  }
  if(!is.null(man))
    man <- paste(man,collapse=",")
  if(!is.null(reitur)) 
    reitur <- paste(reitur,collapse=",")
  if(!is.null(leidangur)){
    leidangur <- paste(leidangur,collapse="','")
    leidangur <- paste("'",leidangur,"'",sep="")
  }
  nafn2a <- c("kastad_n_breidd","kastad_v_lengd","hift_n_breidd","hift_v_lengd")
  nafn2b <- c("dypi_kastad","dypi_hift")
  
  dyp <- lat.lon <- F
  
  ind <- match(col.names,stodvar.col.splus)
  dn <- stodvar.col.oracle[ind[!is.na(ind)]]
  ind <- match(col.names,c("lat","lon"));ind<-ind[!is.na(ind)]
  if(length(ind)>0){ dn <- c(dn,nafn2a);lat.lon <- T}
  
  ind <- match(col.names,c("dypi"));ind <- ind[!is.na(ind)]
  if(length(ind)>0){ dn <- c(dn,nafn2b);dyp <- T}
  
#  dn <- c("synis_id",dn)
  ind <- match(dn,c("lat","lon","dypi"))
  ind1 <- c(1:length(ind));ind1<- ind[!is.na(ind)]
  if(length(ind1)>0) dn <- dn[-ind1]
  dn1 <- unique(dn)
  ind <- match(dn1,c("ar","man","dags","kl.kastad","kl.hift"))
  ind1 <- c(1:length(ind));ind <- ind1[is.na(ind)]
  dn1 <- dn1[ind]
  dn2 <- dn1
  txt <- c("veidarfaeri","kastad_n_breidd","hift_n_breidd","kastad_v_lengd","hift_v_lengd","yfirbordshiti","skip","fiskar.stodvar.synis_id")
  txt1 <- c("veidarf","kastad_breidd","hift_breidd","kastad_lengd","hift_lengd","yfirb_hiti","skip_nr","synis_id")
  i <- match(txt,dn1) 
  i1 <- 1:length(i);i1 <- i1[!is.na(i)];i <- i[!is.na(i)]
  dn1[i] <- txt1[i1]
  i <- grep("net_nr",dn1)
  if(length(i) > 0) {
    dn1 <- dn1[-i]
    dn2 <- dn2[-i]
  }
  dn <- paste(dn1,collapse=",")
#synis_is > 0 dummy til a� geta b�tt vi�  
  skipun <- paste("select",dn,",to_char(dags,'YYYY'),to_char(dags,'MM'),to_char(dags,'DD'),to_char(togbyrjun,'HH24'),to_char(togendir,'HH24'),to_char(togbyrjun,'mi'),to_char(togendir,'mi'),hnattstada from fiskar.leidr_stodvar where synis_id > 0 ")
  if(!is.null(ar)) 
    skipun <- paste(skipun,"and to_char(dags,'YYYY') in (",ar,")")
  if(!is.null(man)) 
    skipun <- paste(skipun,"and to_char(dags,'MM') in (",man,")")
  if(!is.null(reitur)) 
    skipun <- paste(skipun,"and reitur in (",reitur,")")
  if(!is.null(veidarfaeri)) 
    skipun <- paste(skipun,"and veidarf in (",veidarfaeri,")")
  if(!is.null(leidangur)){
    skipun <- paste(skipun,"and leidangur in (",leidangur,")")
  }
  tmpskra <- tempfile("splus")
  on.exit(unlink(tmpskra));#on.error(unlink(tmpskra))
  skipun <- paste("echo \"",skipun,"\" |sql++>",tmpskra)
  system(skipun)
  ind <- match(dn2,stodvar.col.oracle)
  n <-  length(count.fields(tmpskra,"|"))
  if(n==0){
    print("Stodvar Nothing selected")
    return(invisible())
  }
  ttt <- c(stodvar.col.splus[ind],c("ar","man","dags","kl.kastad","kl.hift"))
  x <- read.sql(tmpskra,col.names=c(stodvar.col.splus[ind],c("ar","man","dags","kl.kastad","kl.hift","min.kastad","min.hift","hnattstada")))
  i <- is.na(x$hnattstada)
  if(any(i)) x$hnattstada[i] <- -1
  x$kl.kastad <- x$kl.kastad+x$min.kastad/60
  x$kl.hift <- x$kl.hift+x$min.hift/60
  i <- match(c("min.kastad","min.hift"),names(x))
  x <- x[,-i]
					#	Breyta i desimal tolur
  
  ind <- match(c("kastad.n.breidd","hift.n.breidd"),names(x))
  ind <- ind[!is.na(ind)]
  if(length(ind)>0) {
    x$kastad.n.breidd <- geoconvert(x$kastad.n.breidd)
    x$hift.n.breidd <- geoconvert(x$hift.n.breidd)
  }
  
  ind <- match(c("kastad.v.lengd","hift.v.lengd"),names(x))
  ind <- ind[!is.na(ind)]
  if(length(ind)>0) {
    x$kastad.v.lengd <- x$hnattstada*geoconvert(x$kastad.v.lengd)
    x$hift.v.lengd <- x$hnattstada*geoconvert(x$hift.v.lengd)
  }
  
  if(lat.lon || is.null(col.names)) {
    x$lon <- na.mean(x$hift.v.lengd,x$kastad.v.lengd)
    x$lat <- na.mean(x$hift.n.breidd,x$kastad.n.breidd)
  }		
  if(dyp || is.null(col.names))
    x$dypi <- na.mean(x$dypi.kastad,x$dypi.hift)
  
  
					#	Velja dalka endanlega  
  
  
  cl <- unique(c("synis.id",col.names))
  ind <- match(cl,names(x))
  ind <- ind[!is.na(ind)]
  x <- x[,ind]		
  return(x)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line