lesa.f.lengdir <-
function(synis.id=NULL,ranfiskur,lenfl=NULL,faeduhopur,table="f_lengdir",haed=F,kyn=F,faerslunumer=T,oracle=fjolstOracle){
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
  x <- run.sqlskipun(skipun,synis.id,"synis_id")
  
  return(x)
  
}
