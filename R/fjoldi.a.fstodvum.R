fjoldi.a.fstodvum <-
function(fstodvar,teg) {
  
  lengdir <- lengdar.tafla(fstodvar$synis.id,teg)
  row.names(lengdir)<- c(1:nrow(lengdir))
  len <- fjolst:::join.data.frame(fstodvar,lengdir)
  return(len)
}
