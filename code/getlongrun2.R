#HELPER FUNCTION

getlongrun2<-function(
  m,
  vcov,
  dv,
  iv,
  ivsd
) {
  ####
  # m<-m.tmp
  # vcov<-m.tmp$vcov
  # dv<-thisdv
  # iv<-x
  # ivsd<-x.sd
  ####
  #get the coefficients
  tmpclass<-class(m)[1]
  if(tmpclass=="plm") {
    coefs<-m$coefficients
  } else if(tmpclass=="lmerMod") {
    coefs<-coef(summary(m))[,'Estimate']
  } else {
    stop("Not written for this kind of mod.")
  }
  ###
  tmpregex<-paste0(dv,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  lagdv<-names(coefs)[tmprows]
  means<-c(
    coefs[lagdv],
    coefs[iv]
  )
  tmpterms<-names(means)
  rows<-row.names(vcov)%in%c(tmpterms)
  cols<-colnames(vcov)%in%c(tmpterms)
  vcov.useme<-vcov[rows,cols]
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  colnames(draws)<-names(means)
  #inspect draws
  #head(draws)
  #tmp<-draws[,1]>1
  #draws[tmp,1]
  #draws<-draws[!tmp,]
  ####
  num<-apply(as.matrix(draws[,iv]),1,sum) * ivsd
  den<- 1 - apply(as.matrix(draws[,lagdv]),1,sum)
  lrm.distribution<-num/den
  returnrow<-summarize.distribution2(lrm.distribution)
  return(returnrow)
}