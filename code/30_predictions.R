#description

#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load required packages
require(ggplot2)
require(stringr)
require(plyr)
require(dplyr)

#set dirs
rootdir<-find_root(
  criterion=has_file('longmarch.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load the models
setwd(filesdir)
load("ests.RData")

#load cow info
setwd(datadir)
load("cowcodes.RData")

# #load helper functions
setwd(codedir)
source('functions.R')
source('predict2.R')
setwd(homedir)

#set seed
set.seed(23)

#########################################################
#########################################################

#COUNTERFACTUALS

#i will generate predicted trajectories
#under a couple of pertinent cfactuals

#########################################################
#########################################################

#PERMUATIONGS

#these are cfactuals
tmpcfactuals<-c(
  "observed", #as observed
  "nvexist", #movement of some kind
  "nvlong", #90th percentile
  "nvmax", #maximally strong nv camp
  "nvabsent" #no nv camp
)

#check quantiles
tmpquantiles<-seq(
  from=0,to=1,by=0.05
)
tmp<-demdf$gwf_dem==1 & 
  demdf$gwf_st>1950 &
  (demdf$year-demdf$gwf_st)==1 &
  !is.na(demdf$gwf_dem) & !is.na(demdf$gwf_st) 
tmp<-tapply(
  demdf$nv_regime_dur[tmp],
  demdf$gwf_casename[tmp],
  unique
)
campsA<-tmp[!is.na(tmp)]
sort(campsA)
length(campsA)
quantile(campsA,tmpquantiles)

setwd(metadir); dir()
trandf<-read.csv(
  'gwf_tran.csv',
  stringsAsFactors=F
)
head(trandf)
tmp<-trandf$year>1950 
trandf<-trandf[tmp,]
campsB<-trandf$nv_tran_dur
sort(campsB)
length(campsB)
quantile(campsB,tmpquantiles)

nvexist.val<-1 #45th percentile; mvm't, no duration
nvabsent.val<-0 #least protest observed
nvlong.val<-4.0 #90th percentile
nvmax.val<-13 #most protest observed

##identify cases
#these are units for prediction
#later we will chuck those that
#aren't in sample, can't predict w/ them
tmp<-demdf$gwf_dem==1 & 
  demdf$gwf_st>1950 &
  (demdf$year-demdf$gwf_st)==1 &
  !is.na(demdf$gwf_dem) & !is.na(demdf$gwf_st) 
cases<-unique(demdf$gwf_casename[tmp])

#generate perms
cfdf<-expand.grid(
  case=cases,
  dv=unique(dvs),
  cfactual=tmpcfactuals,
  stringsAsFactors=F
)

#trim
tmp<-rep(T,nrow(cfdf))

keepers<-varsdf$maindv==1 & varsdf$maindem==1
maindvs<-varsdf$oldname[keepers]
tmp<-tmp & cfdf$dv%in%maindvs
#tmp<-tmp & cfdf$dv%in%maindvs[1:2]
tmp<-tmp & !cfdf$cfactual%in%c("nvexist","nvmax")
# tmp<-tmp & cfdf$case=="Dominican Rep 78-NA" 

cfdf<-cfdf[tmp,]
nrow(cfdf) * reps
#cfdf$cfactual %>% unique

#########################################################
#########################################################

#PREDICT!

#put one set of modeldraws, here
mydvs<-unique(cfdf$dv)
modeldraws<-lapply(mydvs,function(thisdv) {
  #modname<-prefmodname
  #get model
  thismodrow<-finaldf$dv==thisdv & 
    finaldf$pref
  thismodname<-unique(finaldf$mname[thismodrow])
  if(length(thismodname)>1)
    stop("Duplicate models.")
  thismod<-mods[[thismodname]]
  #get coefs and draw
  mu<-thismod$coefficients
  robvcov<-plm::vcovHC(
    thismod,
    type="HC0",
    cluster="group"
  )
  #robvcov<-thismod$vcov
  betas<-MASS::mvrnorm(
    n=reps,
    mu=mu,
    Sigma=robvcov
  )
  list(
    mu=mu,
    betas=betas
  )
})
names(modeldraws)<-mydvs

#define outofbounds values
obvals<-lapply(dvs,function(dv) {
  minval<-0
  maxval<-100
  returnlist<-list(min=minval,
                   max=maxval)
  return(returnlist)
})
names(obvals)<-dvs

#prep loop through
rows<-1:nrow(cfdf)
row.names(cfdf)<-rows

#looop through
tmpoutput<-lapply(rows,function(i) {
  
  #i<-35
  #i<-which(cfdf$case=="Pakistan 88-99")[1]
  
  #######################################
  #SETUP
  print("SETUP")
  
  #get params
  #for model selection
  thisdv<-cfdf$dv[i]
  thisdv.pretty<-getname(
    thisdv
  )
  
  #for prediction and df
  thiscf<-cfdf$cfactual[i]
  thiscase<-cfdf$case[i]
  
  #i will also return some info about missing obs
  misslist<-list()
  misslist$case<-thiscase
  
  #track progress
  print("#####")
  print(i)
  print(
    paste0(
      round(100*i/nrow(cfdf),2),
      "%"
    )
  )
  
  #get model
  thismodrow<-finaldf$dv==thisdv & 
    finaldf$pref
  thismodname<-unique(finaldf$mname[thismodrow])
  if(length(thismodname)>1)
    stop("Duplicate models.")
  thismod<-mods[[thismodname]]
  
  #get modeldraws
  thismod.draws<-modeldraws[[thisdv]]
  thisbeta<-thismod.draws$mu
  thesebetas<-thismod.draws$betas

  #get df/sample
  this.sampname<-finaldf$sampname[thismodrow] %>%
    unique
  #get rows
  this.samp<-samps[[this.sampname]]
  thiscase.rows<-demdf$gwf_casename==thiscase & 
    !is.na(demdf$gwf_casename)
  tmprows<-this.samp & thiscase.rows
  
  #get cols
  idvars<-c(
    "ccode",
    "gwf_casename",
    "year"
  )
  
  #include all vars in this model
  modvars<-all.vars(thismod$formula)
  tmpcols<-c(
    idvars,
    modvars,
    paste0(modvars,".IPOL")
  )
  tmpcols<-tmpcols[tmpcols%in%names(demdf)]
  
  # #include all model vars
  # #and their ipolated cousin
  # tmpcols<-c(
  #   idvars,
  #   allvars,
  #   paste0(allvars,".IPOL")
  # ) %>% unique
  # tmpcols<-tmpcols[tmpcols%in%names(demdf)]
  #subset
  tmpdf<-demdf[tmprows,tmpcols]
  
  #######################################
  
  #MISSING OBS
  if(nrow(tmpdf)>0) {
    ####################
    ####################
    #if there are gaps in any vars
    #call in the interpolated version
    tmp<-complete.cases(tmpdf)
    tmpdiffs<-diff(tmpdf$year[tmp])
    if(sum(tmpdiffs>1)>0) {
      tmp<-apply(tmpdf,2,function(x) sum(is.na(x)))
      tmp[tmp>0]
      stop("You're missing yrs")
    }
    # tmp<-sapply(allvars,function(v) {
    #   x<-tmpdf[[v]]
    #   sum(is.na(x))!=0
    # })
    # nacols<-tmpcols[tmp]
    # if(length(nacols)>0) {
    #   
    #   stop("Not implemented.")
    #   #replace missing vars w/ ipol cousins
    #   
    # } else {
    #   
    #   print("Nothing missing.")
    #   
    # }
    #tmpdf<-tmpdf[,c(idvars,allvars)]
    ####################
    ####################
  } else {
    #break this loop, and return NA
    returndf<-data.frame(
      cfrow=i,
      yhat=NA
    )
    return(returndf)
  }
  
  #######################################
  
  #CFACTUAL
  print("DEFINE CFACTUAL")
  
  if(thiscf=="observed") {
    
    #nothing doing
  } else if (thiscf=="nvmax") {
    
    tmpdf$nv_regime_dur<-
      nvmax.val
    
  } else if (thiscf=="nvlong") {
    
    tmpdf$nv_regime_dur<-
      nvlong.val 
  
  } else if (thiscf=="nvexist") {
    
    tmpdf$nv_regime_dur<-
      nvexist.val
    
  } else if (thiscf=="nvabsent") {
    
    tmpdf$nv_regime_dur<-
      nvabsent.val
    
  } 
  
  #######################################
  
  #PREDICITON
  
  print("PREDICTION") 
  
  #get betas
  if(reps==1) {
    betas<-matrix(
      thismod$coefficients,
      nrow=1
    )
    colnames(betas)<-names(thismod$coefficients)
    #need to add an intercept (i.e., the fixef)
    #if there isn't one (in the non-RE models)
    #this gives me the intercept for this sector
    tmp<-str_detect(colnames(betas),"Intercept")
    if(sum(tmp)==0) { #for FE model
      this.intercept<-plm::fixef(thismod)[thisind]
      oldnames<-colnames(betas)
      betas<-cbind(this.intercept,betas)
      colnames(betas)<-c("(Intercept)",oldnames)
      row.names(betas)<-NULL
    }
  } else if(reps>1) {
    mu<-thisbeta
    betas<-thesebetas
    tmp<-str_detect(colnames(betas),"Intercept")
    if(sum(tmp)==0) { #for FE model
      this.intercept<-plm::fixef(thismod)[thisind]
      oldnames<-colnames(betas)
      betas<-cbind(this.intercept,betas)
      colnames(betas)<-c("(Intercept)",oldnames)
      row.names(betas)<-NULL
    }
  } else {
    stop("not implemented")
  }
  
  #output prediction
  tmpseq.j<-1:reps
  returndf<-lapply(tmpseq.j,function(j) {
    #j<-10
    print(j)
    thisbeta<-betas[j,]
    ####
    yhat<-rep(NA,nrow(tmpdf)) 
    for(k in 1:nrow(tmpdf)) {
      #k<-1
      yhat[k]<-predict2(
        thismod,
        tmpdf[k,],
        mybeta=thisbeta
      )
      if(!is.na(yhat[k])) { 
        minval<-obvals[[thisdv]]$min
        maxval<-obvals[[thisdv]]$max
        if(!is.na(minval))
          if(yhat[k]<minval) 
            yhat[k]<-minval
        if(!is.na(maxval))
          if(yhat[k]>maxval) 
            yhat[k]<-maxval
      }
      limit<-length(yhat) 
      if(k+1<=length(yhat))
        tmpdf[[paste0("L.",thisdv)]][k+1]<-
        yhat[k]
    }
    data.frame(
      cfrow=i,
      rep=j,
      year=tmpdf$year,
      yhat=yhat,
      demyear=1:nrow(tmpdf),
      nv_regime_dur=tmpdf$nv_regime_dur,
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  
  #######################################
  #RETURN
 list(
   returndf=returndf
 )
  
})

#########################################################
#########################################################

#put the predictions together
rawpredictdf<-rbindlist(
  lapply(tmpoutput,function(x) x$returndf)
)

nadf<-rawpredictdf[is.na(yhat)]
if(nrow(nadf)>0)
  stop('inspect me')

#merge the scenario info
cfdf$cfrow<-1:nrow(cfdf)

intersect(
  names(cfdf),
  names(rawpredictdf)
)
rawpredictdf<-merge(rawpredictdf,cfdf)

#########################################################
#########################################################

#GET PLOTTING DFS

#make me datatable
rawpredictdf<-data.table(rawpredictdf)

#########################################################
#########################################################

#save out
setwd(filesdir)
save.image("cfactuals.RData")
setwd(homedir)

#########################################################
#########################################################