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
require(plm)
require(lme4)
#require(clusterSEs)

#set dirs
rootdir<-find_root(
  criterion=has_file('longmarch.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load the dataset
setwd(datadir)
demdf<-read.csv(
  "demdf.csv",
  stringsAsFactors=F
)

#load cow info
setwd(datadir)
load("cowcodes.RData")

# #load helper functions
setwd(codedir)
source('functions.R')
setwd(homedir)

#set seed
set.seed(23)
reps<-10000

#########################################################
#########################################################

#RUN ALI PRELIM MODELS
#SETUP

#dvs defined in varsdf
dvs<-varsdf$oldname[varsdf$type=="dv"]

#drop the extra dvs
tmp<-dvs%in%c(
  'ss_spend',
  'eduhealth_spend',
  'welfins_spend',
  'progtaxes'
)
dvs<-dvs[!tmp]

ivs<-c(
  "nv_regime_dur",
  "v_regime_dur"
)

controls<-c(
  ### continuous
  #modernization
  "L.gdpcap_log",
  "L.growthrate",
  "L.bigcity",
  "L.edyrs",
  
  #redistributivist
  "L.gini_generous",
  
  #class-based
  "L.landlords",
  "L.highcapratio", 
  
  #exogenous
  "L.logoilpc",
  "L.logpop",
  "L.trade_gdp",
  "L.exclpop",
  
  #### exogenous, constants
  "ethfrac",
  "colduration",
  
  ### pretransition
  "precamp_cs",
  "durcamp_cl",
  
  #### dummies
  "prior_mil",
  "prior_personal",
  "prior_party",
  #"post_independence", #why?
  
  ##### regime history
  "past_dem_age"
  ###########

)

#make sure no typos/all prez
checkvars<-c(dvs,ivs,controls)
tmp<-!checkvars%in%names(demdf)
if(sum(tmp)>0) {
  print(checkvars[tmp])
  stop("Typo alert.")
}

#########################################################
#########################################################

#FORMULAS

modsdf<-expand.grid(
  dv=dvs,
  lagdv=c("lagdv","nolagdv"),
  iv.lag=c(0),
  control.lag=c(1),
  controls=c("controls","bivariate"),
  sample=c("demsonly","allobs"),
  effects=c("fe","re","regFE","re_regFE","re_lme4"),
  robust=c("clrobSE","normalSE"),
  stringsAsFactors=F
)
modsdf$mname<-apply(modsdf,1,paste0,collapse=".") %>%
  str_replace_all("\\s+","")

#trimmer
tmp<-rep(T,nrow(modsdf))

#this is mandatory trim, MLE and robustSE doesn't work
tmp<-tmp & 
  !(modsdf$robust=="clrobSE" & modsdf$effects=="re_lme4")

#these are trims for main mods
#other permutations are run in robmods
#tmp<-tmp & modsdf$dv%in%c("v2x_polyarchy","v2x_libdem")

#perms
tmp<-tmp & modsdf$iv.lag==0
tmp<-tmp & modsdf$control.lag==0 
tmp<-tmp & modsdf$controls=="controls"
tmp<-tmp & modsdf$lagdv=="lagdv"
tmp<-tmp & modsdf$robust=="clrobSE"
#estimate RE and demsonly
tmp<-tmp & modsdf$sample=="demsonly"
#tmp<-tmp & modsdf$effects%in%c("fe","re","re_regFE")
tmp<-tmp & modsdf$effects=="re_regFE"
modsdf<-modsdf[tmp,]

#check
if(sum(tmp)>0)
  stop('needs to be rewritten')

this.sequence<-1:nrow(modsdf)
forms<-lapply(this.sequence,function(i) {
  #i<-1
  
  #get params
  thisrow<-modsdf[i,]
  thisdv<-thisrow$dv
  
  ####FORMULA
  ##LHS
  lhs<-thisdv
  ##RHS
  #year fe
  thisfe.yr<-"factor(year)"
  #region fe
  #otherwise, effects used later$e
  thisfe.reg<-ifelse(
    thisrow$effects%in%c("re_regFE","regFE"),
    "factor(region_name)",
    " "
  )
  #colonizer fe
  thisfe.col<-"factor(colruler)"
  #lagdv
  thislagdv<-ifelse(
    thisrow$lagdv=="lagdv",
    paste0("L.",thisdv),
    " "
  )
  #iv
  ivlag<-thisrow$iv.lag
  thisiv<-sapply(ivs,function(iv) {
    addlags.term(iv,ivlag) 
  }) %>% paste(collapse=" + ")
  #regional avg of dv
  regavg<-paste0("L.",thisdv,"_regavg")
  #controls
  control.lag<-thisrow$control.lag
  if(thisrow$controls=="controls") {
    thiscontrol<-addlags.term(controls,control.lag)
  } else {
    thiscontrol<-""
  }
  
  #TOGETHER
  rhs<-paste(
    thisfe.yr,
    thisfe.reg,
    thisfe.col,
    thislagdv,
    thisiv,
    thiscontrol,
    regavg,
    sep=" + "
  )
  #get rid of extra + signs
  rhs<-str_replace_all(rhs,"\\+\\s+\\+","+ ") %>%
    str_replace("\\s+\\+\\s+$","") %>%
    str_replace("^\\s+\\+\\s+","")
  
  #RETURN
  #put the formula togeehter
  thisform<-paste(
    lhs,"~",rhs
  ) %>% as.formula
  return(thisform)
  
})

names(forms)<-modsdf$mname

#any checking?
# tmpform<-forms$v2x_polyarchy.lagdv.1.1.controls.demsonly.re
# tmpform
# m.tmp<-plm(
#   data=demdf[demdf$gwf_dem==1 & demdf$gwf_st>1950,],
#   formula=tmpform,
#   model="random",
#   index=c("ccode","year")
# )
# summary(m.tmp)
# m.tmp$model %>% nrow
# m.tmp$model$`factor(ccode)` %>% unique

#########################################################
#########################################################

#SAMPLE
#define by dv,demsonly/not

sampsdf<-expand.grid(
  dv=unique(modsdf$dv),
  sample=unique(modsdf$sample),
  stringsAsFactors=F
)
sampsdf$sampname<-apply(
  sampsdf,
  1,paste0,collapse="."
)

tmp.seq<-1:nrow(sampsdf)
samps<-lapply(tmp.seq,function(i) {
  #i<-1
  thisdv<-sampsdf$dv[i]
  thissample<-sampsdf$sample[i]
  #get the sample
  #all vars for this dv have to be present
  thisdv.forms<-forms[modsdf$dv==thisdv]
  thisdv.vars<-lapply(thisdv.forms,all.vars) %>%
    unlist %>%
    unique
  restrict1<-complete.cases(demdf[,thisdv.vars])
  #and, further restriction
  if(thissample=="demsonly") {
    restrict2<-demdf$gwf_st>1950 & demdf$gwf_dem==1
  } else {
    restrict2<-demdf$gwf_st>1950
  }
  restrict1 & restrict2
})
names(samps)<-sampsdf$sampname

sampinfodf<-lapply(samps,function(s) {
  tmpdf<-demdf[s,]
  data.frame(
    N=nrow(tmpdf),
    N.cows=length(unique(tmpdf$ccode)),
    range=paste0(
      min(tmpdf$year),"-",max(tmpdf$year)
    )
  )
}) %>% rbind.fill

#sampsdf
sampsdf<-cbind(
  sampsdf,
  sampinfodf
)
sampsdf

#########################################################
#########################################################

#ESTIMATION

this.sequence<-seq_along(forms)
mods<-lapply(this.sequence,function(i) {
  #i<-1
  #progress
  print(
    paste(
      "Estimating model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  thisform<-forms[[i]]
  
  thisdv<-modsdf$dv[i]
  thissample<-modsdf$sample[i]
  tmprow<-sampsdf$dv==thisdv & 
    sampsdf$sample==thissample
  this.samp<-samps[[sampsdf$sampname[tmprow]]]
  
  #also restrict cols only to vars,
  #else plm() returns output
  indexvars<-c("ccode","year")
  modvars<-all.vars(thisform)
  allvars<-c(indexvars,modvars) %>% 
    unique
  #this is the estimating df
  tmpdf<-demdf[this.samp,allvars]
  
  #if re/fe, then get plm arg
  #if re_lme4, then est via that
  #and add RE to the argument
  #if PLM
  thiseffect<-modsdf$effects[i]
  if(thiseffect%in%c("fe","re","re_regFE","regFE")) {
    #get fe/re spec?
    if(thiseffect=="fe") {
      thismodel.arg<-"within"
    } else if (thiseffect=="regFE") {
      thismodel.arg<-"pooling"
    } else if (thiseffect%in%c("re","re_regFE")) {
      thismodel.arg<-"random"
    } else {
      stop("Not implemented.")
    }
    #estimate
    m.tmp<-plm(
      data=tmpdf,
      formula=thisform,
      index=indexvars,
      model=thismodel.arg
    )
  } else if (thiseffect=="re_lme4") {
    #estimate via relme4
    newform<-deparse(thisform) %>%
      paste0(collapse="") %>% 
      str_replace(
        "factor\\(year\\)",
        "factor(year) + (1 | ccode)"
      ) %>%
      as.formula
    m.tmp<-lmer(
      data=tmpdf,
      formula=newform
    )
  }
  return(m.tmp)
})
names(mods)<-names(forms)

#########################################################
#########################################################

#SDS/RANGE

#multiply each variable by 20th:80th percentile range
#unless it is dummy, in which case leave as is

vars<-c(
  ivs,
  controls,
  paste0("L.",dvs,"_regavg")
)

#output sd and range in polyarchy sample
thisdf<-demdf[samps$v2x_polyarchy.demsonly,]
#which are just equal to 1 if dummy
sdsdf<-lapply(vars,function(thisv) {
  #thisv<-vars[3]
  print(thisv)
  isdummy<-getdummy(thisv)
  avg<-mean(thisdf[[thisv]],na.rm=T)
  if(isdummy) {
    sd<-rng<-1
  } else {
    sd<-sd(thisdf[[thisv]],na.rm=T)
    rng<-quantile(
      thisdf[[thisv]],
      c(0.2,0.8),
      na.rm=T
    ) %>% 
      diff %>% 
      unname
  }
  data.frame(
    var=thisv,
    avg=avg,
    sd=sd,
    range=rng,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#add all dvs, too
exsdsdf<-lapply(dvs,function(dv) {
  thisdf<-demdf[samps[[sampsdf$sampname[sampsdf$dv==dv]]],]
  avg<-mean(thisdf[[dv]],na.rm=T)
  sd<-sd(thisdf[[dv]],na.rm=T)
  rng<-quantile(
    thisdf[[dv]],
    c(0.2,0.8),
    na.rm=T
  ) %>% 
    diff %>% 
    unname
  data.frame(
    var=dv,
    avg=avg,
    sd=sd,
    range=rng,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#add to sdsdf
sdsdf<-rbind.fill(
  sdsdf,
  exsdsdf
)


#########################################################
#########################################################

#GET RESULTS

#load lmtest
require(lmtest)

this.sequence<-seq_along(mods)
tmpoutput<-lapply(this.sequence,function(i) {
  #i<-1
  #progress
  print(
    paste(
      "Getting results from model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  m<-mods[[i]]
  mname<-modsdf$mname[i]
  thisdv<-modsdf$dv[i]
  this.samp<-modsdf$sample[i]
  this.sampname<-paste0(
    thisdv,".",this.samp
  )
  
  #what kind
  isplm<-ifelse(
    class(m)[1]=="plm",
    T,F
  )
  
  #get coefs
  if(isplm) {
    
    coefs<-m$coefficients
    #get vcov
    if(modsdf$robust[i]=="normalSE") {
      thisvcov<-m$vcov
    } else if(modsdf$robust[i]=="clrobSE") {
      thisvcov<-vcovHC(
        m,
        type="HC1",
        cluster="group"
      )
    }
    coefs.tested<-coeftest(m,thisvcov)
    
  } else if(!isplm) {
    
    thisvcov<-vcov(m)
    coefs.tested<-coef(summary(m))
    #set up a coefs vector, like plm
    coefs<-coefs.tested[,'Estimate']
    
  }
  
  #SHORT-RUN
  #all ivs, and lag dvs
  shortrunvars<-c(
    thisdv,
    paste0("L.",thisdv,"_regavg"),
    ivs,
    controls
  )
  sr.sequence<-seq_along(shortrunvars)
  print("SR")
  shortrundf<-lapply(sr.sequence,function(j) {
    #j<-2
    #print(j)
    #get params
    thisiv<-shortrunvars[j]
    # #if this is the dv, no multiply
    if(thisiv==thisdv) {
      thisiv.sd<-1
    } else {
      thisiv.sd<-sdsdf$sd[sdsdf$var==thisiv]
    }
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #get the var(s)
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    thisrow<-str_detect(row.names(coefs.tested),thevar.regex)
    notvar.regex<-paste0("X",thisiv)
    thisrow<-thisrow & !str_detect(row.names(coefs.tested),notvar.regex)
    #if(sum(thisrow)>1)
    #stop(paste(thisiv,"is matching >1 terms"))
    #but don't match square terms
    #thisrow<-thisrow & !str_detect(row.names(coefs.tested),paste0(thisiv,"2"))
    #this gives term
    term<-row.names(coefs.tested)[thisrow]
    est<-coefs.tested[thisrow,"Estimate"]
    se<-coefs.tested[thisrow,"Std. Error"]
    tval.col<-str_detect(colnames(coefs.tested),"t.value")
    t<-coefs.tested[thisrow,tval.col]
    if(isplm) {
      pval<-coefs.tested[thisrow,"Pr(>|t|)"]
      #multiply by iv.sds
      est<-est*thisiv.sd
      se<-se*thisiv.sd
      #now compute
      est.min<-est-1.96*se
      est.max<-est+1.96*se
    } else {
      pval<-NA
      tmp<-confint(m,row.names(coefs.tested)[thisrow]) * 
        thisiv.sd
      est.min<-tmp[1]; est.max<-tmp[2]
    }
    #if it wasn't in this model.. 
    if(sum(thisrow)>0) {
      returnrow<-data.frame(
        iv=thisiv,
        term,
        mu=est,
        mu.min=est.min,
        mu.max=est.max,
        se,
        pval,
        t,
        stringsAsFactors=F
      )
    } else {
      returnrow<-data.frame(
        iv=thisiv,
        mu=NA
      )
    }
    #return
    return(returnrow)
  }) %>% rbind.fill
  #identify
  shortrundf$type<-"shortrun"
  #chuck all nas
  shortrundf<-shortrundf[!is.na(shortrundf$mu),]
  
  ##########################################
  
  #LONG-RUN
  print("LR")
  longrunvars<-c(
    ivs,
    paste0("L.",thisdv,"_regavg"),
    controls
  )
  lroutput<-lapply(seq_along(longrunvars),function(j) {
    #j<-3
    #print(j)
    #get params
    thisiv<-longrunvars[j]
    # #get sd
    thisiv.sd<-sdsdf$sd[sdsdf$var==thisiv]
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #did we detect var?
    tmprows<-names(coefs)==thisiv
    if(sum(tmprows)>1)
      stop("Problem.")
    #is there a lag of the dv
    islag<-modsdf$lagdv[i]=="lagdv"
    if(!islag | sum(tmprows)==0) {
      returnrow<-data.frame(mu=NA)
    } else {
      #all ests involving this var
      tmprows<-names(coefs)==thisiv
      if(sum(tmprows)>1)
        stop("Not written for multiple terms")
      iv.terms<-names(coefs)[tmprows]
      tmpregex<-paste0(thisdv,"$")
      tmprows<-str_detect(names(coefs),tmpregex)
      lagdv.terms<-names(coefs)[tmprows]
      #get the longrun estimate
      means<-c(
        coefs[lagdv.terms],
        coefs[iv.terms]
      )
      # if(isplm) {
      #   means<-c(
      #     m$coefficients[lagdv.terms],
      #     m$coefficients[iv.terms]
      #   )
      # } else {
      #   coefs
      # }
      #get the vcov matrix
      new.vcov<-thisvcov #from above
      rows<-row.names(new.vcov)%in%c(lagdv.terms,iv.terms)
      cols<-colnames(new.vcov)%in%c(lagdv.terms,iv.terms)
      vcov.useme<-new.vcov[rows,cols]
      #vcov needs to be ordered in the same way as the means
      new.order<-match(names(means),row.names(vcov.useme))
      vcov.useme<-vcov.useme[new.order,new.order]
      #sample from the multivariate distribution defined here
      draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
      numerator<-apply(draws[,iv.terms] %>% as.matrix,1,sum) #if more than one lag of iv
      denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #if more than one lag dv
      lrm.distribution<-numerator/denominator
      #put this distribution in meaningful units
      lrm.distribution<-lrm.distribution * thisiv.sd
      returnrow<-summarize.distribution2(lrm.distribution)
    }
    #return this info (but not w/ lag)
    returnrow$iv<-getname(thisiv,to="oldname")
    return(returnrow)
  })
  longrundf<-rbind.fill(lroutput)
  #identify as longrun
  longrundf$type<-"longrun"
  #get rid of this when not applicable
  longrundf<-longrundf[!is.na(longrundf$mu),]
  
  ##########################################
  
  #finalize
  
  thism.estsdf<-rbind.fill(shortrundf,longrundf)
  thism.estsdf$mname<-mname
  thism.estsdf$seq<-i
  return(thism.estsdf)
})

#put together in df
estsdf<-rbind.fill(tmpoutput)
estsdf

#classify the shortrun pvals into pval class
tmp<-estsdf$type=="shortrun"
estsdf$pval.class[estsdf$pval<0.01 & tmp]<-"at alpha=0.01"
estsdf$pval.class[estsdf$pval>=0.01 & estsdf$pval<0.05 & tmp]<-"at alpha=0.05"
estsdf$pval.class[estsdf$pval>=0.05 & estsdf$pval<0.10 & tmp]<-"at alpha=0.10"
estsdf$pval.class[estsdf$pval>=0.10 & tmp]<-"not sig"
tmp<-is.na(estsdf$pval.class)
if(sum(tmp)>0)
  stop()

#########################################################
#########################################################

#FIT STATS

this.sequence<-seq_along(mods)
fitdf<-lapply(this.sequence,function(i) {
  #i<-10
  #get params
  m<-mods[[i]]
  #track progress
  print(paste("Calc fit for model",i,"of",max(this.sequence)))
  thisrow<-calcfits(m)
}) %>% rbind.fill
fitdf$mname<-modsdf$mname

#########################################################
#########################################################

#PUT TOGETHER

#put model info (from specs)
#together with estimates
#and fitdf

mergelist<-list(modsdf,estsdf,fitdf)
finaldf<-Reduce(
  function(...)
    merge(..., by="mname", all=T),
  mergelist
)

#add sample info
finaldf<-merge(
  finaldf,
  sampsdf,
  by=c("sample","dv")
)

#########################################################
#########################################################

#IDENTIFY PREFERRED MODELS
#for each dv, identify a preferred model
names(finaldf)
tmp<-finaldf$sample=="demsonly" & 
  finaldf$lagdv=="lagdv" & 
  finaldf$iv.lag==0 & 
  finaldf$control.lag==0 & 
  finaldf$controls=="controls" & 
  finaldf$effects=="re_regFE" & 
  finaldf$robust=="clrobSE" 
finaldf$pref[tmp]<-T
finaldf$pref[!tmp]<-F

tmp<-finaldf$mname[tmp] %>% 
  unique
if(length(tmp)!=length(unique(modsdf$dv)))
  stop("Not unique models.")

#########################################################
#########################################################

#for perusing, save estimates
setwd(outputdir)
write.csv(
  finaldf,
  "allests.csv",
  row.names=F
)

#save out
setwd(filesdir)
save.image("ests.RData")

#########################################################
#########################################################

#browse

