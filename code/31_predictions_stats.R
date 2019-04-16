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
require(data.table)

#set dirs
rootdir<-find_root(
  criterion=has_file('longmarch.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load the models
setwd(filesdir)
load("cfactuals.RData")

#load cow info
setwd(datadir)
load("cowcodes.RData")

# #load helper functions
setwd(codedir)
source('functions.R')
setwd(homedir)

#set seed
set.seed(23)

#########################################################
#########################################################

#EDITS

###

#ADD ACTUAL VALUES AT T=0

tmpdf<-unique(rawpredictdf[,c("case","year","demyear")])
extradf<-tmpdf[tmpdf$demyear==1,]
extradf$year<-extradf$year-1
extradf$demyear<-extradf$demyear-1
extradf<-rbind.fill(
  tmpdf,
  extradf
)
roworder<-order(
  extradf$case,
  extradf$year
)
extradf<-extradf[roworder,]

#get cows
tmpdf<-data.frame(
  gwf_casename=unique(extradf$case),
  stringsAsFactors=F
)
mergedf<-unique(
  demdf[!is.na(demdf$gwf_casename),
        c("ccode","gwf_casename")]
)
tmpdf<-merge(
  tmpdf,
  mergedf,
  all.x=T,
  by=c("gwf_casename")
)
names(tmpdf)[names(tmpdf)=="gwf_casename"]<-"case"
extradf<-merge(
  extradf,
  tmpdf,
  by=c("case"),
  all=T
)
tmp<-is.na(extradf$ccode)
if(sum(tmp)>0)
  stop('merge probs')

#might as well also add to rawpredictdf
rawpredictdf<-merge(
  rawpredictdf,
  tmpdf,
  all=T,
  by=c("case")
)
tmp<-nrow(rawpredictdf[is.na(ccode)])
if(tmp!=0)
  stop()

#get actual levels of dv
#from demdf
mergevars<-c(
  "ccode",
  "year"
)
newvars<-c(
  unique(rawpredictdf$dv)
)
tmpvars<-c(mergevars,newvars)
tmpdf<-demdf[,tmpvars] %>%
  unique

extradf<-merge(
  extradf,
  tmpdf,
  all.x=T,
  by=c("ccode","year")
)

#gather
#make this a fake cfactual
gathercols<-newvars
extradf<-gather_(
  extradf,
  "dv",
  "yhat",
  gathercols
)
extradf$cfactual<-"actual"
extradf<-data.table(extradf)

###

#ADD 0 TO ALL CFACTUALS

#we also want to add versions of this
#for each of the other counterfactuals
#for demyear = 0 
fakedf<-data.frame(
  demyear=0,
  cfactual=unique(rawpredictdf$cfactual),
  stringsAsFactors=F
)
tmpdf<-extradf
tmpdf$cfactual<-NULL
fakedf<-merge(
  tmpdf,
  fakedf,
  all.y=T,
  by="demyear"
)

#rbind this to rawpredictdf
rawpredictdf<-rbindlist(
  list(
    rawpredictdf,
    fakedf,
    extradf
  ),
  use.names=T,
  fill=T
)

###

#ADD STARTING VALS
#for each dv-case

tmpdf<-rawpredictdf[
  ,
  .(
    yhat0 = yhat[demyear==0]
  )
  ,
  by=c("dv","case")
  ]
tmpdf<-unique(tmpdf)
rawpredictdf<-merge(
  rawpredictdf,
  tmpdf,
  by=c("dv","case")
)

#yhatp gives progress since initial val
rawpredictdf[
  ,
  yhatP := yhat - yhat0
  ]

#########################################################
#########################################################

#CALCULATE GAIN

#idea is that, over life-course of democracy,
#all democracies progress a little bit. 
#they get a little bit more democratic
#but struggle democracies improve more
#calculate gain under each scenario, in each case

#gain has to be relative to starting point of spell.
#otherwise, since spells are of varying length, 
#trends could reflect changing compositino of sample

############

###ADD OVERALL PROGRESS
#this is average of all cases
#ordered by demyear

overalldf<-rawpredictdf[
  ,
  .(
    yhat=mean(yhat,na.rm=T),
    yhatP=mean(yhatP,na.rm=T),
    case="overall"
  )
  ,
  by=c("rep","demyear","cfactual","dv")
  ]

predictdf=rbindlist(
  list(
    rawpredictdf,
    overalldf
  ),
  fill=T
)

############

#to represent 'effect size', 
#i want each DV to be represented
#as in-sample SD from the starting value

#merge in SD information
tmpdvs<-predictdf$dv %>% unique
tmpdf<-sdsdf[sdsdf$var%in%tmpdvs,]
names(tmpdf)[names(tmpdf)=="var"]<-"dv"
tmpdf$dv<-as.character(tmpdf$dv)

predictdf<-merge(
  predictdf,
  tmpdf
)

#calculate sd above/below avg
predictdf[,yhatsd := (yhat-avg)/sd]
#calculate sd above/below starting point
predictdf[,yhatPsd := yhatP/sd]

############

#GET LEVELS
#this is for plotting trajectories
#plot in terms of sds above/below mean

levelsdf<-predictdf[
  cfactual!="actual",
  .(
    yhatP=median(yhatPsd),
    yhatP.max=quantile(yhatPsd,0.975),
    yhatP.min=quantile(yhatPsd,0.025),
    year=unique(year)
  )
  ,
  by=c("case","demyear","cfactual","dv")
  ]

###########

#GET DIFFERENCE W/ OBSERVED
#for plotting diff w/ observed
#this will be in terms of SD's
#as well..

tmpfunction<-function(x,y) {
  x - x[y=="observed"]
}
compdf<-predictdf[
  cfactual!="actual",
  .(
    yhatPdiff=tmpfunction(yhatPsd,cfactual),
    yhatP.obs=yhatPsd[cfactual=="observed"],
    cfactual=cfactual
  ),
  by=c("rep","demyear","case","dv")
  ]
compdf<-compdf[cfactual!="observed"]

###########

#GET DIFFERENCE W/ OBSERVED
#this summarizes differences across reps
#allows plotting of SE's associated w/ it, 
#but only at a single point (ie.,. singledemyear)

diffdf<-compdf[
  ,
  .(
    yhatPdiff=median(yhatPdiff),
    yhatPdiff.max=quantile(yhatPdiff,0.975),
    yhatPdiff.min=quantile(yhatPdiff,0.025)
  ),
  by=c("cfactual","demyear","case","dv")
  ]

###########

#GET IMPACT OF MOBILIZATION
#this collapses to a single contrast:
#mobilization vs. quiescence

mobgaindf<-compdf[
  ,
  .(
    mobgain=(
      yhatPdiff[cfactual=="nvlong"] - 
        yhatPdiff[cfactual=="nvabsent"]
    )
  )
  ,
  by=c("dv","rep","case","demyear")
  ]
mobgaindf<-mobgaindf[
  ,
  .(
    gain=median(mobgain),
    maxgain=quantile(mobgain,0.975),
    mingain=quantile(mobgain,0.025)
  ),
  by=c("dv","case","demyear")
  ]

###########

#GET PCT PROGRESS EXPL BY MOBILIZATION
#of the progress made in the observed world,
#how much would vanish if there had
#been no mobilization at all

pctdf<-predictdf[
  ,
  .(
    #how much of yhatgain does not happen
    #in nvabsent, thatdoes happen in observed
    gain.obs=(
      yhatPsd[cfactual=="observed"]
    ),
    gain.absent=(
      yhatPsd[cfactual=="nvabsent"]
    ),
    gain.mob=(
      yhatPsd[cfactual=="nvlong"]
    )
  )
  ,
  by=c("dv","case","rep","demyear")
  ]

pctdf[,pctdiff.loss := 100 * (1 - gain.absent/gain.obs)]
pctdf[,pctdiff.gain := 100 * gain.mob/gain.obs - 100]

#clean this up to get sums
#by gain, and by loss
pctdf$gain.obs<-
  pctdf$gain.absent<-
  pctdf$gain.mob<-NULL

pctdf<-gather(
  pctdf,
  stat,
  pctdiff,
  pctdiff.loss:pctdiff.gain
)
pctdf<-data.table(pctdf)
pctdf[,stat:=str_replace(stat,"pctdiff\\.","")]

pctdf.sum<-pctdf[
  case=="overall" & demyear!=0,
  .(
    pctdiff=median(pctdiff),
    pctdiff.max=quantile(pctdiff,c(0.975)),
    pctdiff.min=quantile(pctdiff,c(0.025))
  ),
  by=c("dv","case","demyear","stat")
  ]

###########

#GET PREDICTION ERROR



#########################################################
#########################################################

#save out as cfactualstats
setwd(filesdir); dir()
save.image(
  "cfactuals_stats.RData"
)