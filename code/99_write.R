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

#load cow info
setwd(datadir)
load("cowcodes.RData")

# #load helper functions
setwd(codedir)
source('functions.R')
setwd(homedir)

#########################################################
#########################################################

#this file takes all output
#outputs various stats
#athat are useful for write up

writelist<-list()
writelist.filename<-"writestats.txt"

#load latest object
setwd(filesdir)
load("cfactuals_stats.RData")

#########################################################
#########################################################

#random stats
tmpsamp<-samps$v2x_polyarchy.demsonly
thisdf<-demdf[tmpsamp,]
writelist[['Ncountries']]<-length(unique(thisdf$ccode))

tmp<-tapply(thisdf$gwf_casename,thisdf$ccode,function(x) {
  length(unique(x))
})
sum(tmp==1)
sum(tmp>1)
sum(tmp>2)

#avg democracy score, by poilty2
demvars<-c(
  "v2x_polyarchy",
  "e_polity2"
)
plotdf<-lapply(demvars,function(demvar) {
  tmp<-tapply(demdf[[demvar]],demdf$year,mean,na.rm=T)
  data.frame(
    year=as.numeric(names(tmp)),
    avg=tmp,
    var=demvar,
    stringsAsFactors=F
  )
}) %>% rbind.fill

ggplot(
  plotdf,
  aes(
    x=year,
    y=avg,
    group=var,
    color=var
  )
) + 
  geom_line() 

#########################################################
#########################################################

#dist of campaign lengths
tmpsamp<-samps$v2x_polyarchy.demsonly
thisdf<-demdf[tmpsamp,]
tmp<-tapply(thisdf$nv_regime_dur,thisdf$gwf_casename,unique)
tmpdf<-data.frame(
  case=names(tmp),
  duration=unname(tmp),
  stringsAsFactors=F
)
tmpdf<-tmpdf[order(-tmpdf$duration),]

writelist[['campaigns']]<-nrow(tmpdf)
writelist[['avgcampdur']]<-mean(tmpdf$duration)
writelist[['medcampdur']]<-quantile(tmpdf$duration,c(0.5))
writelist[['0.1campdur']]<-quantile(tmpdf$duration,c(0.1))
writelist[['0.9campdur']]<-quantile(tmpdf$duration,c(0.90))
writelist[['0camps']]<-sum(tmpdf$duration==0)
writelist[['nonzerocamps']]<-sum(tmpdf$duration!=0)
writelist[['sdcamps']]<-sd(tmpdf$duration)

tmpdf$country<-str_replace(tmpdf$case,"\\s[0-9]{2}.*","")
tmp<-tapply(tmpdf$duration,tmpdf$country,sd) 
sum(!is.na(tmp))
sum(!is.na(tmp) & tmp>0)
tmp[!is.na(tmp) & tmp>0] %>% sort

tmp<-tapply(tmpdf$duration,tmpdf$country,function(x) {
  if(length(x)>1) {
    x
  } else {
    NA
  }
}) 
tmp[!is.na(tmp)] 

#########################################################
#########################################################

#how many dvs
writelist[['howmanydvs']]<-sum(varsdf$display,na.rm=T)

#########################################################
#########################################################

#reg ests
setwd(filesdir); dir()
load('ests.RData')

#these are the maindvs
tmp<-varsdf$maindem==1
maindvs<-varsdf$oldname[tmp]
tmp<-finaldf$iv=="nv_regime_dur" & 
  finaldf$type=="longrun" &
  finaldf$dv%in%maindvs
tmpdf<-finaldf[tmp,]

tmpseq.i<-1:nrow(tmpdf)
for(i in tmpseq.i) {
  #i<-1
  thisrow<-tmpdf[i,]
  thisdv<-thisrow$dv
  thisdv.sd<-sdsdf$sd[sdsdf$var==thisdv]
  ###w/o dv
  elname<-paste0(thisrow$dv,"_lrests")
  el<-paste0(
    format(round(thisrow$mu,1),1),
    " (95% CI: ",
    format(round(thisrow$mu.min,1),1),
    " - ",
    format(round(thisrow$mu.max,1),1),
    ")"
  )
  ###w dv sd
  elname<-paste0(thisrow$dv,"_lrests_std")
  el<-paste0(
    format(round(thisrow$mu/thisdv.sd,2),2),
    " (95% CI: ",
    format(round(thisrow$mu.min/thisdv.sd,2),2),
    " - ",
    format(round(thisrow$mu.max/thisdv.sd,2),2),
    ")"
  )
  writelist[[elname]]<-el
}

#how many variables have consistent impact on all five? 
head(finaldf)
tmp<-finaldf$type=="longrun" & 
  finaldf$dv%in%maindvs
tmpdf<-finaldf[tmp,]
ivs<-unique(tmpdf$iv)
tmpseq.i<-seq_along(ivs)
tmpdf<-lapply(tmpseq.i,function(i) {
  #i<-2
  thisiv<-ivs[i]
  thisdf<-tmpdf[tmpdf$iv==thisiv,]
  #all positive? 
  pos<-sum(thisdf$mu>0)
  sig<-sum(thisdf$pval.class!="not sig")
  data.frame(
    iv=thisiv,
    pos,sig,
    stringsAsFactors=F
  )
}) %>% rbind.fill

roworder<-order(
  -tmpdf$pos,
  -tmpdf$sig
)
tmpdf<-tmpdf[roworder,]
tmpdf

writelist[['ests_consistent']]<-sum(!tmpdf$pos%in%c(0,5))
writelist[['ests_notconsistent']]<-sum(tmpdf$pos%in%c(0,5))
10/22
sum(dbinom(c(1,5),5,0.5))

#########################################################
#########################################################

#robusts

#campaign dummies results
setwd(filesdir)
load('robusts.RData')
estsdf$mod %>% unique
tmp<-estsdf$mod=="dummy_clSE" &
  estsdf$dv%in%maindvs &
  estsdf$iv=="nv_regime_dur"
estsdf[tmp,]

#########################################################
#########################################################

#get cfactual stats
setwd(filesdir)
load('cfactuals_stats.RData')

#how many cases still around after twnety
rawpredictdf<-data.table(rawpredictdf)

tmpdf<-rawpredictdf[
  ,
  .(
    maxyear=max(demyear)
  ),
  by="case"
]
tmpdf[order(tmpdf$maxyear)]

median(tmpdf$maxyear)

#level ests
tmp<-levelsdf$demyear==10 & 
  levelsdf$case=="overall"
tmpdf<-levelsdf[tmp,]
# tmpseq.i<-1:nrow(tmpdf)
# for(i in tmpseq.i) {
#   thisrow<-tmpdf[i,]
#   elname<-paste0(thisrow$dv,"_levelgain")
#   el<-paste0(
#     format(round(thisrow$gain,2),2),
#     " (95% CI: ",
#     format(round(thisrow$mingain,2),2),
#     " - ",
#     format(round(thisrow$maxgain,2),2),
#     ")"
#   )
#   writelist[[elname]]<-el
# }

#cfests ests
tmp<-mobgaindf$demyear==10 & 
  mobgaindf$case=="overall"
tmpdf<-mobgaindf[tmp,]
tmpseq.i<-1:nrow(tmpdf)
for(i in tmpseq.i) {
  thisrow<-tmpdf[i,]
  elname<-paste0(thisrow$dv,"_mobgain")
  el<-paste0(
    format(round(thisrow$gain,2),2),
    " (95% CI: ",
    format(round(thisrow$mingain,2),2),
    " - ",
    format(round(thisrow$maxgain,2),2),
    ")"
  )
  writelist[[elname]]<-el
}

#cfests
tmp<-pctdf.sum$demyear==10
tmpdf<-pctdf.sum[tmp,]
tmpseq.i<-1:nrow(tmpdf)
for(i in tmpseq.i) {
  thisrow<-tmpdf[i,]
  elname<-paste0(thisrow$dv,"_",thisrow$stat,"_pctdiff")
  el<-paste0(
    format(round(thisrow$pctdiff,0),0),
    " (95% CI: ",
    format(round(thisrow$pctdiff.min,0),0),
    " - ",
    format(round(thisrow$pctdiff.max,0),0),
    ")"
  )
  writelist[[elname]]<-el
}

#how many predictions
writelist[['Npredictions']]<-nrow(rawpredictdf)

#########################################################
#########################################################

#write out writelist
tmpdir<-file.path(
  outputdir,
  "writingstats"
)
dir.create(tmpdir,showWarnings=F)
setwd(tmpdir)
file.remove(writelist.filename)

tmpseq.i<-seq_along(writelist)
for(i in tmpseq.i) {
  #i<-1
  thisname<-names(writelist)[i]
  thiscontent<-writelist[[i]]
  fullcontent<-paste0(
    "####\n",
    thisname,":","\t",thiscontent,
    "\n####","\n"
  )
  write(
    fullcontent,
    writelist.filename,
    append=T
  )
}


