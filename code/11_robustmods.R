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

#load the image
setwd(datadir)
setwd(filesdir)
load('ests.RData')

#load cow info
setwd(datadir)
load("cowcodes.RData")

# #load helper functions
setwd(codedir)
source('functions.R')
source('getlongrun2.R')
setwd(homedir)

#set seed
set.seed(23)
reps<-10000

#########################################################
#########################################################

#ESTIMATE ROBUSTNESS MODS
#models for robustness output

#models are named externally
setwd(metadir); dir()
rawrobdf<-read.csv(
  'robustness.csv',
  stringsAsFactors=F
)

#those which are NA, are deprecated
rawrobdf<-rawrobdf[!is.na(rawrobdf$order),]

#make a table, for output
rawrobdf$letter<-
  paste0(
    "(",LETTERS[1:nrow(rawrobdf)],")"
  )
setwd(outputdir)
write.csv(
  rawrobdf[,c("letter","propername")],
  "tab_robmods.csv",
  row.names=F
)

#to be used below
getmodname<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$propername[rawrobdf$mname==mname]
}

getmodorder<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$order[rawrobdf$mname==mname]
}

#to loop through
robdf<-expand.grid(
  mod=rawrobdf$mname,
  dv=dvs,
  stringsAsFactors=F
)
#except for cgv, which estimated separately below
tmp<-robdf$mod!="cgvspells"
robdf<-robdf[tmp,]
#clustered SE's is default
robdf$mod<-paste0(
  robdf$mod,"_clSE"
)
#if want non-clustered SE's,
#have to add code here

#trim?
tmp<-rep(T,nrow(robdf))
#tmp<-tmp & robdf$dv%in%c("v2xdl_delib")#,"v2x_liberal")
#tmp<-tmp & robdf$mod%in%c("pref_clSE","pretrans_clSE")
tmp<-tmp & robdf$dv!="progtaxes" #giving probs

# #mandatory trim
# tmp<-tmp & !(robdf$mod=="re_lme4_clSE")
# tmp<-tmp &
#   !(
#     robdf$mod%in%c("bvrt_clSE","bvrt_all_clSE") &
#       robdf$dv=="v2x_suffr"
#   )


#full trim
robdf<-robdf[tmp,]
robdf$seq<-1:nrow(robdf)

#LOOP!
tmp.seq<-1:nrow(robdf)
tmpoutput<-lapply(tmp.seq,function(i) {
  
  #i<-17
  #i<-which(robdf$dv=="v2xdl_delib" & robdf$mod=="nobigoil_clSE")
  #get param
  thisdv<-robdf$dv[i]
  thismod<-robdf$mod[i]
  thismod.base<-str_replace(thismod,"\\_clSE$","")
  #track progress
  print("####")
  print(
    paste(
      "Estimating model",
      i,"of",max(tmp.seq)
    )
  )
  print(thisdv)
  print(thismod)
  
  #############
  
  #PREPATORY
  
  #get sample and formula from pref
  tmp<-finaldf$dv==thisdv &
    finaldf$pref
  this.sampname<-unique(finaldf$sampname[tmp])
  this.samp<-samps[[this.sampname]]
  this.mname<-unique(finaldf$mname[tmp])
  #thiseffect<-unique(finaldf$effects[tmp])
  # thismodel.arg<-ifelse(
  #   thiseffect=="fe",
  #   "within",
  #   "random"
  # )
  thismodel.arg<-"random" #default pref.
  thisform<-forms[[this.mname]]
  islmer<-F #most w/ plm
  #base sample will be helpful
  base.samp<-demdf$gwf_st>1950 &
    demdf$gwf_dem==1 &
    !is.na(demdf$gwf_st) &
    !is.na(demdf$gwf_dem)
  
  #############
  
  #which model?
  #redefine samp/form as necessary
  #and then re-estimate below
  
  #############
  
  #SET UP FORMULA/SAMPLE
  
  if(thismod.base=="pref") {
    
    #don't change anything
    
    
  } else if (thismod.base=="nopretrans") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #are pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      #if yes, remove
      thisform<-str_replace(
        oldform,
        "precamp_cs + durcamp_cl +",""
      )
    } else {
      #don't change anything
    }
    thisform<-thisform %>%
      as.formula
    
  } else if (thismod.base=="pretrans") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #are pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      #don't change anything
    } else {
      #if no, add
      thisform<-paste0(
        oldform,
        "+ precamp_cs + durcamp_cl"
      )
    }
    thisform<-thisform %>%
      as.formula
    
  } else if (thismod.base=="pretrans_sfc") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #are pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      #don't change anything
    } else {
      #if no, add
      thisform<-paste0(
        oldform,
        "+ precamp_cs + durcamp_cl + durcamp_sfc"
      )
    }
    thisform<-thisform %>%
      as.formula
    
  } else if (thismod.base=="pretrans5") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #is pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      thisform<-str_replace(
        oldform,
        "durcamp_cl",
        "pretrans5_cl"
      )
    } else {
      thisform<-paste0(
        oldform,
        "+ precamp_cs + pretrans5_cl"
      )
    }
    thisform<-thisform %>%
      as.formula
    
  } else if (thismod.base=="pretrans10") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #is pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      thisform<-str_replace(
        oldform,
        "durcamp_cl",
        "pretrans10_cl"
      )
    } else {
      thisform<-paste0(
        oldform,
        "+ precamp_cs + pretrans10_cl"
      )
    }
    thisform<-thisform %>%
      as.formula
    
  } else if (thismod.base=="pretransCSR") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    #is pretrans already there?
    tmp<-str_detect(oldform,"precamp_cs") &
      str_detect(oldform,"durcamp_cl")
    if(tmp) {
      thisform<-str_replace(
        oldform,
        "precamp_cs",
        "precamp_csr"
      )
    } else {
      thisform<-paste0(
        oldform,
        "+ precamp_csr + durcamp_cl"
      )
    }
    thisform<-thisform %>%
      as.formula
    
  } else if(thismod.base=="bvrt") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(oldform,"(\\sv_regime_dur)(.*)$","\\1") %>%
      as.formula
    
  } else if (thismod%in%c("bvrt_all","bvrt_all_clSE")) {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(oldform,"(\\sv_regime_dur)(.*)$","\\1") %>%
      as.formula
    this.samp<-base.samp
    
  } else if (thismod.base=="sparse") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "(\\bL\\.growthrate)(.*)$",
      "\\1"
    ) %>% str_replace(
      "\\+\\sfactor\\(colruler\\)",
      ""
    ) %>% as.formula
    
  } else if (thismod.base=="sparse_all") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "(\\bL\\.growthrate)(.*)$",
      "\\1"
    ) %>% str_replace(
      "\\+\\sfactor\\(colruler\\)",
      ""
    ) %>% as.formula
    this.samp<-base.samp
    
  } else if (thismod.base=="tgame") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "(L.exclpop)",
      "\\1 + L.landlords + L.gini_generous"
    ) %>%
      as.formula
    this.samp<-base.samp
    
  }  else if (thismod.base=="wpolity") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "(L.exclpop)",
      "\\1 + L.log_total_igo_links"
    ) %>%
      as.formula
    
  } else if (thismod.base=="trade") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "(L.exclpop)",
      "\\1 + L.ustradepct + 
      L.chinatradepct + 
      L.eutradepct + 
      L.russiatradepct"
    ) %>%
      as.formula
    
  } else if (thismod.base=="4lags") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    lagdv.regex<-paste0("L\\.",thisdv)
    lagdv4lags<-paste0(
      c("L.","L2.","L3.","L4."),
      thisdv,
      collapse=" + "
    )
    thisform<-str_replace(
      oldform,
      lagdv.regex,
      lagdv4lags
    ) %>% as.formula
    
  } else if (thismod.base=="fe") {
    
    thismodel.arg<-"within"
    
  } else if (thismod.base=="dummy") {
    
    #replace w/ the nv_regime_dur dummy
    #and the v_regime_dur dummy
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      "regime_dur",
      "regime_dum"
    ) %>% as.formula
    # #don't restrict to democracies,
    # #b/c transition dummies are constants
    # but this is only true
    # this.samp<-demdf$gwf_st>1950 &
    #   !is.na(demdf$gwf_st)
    
  } else if (thismod.base=="logdur") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "\\bnv_regime_dur",
      "lognv_regime_dur"
    ) %>% str_replace(
      "\\bv_regime_dur",
      "logv_regime_dur"
    ) %>% as.formula
    
  } else if (thismod.base=="droplong") {
    
    #drop longest nonviolent campaign
    tmpval<-demdf$nv_regime_dur[base.samp] %>%
      unique %>% max
    this.samp<-base.samp & demdf$nv_regime_dur!=tmpval
    
  } else if (thismod.base=="altcoding") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "\\bnv_regime_dur",
      "nv_regime_dur_alt"
    ) %>% as.formula
    
  } else if (thismod.base=="navco") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "\\bnv_regime_dur",
      "navco_nv_dur"
    ) %>% str_replace(
      "\\bv_regime_dur",
      "navco_v_dur"
    ) %>% as.formula
    
  } else if (thismod.base=="allobs") {
    
    #don't restrict to democracies
    this.samp<-demdf$gwf_st>1950 &
      !is.na(demdf$gwf_st)
    
  } else if (thismod.base=="allobs_fe") {
    
    #don't restrict to democracies
    this.samp<-demdf$gwf_st>1950 &
      !is.na(demdf$gwf_st)
    #make FE
    thismodel.arg<-"within"
    
    # } else if (thismod.base=="re_lme4") {
    #
    #   #formula will change
    #   thisform<-deparse(thisform) %>%
    #     paste0(collapse="") %>%
    #     str_replace(
    #       "factor\\(year\\)",
    #       "factor(year) + (1 | ccode)"
    #     ) %>%
    #     as.formula
    #   islmer<-T
    
  } else if (thismod.base=="earlystart") {
    
    this.samp<-demdf$gwf_st>1945 &
      demdf$gwf_dem==1 &
      !is.na(demdf$gwf_st) &
      !is.na(demdf$gwf_dem)
    
  } else if (thismod.base=="latestart") {
    
    this.samp<-demdf$gwf_st>1950 &
      demdf$gwf_st<2000 & 
      demdf$gwf_dem==1 &
      !is.na(demdf$gwf_st) &
      !is.na(demdf$gwf_dem)
    
  } else if (thismod.base=="nobigoil1") {
    
    #drop the three biggest oil producers
    tmpccodes<-tapply(
      demdf$logoilpc[this.samp],
      demdf$ccode[this.samp],
      max
    ) %>% sort %>% rev
    bigoilcs<-head(names(tmpccodes),1)
    unique(demdf$countryname[demdf$ccode%in%bigoilcs])
    this.samp<-this.samp & 
      !demdf$ccode%in%bigoilcs
    
  } else if (thismod.base=="nobigoil3") {
    
    #drop the three biggest oil producers
    tmpccodes<-tapply(
      demdf$logoilpc[this.samp],
      demdf$ccode[this.samp],
      max
    ) %>% sort %>% rev
    bigoilcs<-head(names(tmpccodes),3)
    unique(demdf$countryname[demdf$ccode%in%bigoilcs])
    this.samp<-this.samp & 
      !demdf$ccode%in%bigoilcs
    
  } else if (thismod.base=="justoil") {
    
    #bivariate: oil and democracy
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace(
      oldform,
      "\\bnv_regime_dur.*",
      ""
    ) %>%
      paste0(
        "L.logoilpc"
      ) %>%
      as.formula
    
  } else {
    
    stop(
      print(paste(thismod,"not implemented."))
    )
    this.samp<-rep(F,nrow(demdf))
    
  }
  
  #############
  
  #ESTIMATION
  
  #get the df
  tmpdf<-demdf[this.samp,]
  regvars<-all.vars(thisform)
  ssvars<-c("ccode","year",regvars) %>% unique
  prez<-complete.cases(tmpdf[,ssvars])
  tmpdf<-tmpdf[prez,ssvars]
  
  if(nrow(tmpdf)>0) {
    
    #estimate model
    if(!islmer) {
      m.tmp<-plm(
        data=tmpdf,
        form=thisform,
        model=thismodel.arg
      )
    } else {
      m.tmp<-lmer(
        data=tmpdf,
        form=thisform
      )
    }
    
    #get vcov
    if(str_detect(thismod,"\\_clSE$")) {
      #robust SE
      vcov.tmp<-vcovHC(
        m.tmp,
        type="HC1",
        cluster="group"
      )
    } else {
      vcov.tmp<-vcov(m.tmp)
    }
    
    #loop through regvars,
    #except for the dv, lagdv, year
    tmp<-regvars%in%c(
      thisdv,
      paste0("L.",thisdv),
      "year",
      "region_name",
      "colruler"
    )
    loopvars<-regvars[!tmp]
    
    returndf<-lapply(loopvars,function(x) {
      
      #x<-"nv_regime_dur"
      
      tmp<-sdsdf$var==x
      if(sum(tmp)!=1) {
        x.sd<-1
      } else {
        x.sd<-sdsdf$sd[sdsdf$var==x]
      }
      
      # #if dummy, adjust
      # if(str_detect(thismod,"dummy")) {
      #   x<-str_replace(x,"dur","dum")
      #   x.sd<-1
      # }
      # #if log, adjust
      # if(str_detect(thismod,"log")) {
      #   x<-paste0("log",x)
      #   x.sd<-sd(
      #     tmpdf[[x]]
      #   )
      # }
      # #if navco, adjust
      # if(str_detect(thismod,"navco")) {
      #   if(x=="nv_regime_dur") {
      #     x<-"navco_nv_dur"
      #     x.sd<-sd(
      #       tmpdf[[x]]
      #     )
      #   }
      #   if(x=="v_regime_dur") {
      #     x<-"navco_v_dur"
      #     x.sd<-sd(
      #       tmpdf[[x]]
      #     )
      #   }
      # }
      # #if alt coding, adjust
      # if(str_detect(thismod,"altcoding")) {
      #   if(x=="nv_regime_dur") {
      #     x<-"nv_regime_dur_alt"
      #     x.sd<-sd(
      #       tmpdf[[x]]
      #     )
      #   }
      # }
      
      #make sure that the var is in the model
      #else return NA
      tmpclass<-class(m.tmp)[1]
      if(tmpclass=="plm") {
        tmpcoefs<-m.tmp$coefficients
      } else if(tmpclass=="lmerMod") {
        tmpcoefs<-coef(summary(m.tmp))[,'Estimate']
      } else {
        stop("Not written for this kind of mod.")
      }
      if(x%in%names(tmpcoefs)) {
        y<-getlongrun2(
          m=m.tmp,
          vcov=vcov.tmp,
          dv=thisdv,
          iv=x,
          ivsd=x.sd
        )
      } else {
        y<-data.frame(mu=NA)
      }
      y
    }) %>% rbind.fill
    #add id info
    returndf$iv<-loopvars
    returndf$mod<-thismod
    returndf$dv<-thisdv
    if(thismod=="re_lme4") {
      returndf$N<-dim(attributes(m.tmp)$frame)[1]
    } else {
      returndf$N<-nrow(m.tmp$model)
    }
    tmpcows<-length(unique(tmpdf$ccode))
    returndf$N.cows<-length(unique(tmpdf$ccode))
  } else {
    returndf<-data.frame(
      mod=thismod
    )
  }
  
  #return
  return(returndf)
  
})

#make estsdf
estsdf<-rbind.fill(tmpoutput)

#########################################################
#########################################################

#ALTERNATIVE SPELLS
#use pzeworski rather than geddes

###PREP

#load p. data
require(haven)
setwd(filesdir); dir()
tmpdf<-read_dta(
  'CGV_dataset.dta'
) %>% as.data.frame
# write_dta(
#   tmpdf,
#   'CGV_dataset_AU.dta',
#   version=13
# )

##
#quick edits
#fill in nv_tran_dur
#add var for age of democracy

#vars have to be filled in,
#per spell, per country
roworder<-order(
  tmpdf$ccode,
  tmpdf$year
)
tmpdf<-tmpdf[roworder,]

idvars<-c("ccode","year")
newvars<-c("democracy","nv_tran_dur","v_tran_dur")

#loop through and define
tmpdf<-by(tmpdf,tmpdf$ccode,function(df) {
  #df<-tmpdf[tmpdf$ccode==770,]
  #print(unique(df$ccode))
  #get spell
  tmp<-c(1,diff(df$year))
  x<-rep(0,nrow(df))
  x[tmp>1]<-1
  y<-cumsum(x)
  df$cgv_casename<-paste0(
    unique(df$ctryname),
    "-",y+1
  )
  #fill in tran_dur vars
  df$nv_tran_dur<-na.locf(df$nv_tran_dur)#,na.rm=F)
  df$v_tran_dur<-na.locf(df$v_tran_dur)#,na.rm=F)
  #calculate cumulative years of dem
  #df[,c(idvars,newvars)]
  df$cumagedem<-
    (df$agedem[1] - 1) + 
    cumsum(df$democracy) - 1
  #calculate past dem age
  df$past_cgvdem_age<-tapply(df$cumagedem,df$cgv_casename,function(x) {
    rep(min(x),length(x))
  }) %>% unlist
  df$cgv_st<-tapply(df$year,df$cgv_casename,function(x) {
    rep(min(x),length(x))
  }) %>% unlist
  # tmpvars<-c(
  #   idvars,
  #   newvars,
  #   "cumagedem",
  #   "cgv_casename",
  #   "past_cgvdem_age",
  #   "cgv_st"
  # )
  # df[,tmpvars]
  #return
  df
}) %>% rbind.fill

###

#get vars you need from here
keepvars<-c(
  "ccode",
  "year",
  "ctryname",
  "nv_tran_dur",
  "v_tran_dur",
  "democracy",
  "region_democracy",
  "past_cgvdem_age",
  "cgv_st"
)
tmpdf<-tmpdf[,keepvars]
#rename
names(tmpdf)[names(tmpdf)=="democracy"]<-"cgvdem"
names(tmpdf)[names(tmpdf)=="region_democracy"]<-"region_cgvdem"
#merge into demdf
tmpdf<-merge(
  tmpdf,
  demdf,
  all=T
)

#get valid rows (i..e, sample)
theserows<-tmpdf$cgv_st>1950 & tmpdf$cgvdem==1 &
  !is.na(tmpdf$cgvdem)
#get valid cols
idvars<-c("ccode","year")
newvars<-c("nv_tran_dur","v_tran_dur","region_cgvdem","past_cgvdem_age")
oldvars<-lapply(forms,all.vars) %>%
  unlist %>% unique
#get df
regvars<-c(idvars,newvars,oldvars) %>%
  unique
regdf<-tmpdf[theserows,regvars]

#shouldn't have missing..
tmp<-!is.na(regdf$nv_tran_dur) & !is.na(regdf$v_tran_dur)
if(sum(!tmp)>0)
  stop('missing some tran')

#have to add tran dur vars to sds
durvars<-c(
  "nv_tran_dur",
  "v_tran_dur"
)
tmpsdsdf<-lapply(durvars,function(v) {
  data.frame(
    var=v,
    sd=sd(regdf[[v]]),
    range=quantile(
      regdf[[v]],
      c(0.2,0.8),
      na.rm=T
    ) %>% 
      diff %>% 
      unname,
    stringsAsFactors=F
  )
}) %>% rbind.fill
tmp<-sdsdf$var%in%c("nv_tran_dur","v_tran_dur")
if(sum(tmp)==0) {
  sdsdf<-rbind.fill(
    sdsdf,
    tmpsdsdf
  )
}

####MODELS
#loop through every dv
#run the preferred model
dvs<-unique(estsdf$dv) #same dvs as above
tmpseq.i<-seq_along(dvs)
tmpoutput<-lapply(tmpseq.i,function(i) {
  #i<-1
  #get params
  thisdv<-dvs[i]
  #track progress
  print("####")
  print(
    paste(
      "Estimating model",
      i,"of",max(tmp.seq)
    )
  )
  print(thisdv)
  #get formula from pref
  tmp<-finaldf$dv==thisdv & 
    finaldf$pref
  this.mname<-unique(finaldf$mname[tmp])
  thismodel.arg<-"random" #default pref.
  thisform<-forms[[this.mname]]
  #adjust
  oldform<-deparse(thisform) %>% 
    paste0(collapse="")
  thisform<-str_replace(
    oldform,
    "\\bnv_regime_dur",
    "nv_tran_dur"
  ) %>% str_replace(
    "\\bv_regime_dur",
    "v_tran_dur"
  ) %>% str_replace(
    "\\bv_regime_dur",
    "v_tran_dur"
  ) %>% str_replace(
    "\\bv_regime_dur",
    "v_tran_dur"
  ) %>% as.formula
  #get df
  tmpdf<-regdf
  regvars<-all.vars(thisform)
  regvars<-c("ccode","year",regvars) %>% unique
  prez<-complete.cases(tmpdf[,regvars])
  #further restriction
  tmpdf<-tmpdf[prez,regvars]
  #estimate
  if(nrow(tmpdf)>0) {
    m.tmp<-plm(
      data=tmpdf,
      form=thisform,
      model=thismodel.arg
    )
    #robust SE
    vcov.tmp<-vcovHC(
      m.tmp,
      type="HC1",
      cluster="group"
    )
    # keyvars<-c(
    #   "nv_tran_dur",
    #   "v_tran_dur"
    # )
    tmp<-regvars%in%c(
      thisdv,
      paste0("L.",thisdv),
      "year",
      "ccode",
      "region_name",
      "colruler"
    )
    loopvars<-regvars[!tmp]
    returndf<-lapply(loopvars,function(x) {
      #x<-keyvars[1]
      x.sd<-sdsdf$sd[sdsdf$var==x]
      getlongrun2(
        m=m.tmp,
        vcov=vcov.tmp,
        dv=thisdv,
        iv=x,
        ivsd=x.sd
      )
    }) %>% rbind.fill
    #add id info
    returndf$iv<-loopvars
    returndf$mod<-"cgvspells_clSE"
    returndf$dv<-thisdv
    returndf$N<-nrow(m.tmp$model)
    tmpcows<-length(unique(tmpdf$ccode))
    returndf$N.cows<-length(unique(tmpdf$ccode))
  } else {
    returndf<-data.frame(
      mod=thismod
    )
  }
  #return
  returndf
})

#put it together
cgvestsdf<-rbind.fill(tmpoutput)

#########################################################
#########################################################

#FINALIZE
estsdf<-rbind.fill(
  estsdf,
  cgvestsdf
)

#remove anything that wasn't estimated
tmp<-is.na(estsdf$mu)
missing<-unique(estsdf$mod[tmp])
print(missing)
estsdf<-estsdf[!tmp,]

#get modname
estsdf$modname<-sapply(estsdf$mod,getmodname)

#edit nv_tran to be nv_regime
estsdf$iv<-str_replace(
  estsdf$iv,
  "v_tran_dur",
  "v_regime_dur"
)

#save out
setwd(filesdir)
save.image("robusts.RData")

#########################################################
#########################################################