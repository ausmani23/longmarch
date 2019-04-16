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
setwd(homedir)

#set seed
set.seed(23)

#########################################################
#########################################################

#initial regtables list
regtables<-list()

#DEPRECTATED
# #PICK MODEL GROUPS
# 
# #later, can identify how to output these 
# #now, this is for examining covariates, 
# #ensuring that all is sensible
# 
# #so, for each DV, I output
# #bivariate RE
# #control RE
# #control RE/region FE
# #control country FE
# 
# conditions<-list(
#   #bivariate.re_regFE="tmpdf$controls=='bivariate' & tmpdf$effects=='re_regFE'",
#   controls.re_regFE="tmpdf$controls=='controls' & tmpdf$effects=='re_regFE'",
#   controls.re="tmpdf$controls=='controls' & tmpdf$effects=='re'",
#   controls.fe="tmpdf$controls=='controls' & tmpdf$effects=='fe'"
# )
# 
# #create a list of models for each dv
# tmp.seq<-seq_along(dvs)
# regtables<-lapply(tmp.seq,function(i) {
#   #i<-1
#   thisdv<-dvs[i]
#   #this is subsetted modsdf
#   tmpdf<-modsdf[modsdf$dv==thisdv,]
#   #loop through the conditions, 
#   #and get mnames for output
#   this.sequence2<-seq_along(conditions)
#   mnames<-sapply(this.sequence2,function(j) {
#     #j<-2
#     #get condition/parma
#     thiscond<-conditions[[j]]
#     thisrow<-eval(parse(text=thiscond))
#     #get best model w/in this group
#     #tmprank<-rankme(df$aic[thisrow],"aic")
#     #make sure this is just one model
#     y<-tmpdf$mname[thisrow] %>% 
#       unique
#     if(length(y)>1)
#       stop("Duplicate models")
#     return(y)
#   })
#   #show warning if something missing
#   tmp<-sapply(mnames,length)==0
#   if(sum(tmp)>0) {
#     print(conditions[tmp])
#     #warning("These models missing.")
#   }
#   #take missing model out
#   mnames<-mnames[!tmp] %>%
#     unlist
#   #return
#   return(mnames)
# })
# 
# names(regtables)<-dvs

#########################################################
#########################################################

#FINAL REGTABLE

#if standardized esitmates
finaldvs<-finaldf$dv %>% unique
tmp<-sdsdf$var%in%finaldvs
tmpdf<-sdsdf[tmp,]
names(tmpdf)<-c("dv","dvavg","dvsd","dvrange")
finaldf<-merge(
  finaldf,
  tmpdf,
  by="dv",
  all=T
)
for(v in c("mu","mu.min","mu.max","se"))
  finaldf[[v]]<-finaldf[[v]]/finaldf$dvsd
  
#browse ests
tmp<-finaldf$dv%in%c("v2x_polyarchy","v2x_liberal") &
  finaldf$iv=="nv_regime_dur" & 
  finaldf$type=="longrun"
finaldf[tmp,c("mu","mu.min","mu.max","pval.class")]

#full regtables only needed for the main vars
#one regtable, in effect, w/ five columns and pref model
mainvars<-c(
  #"v2x_polyarchy",
  #"v2x_liberal",
  "v2xdl_delib",
  "v2x_egal",
  "v2x_partip"
)

tmp<-finaldf$pref & finaldf$dv%in%mainvars
regtablef<-finaldf$mname[tmp] %>% 
  unique
regtables[["tab"]]<-
  regtablef

#if don't make the rest
regtables<-regtables["tab"]

#########################################################
#########################################################

#LOOP THROUGH GROUPS, MAKE TABLES

#sequence to loop through
this.sequence<-seq_along(regtables)
tmp.seq<-seq_along(regtables)
for(i in this.sequence) {
  
  #i<-1
  
  #get params
  thistab<-regtables[[i]]
  thistabname<-names(regtables)[i]
  
  #track progress
  print("#####")
  print(i)
  print(thistabname)
  
  #subset estimates df
  #subset big df w/ these models
  ssdf<-finaldf[finaldf$mname%in%thistab,]
  thisdv<-unique(ssdf$dv)
  is.ecm<-F #ADL estimated
  
  #######################################
  #######################################
  
  ###SHORT RUN
  print("SR")
  tmpdf<-ssdf[ssdf$type=="shortrun",]
  #these are the terms which will appear on the left
  #note: here we loop through terms and not vars,
  #as for the long run vars
  terms<-tmpdf$term %>% unique
  #these are models which will be columns
  mods<-tmpdf$mname %>% unique
  #order them, if a standard regtable
  if(thistabname!="tab") {
    neworder<-sapply(names(conditions),function(x) {
      #stop("DEPRECATED FOR NOW")
      #x<-"controls.re"
      x2<-str_split(x,"\\.")[[1]]
      tmp<-lapply(
        x2,
        function(bit)
          str_detect(mods,paste0("\\.",bit,"\\."))
      ) %>% as.data.frame 
      y<-which(apply(tmp,1,sum)==2)
      if(length(y)==0)
        y<- -999 #miss code
      y
    })
    neworder<-neworder[neworder!=-999]
  } else if(thistabname=="tab") {
    tmp<-sapply(mods,function(x) {
      finaldf$dv[finaldf$mname==x] %>%
        unique %>% getname(to="order")
    }) 
    neworder<-order(tmp)
  } else {
    stop("Not implemented.")
  }
  mods<-mods[neworder]
  
  #loop through and get
  srests<-lapply(seq_along(terms),function(i) {
    #i<-2
    thisterm<-terms[i]
    #print(thisiv)
    #get each estimate
    ivrows<-lapply(seq_along(mods),function(j) {
      #j<-4
      thismod<-mods[j]
      thisrow<-tmpdf$term==thisterm & 
        tmpdf$mname==thismod
      #get estimate
      estdf<-tmpdf[thisrow,]
      #use gimmie.est to get the display
      tmp<-apply(estdf[,c("mu","pval","se")],1,function(x)
        gimmie.est(x[1],x[2],x[3],nrow=2))
      tmp<-matrix(tmp,ncol=1)
      return(tmp)
    })
    #adjust all these rows to be equal
    maxrows<-max(sapply(ivrows,nrow))
    ivrows<-lapply(ivrows,function(r) {
      if(nrow(r)<maxrows) {
        x<-rep("",maxrows-nrow(r))
        y<-matrix(x,ncol=1)
        r<-rbind(r,y)
      }
      return(r)
    })
    ivrows<-Reduce(cbind,ivrows)
    ivrows<-data.frame(ivrows,stringsAsFactors=F)
    names(ivrows)<-mods
    ivrows$type<-c("est","se")
    ivrows$term<-thisterm
    
    return(ivrows)
  }) %>% rbind.fill
  
  #get the var on which this is based
  srests$iv<-sapply(srests$term,getname,to="oldname")
  
  #make a note of whether this is a dv
  tmp<-sapply(srests$term,getname,from="oldname",to="type")
  srests$isdv<-ifelse(tmp=="dv",T,F)
  
  #get the lag numbers
  srests$lag.numbers<-sapply(srests$term,getlag)
  #get the diff numbers
  srests$diff.numbers<-sapply(srests$term,getdiff)
  
  #make an appropriate label
  main<-sapply(srests$iv,getname)
  suffix<-sapply(srests$lag.numbers,function(x) {
    if(is.na(x)) {
      y<-""
    } else {
      y<-paste0("$_{t-",x,"}$")
    }
  })
  prefix<-sapply(srests$diff.numbers,function(x) {
    if(is.na(x)) {
      y<-""
    } else {
      y<-paste0(rep("$\\Delta$",x),collapse=" ")
    }
    return(y)
  })
  srests$label<-paste0(prefix," ",main,suffix)
  
  #order sr ests
  #order rows
  roworder<-sapply(srests$iv,getorder)
  tmp<-sum(is.na(roworder))
  if(tmp>0) {
    print(roworder[is.na(roworder)])
    stop("Fix varnames.csv")
  }
  newlevels<-srests$iv[order(roworder)] %>% 
    unique
  srests$iv<-factor(
    srests$iv,
    levels=newlevels
  )
  termorder<-order(
    -srests$isdv,
    srests$iv,
    srests$lag.number
  )
  srests<-srests[termorder,]
  
  #get cols
  #make label sparse
  srests$label[1:nrow(srests)%%2!=1]<-""
  #return lagdvs and ivs separately
  splitter<-which(diff(srests$isdv)!=0)
  srests.ldvs<-srests[1:splitter,c("label",mods)]
  srests.ivs<-srests[(splitter+1):nrow(srests),c("label",mods)]
  
  ########################
  
  #get longrun
  print("LR")
  tmpdf<-ssdf[ssdf$type=="longrun",]
  
  #adjust, if necessary
  # #a quick tweak will make interactions/squared easier
  # tmp<-!is.na(tmpdf$atvals)
  # tmpdf$iv[tmp]<-paste0(
  #   tmpdf$iv[tmp],
  #   ".",
  #   tmpdf$atvals.with[tmp],
  #   "=",
  #   tmpdf$atvals[tmp]
  # )
  
  #ivs, here
  #tmpdf$iv<-sapply(tmpdf$iv,getname,to="oldname")
  ivs<-tmpdf$iv %>% 
    unique

  lrests<-lapply(seq_along(ivs),function(i) {
    #i<-3
    thisiv<-ivs[i]
    #print(thisiv)
    #get each estimate
    ivrows<-lapply(seq_along(mods),function(j) {
      #j<-3
      thismod<-mods[j]
      thisrow<-tmpdf$iv==thisiv & 
        tmpdf$mname==thismod
      #get estimate
      estdf<-tmpdf[thisrow,]
      #use gimmie.est to get the display
      tmp<-gimmie.est2(
        mu=estdf$mu,
        pval.class=estdf$pval.class,
        mu.min=estdf$mu.min,
        mu.max=estdf$mu.max,
        nrow=2
      )
      tmp<-matrix(tmp,ncol=1)
      return(tmp)
    }) 
    #print(thisiv)
    #adjust all these rows to be equal
    maxrows<-max(sapply(ivrows,nrow))
    ivrows<-lapply(ivrows,function(r) {
      if(nrow(r)<maxrows) {
        x<-rep("",maxrows-nrow(r))
        y<-matrix(x,ncol=1)
        r<-rbind(r,y)
      }
      return(r)
    })
    #print(thisiv)
    ivrows<-Reduce(cbind,ivrows)
    ivrows<-data.frame(ivrows,stringsAsFactors=F)
    names(ivrows)<-mods
    #add some identifying information
    ivrows$type<-c("est","se")
    ivrows$iv<-thisiv
    return(ivrows)
  }) %>% rbind.fill
  
  #add the info to the rows
  roworder<-sapply(lrests$iv,getorder)
  tmp<-sum(is.na(roworder))
  if(tmp>0) {
    print(roworder[is.na(roworder)])
    stop("Fix varnames.csv")
  }
  newlevels<-lrests$iv[order(roworder)] %>% unique
  lrests$iv<-factor(
    lrests$iv,
    levels=newlevels
  )
  lrests$label<-sapply(lrests$iv,getname)
  #order rows 
  termorder<-order(
    lrests$iv,
    lrests$type
  )
  #order cols
  colorder<-mods
  #these are the lrests
  lrests<-lrests[termorder,c("label",colorder)]
  #make sparse
  lrests$label[1:nrow(lrests)%%2!=1]<-""
  
  #######################################
  #######################################
  
  modelinfo<-lapply(seq_along(mods),function(j) {
    #j<-2
    thismod<-mods[j]
    thismod.row<-modsdf$mname==thismod
    thisdv<-modsdf$dv[thismod.row]
    this.samp<-modsdf$sample[thismod.row]
    this.sampname<-sampsdf$sampname[sampsdf$dv==thisdv & sampsdf$sample==this.samp]
    this.sample<-samps[[this.sampname]]
    #info
    N.obs<-format(sum(this.sample),big.mark=",",scientific=F)
    N.countries<-demdf$ccode[this.sample] %>% unique %>% length
    range<-paste0(range(unique(demdf$year[this.sample])),collapse="-")
    avg.N<-tapply(demdf$year[this.sample],demdf$ccode[this.sample],length)
    avg.N<-round(mean(avg.N),1)
    #country/region fe or re?
    thiseffect<-modsdf$effects[thismod.row]
    if(thiseffect=="re") {
      countryhet<-"RE"
      reghet<-""
    } else if (thiseffect=="fe") {
      countryhet<-"FE"
      reghet<-"N//A"
    } else if (thiseffect=="re_regFE") {
      countryhet<-"RE"
      reghet<-"FE"
    }
    #year FE
    yearfe<-"FE"
    #colonizer FE
    colfe<-"Yes"
    #lag of dv
    # thislagdv<-modsdf$lagdv[thismod.row]
    # lagdv<-ifelse(thislagdv=="lagdv","Yes","No")
    #model stats
    thismod.df<-tmpdf[tmpdf$mname==thismod,] 
    adjr2<-thismod.df$adjr2 %>% unique
    adjr2<-sprintf("%.3f",adjr2)
    bic<-thismod.df$bic %>% unique
    bic<-sprintf("%.2f",bic)
    thiscol<-data.frame(
      c(
        N.obs,
        N.countries,
        range,
        avg.N,
        colfe,
        countryhet,
        reghet,
        yearfe,
        adjr2#,
        #bic
      ),
      stringsAsFactors = F
    )
    names(thiscol)<-thismod
    return(thiscol)
  }) 
  modelinfo<-Reduce(cbind,modelinfo)
  modelinfo$label<-c(
    "Observations",
    "Countries",
    "Range",
    "Avg. $N_{i}$",
    "Colonizer Dummies",
    "Country-Level",
    "Region-Level",
    "Year-Level",
    "Adj. $R^{2}$"#,
    #"BIC"
  )
  
  #######################################
  #######################################
  
  #put together
  
  if(is.ecm) {
    ldvfiller<-data.frame(label=c("","\\textit{Error Correction Rate}",""))
  } else {
    ldvfiller<-data.frame(label=c("","\\textit{Lagged Dep. Var}",""))
  }
  srfiller<-data.frame(label=c("","\\textit{Short-Run Impact}",""))
  lrfiller<-data.frame(label=c("","\\textit{Long-Run Multiplier}",""))
  infofiller<-data.frame(label=c("","\\textit{Model Info}",""))
  regtable<-rbind.fill(
    ldvfiller,
    srests.ldvs,
    srfiller,
    srests.ivs,
    lrfiller,
    lrests,
    infofiller,
    modelinfo
  )
  #make NA's blank
  regtable<-apply(regtable,2,function(x) {
    x[is.na(x)]<-""
    return(x)
  })
  #write out each of these
  setwd(outputdir)
  filename<-paste0(thistabname,"_regtable.csv")
  write.csv(
    regtable,
    filename,
    row.names=F
  )
  
}

