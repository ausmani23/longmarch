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
require(tidyr)

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

#load helper functions
setwd(codedir)
source('functions.R')
setwd(homedir)

#set seed
set.seed(23)
reps<-5000

#########################################################
#########################################################

#plotting prelims
require(ggthemes)
require(extrafont)
require(scales)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(device = "win",quiet=T) #register w/ windows
#fonts()
Sys.setenv(
  R_GSCMD = gsdir_full
)
#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#DESCRIPTIVE STATISTICS
#take all vars used/mentioned
#generate simple descritpitve stats table

#DATA AVAILABILITY/SUMMARY

#for vars in the main regressions
prefmodnames<-finaldf$mname[finaldf$pref] %>%
  unique
prefmods<-mods[prefmodnames]
sumvars<-lapply(prefmods,function(x) all.vars(x$formula)) %>%
  unlist %>% unique

#remove lagged dvs, regavg dvs, non-main dvs
tmp<-sapply(sumvars,getname,to="type")=="dv"
tmp<-tmp & str_detect(sumvars,"^L\\.")
tmp<-tmp | str_detect(sumvars,"regavg")
sumvars<-sumvars[!tmp]

#remove extradvs
extradvs<-varsdf$oldname[varsdf$maindem!=1 & varsdf$type=="dv"]
tmp<-sumvars%in%extradvs
sumvars<-sumvars[!tmp]

#remove id vars
idvars<-c(
  "year",
  "region_name",
  "colruler"
)
sumvars<-sumvars[!sumvars%in%idvars]

tmpdf<-lapply(sumvars,function(v) {
  #v<-"netmigrate"
  #get nice name
  varname<-getname(v)
  #get within-sample mean, within-country SD
  tmpdf<-demdf[samps$v2x_polyarchy.demsonly,]
  avg<-mean(tmpdf[[v]],na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  sd.all<-sd(tmpdf[[v]],na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  sd.within<-tapply(tmpdf[[v]],tmpdf$ccode,sd,na.rm=T) %>% 
    mean(na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  
  #bigdf$countryname[tmp] %>% unique
  #return
  data.frame(
    v,
    varname,
    avg,
    sd.all,
    sd.within,
    stringsAsFactors=F
  )
}) %>% rbind.fill
tmpdf

#add some hepful info from varnames
mergevars<-c("pretty","order","type")
mergedf<-varsdf[,mergevars]
names(mergedf)[names(mergedf)=="pretty"]<-"varname"
datadf<-merge(tmpdf,mergedf)

#row order,colorder
roworder<-order(
  datadf$order
)
colorder<-c(
  #these make no sense in same table as reg stats
  "varname",
  "type",
  "avg",
  "sd.all",
  "sd.within"
)
datadf<-datadf[roworder,colorder]
datadf

#save out
setwd(outputdir)
write.csv(
  datadf,
  "tab_descriptive.csv",
  row.names=F
)

#########################################################
#########################################################

#SAMPLE/TRANSITIONS/CAMPAIGN DETAILS
#cases in the sample,
#spell year, 
#campaign associated with them

#this will be a country-spell dataset, in effect
#but we are only considering democracies..
#and we also want info about in sample

#we want to identify all spells, spells that are in the 
#sample, and then also give information about 
#nv_regime_dur and v_regime_dur beforehand

demdf$insample<-samps$v2xdl_delib.demsonly
# sampcases<-demdf$gwf_casename[demdf$insample] %>% unique
# demdf$insample_cases<-demdf$gwf_casename%in%sampcases
demdf$insample_cases<-T #to get all demcases

tmp<-demdf$gwf_st>1950 & demdf$gwf_dem==1 & 
  !is.na(demdf$gwf_st) & !is.na(demdf$gwf_dem) &
  demdf$insample_cases
tmpdf<-demdf[tmp,]

#can make it easily, via by
tmpoutput<-by(tmpdf,tmpdf$gwf_casename,function(df) {
  #df<-tmpdf[tmpdf$gwf_casename=="Pakistan 88-99" & !is.na(tmpdf$gwf_casename),]
  #we want year range of the case
  startyear<-min(df$year)
  yr.rng<-paste0(
    range(df$year),
    collapse="-"
  )
  #nv campaign and v campaign info
  nv<-unique(df$nv_regime_dur)
  v<-unique(df$v_regime_dur)
  #obs in the regression sample
  obs_a<-sum(df$insample)
  obs_p<-sum(df$insample_cases)
  #country-case info
  countryname<-tail(df$countryname,1)
  casename<-unique(df$gwf_casename)
  #return
  data.frame(
    countryname,
    casename,
    startyear, 
    yr.rng,
    nv,
    v,
    obs_a,
    obs_p,
    obs=100 * obs_a/obs_p,
    stringsAsFactors=F
  )
})

#make the df
campdf<-rbind.fill(tmpoutput)

#also remove yrs from the countrynames
campdf$countryname<-str_replace(
  campdf$countryname,
  "[0-9]{4}\\-[0-9]{4}",""
)

#order rows and cols
roworder<-order(
  campdf$countryname,
  campdf$startyear
)
colorder<-c(
  "countryname",
  "yr.rng",
  "obs",
  "nv",
  "v"
)
campdf<-campdf[roworder,colorder]

#save out
setwd(outputdir)
write.csv(
  campdf,
  "tab_sample.csv",
  row.names=F
)

#how many regimes/countreis? 
length(campdf$countryname) 
length(unique(campdf$countryname)) 

#how many had mobilizations of more than a year
sum(campdf$nv>=1)
mean(campdf$nv)
quantile(campdf$nv,c(0.9))

#how many countries in our sample experience one spell
tmptab<-table(campdf$countryname)
sum(tmptab==1)
sum(tmptab!=1)
tmpnames<-names(tmptab[tmptab>1])
tmp<-campdf$countryname%in%tmpnames
campdf[tmp,]

#correlation between armed and unarmed?
head(campdf)
cor(campdf$nv,campdf$v)

#########################################################
#########################################################

# #CORRELATION MATRIX FOR DV'S
# #overall, within and between
# require(psych) #makes this easy
# 
# #summarized in a table
# #and summarized in a tile plot
# 
# tmp<-varsdf$type=="dv"
# idvars<-c("ccode","year")
# vdem.vars<-varsdf$oldname[tmp]
# tmpcols<-c(idvars,vdem.vars)
# 
# #in-sample,out-sample
# #overall,between,within
# 
# parsdf<-expand.grid(
#   sample=c("base","out"),
#   type=c("overall","between","within"),
#   stringsAsFactors=F
# )
# parsdf$tabname<-paste0(parsdf$sample,".",parsdf$type)
# 
# #loop through and produce tables
# tmpseq<-1:nrow(parsdf)
# cortabs<-lapply(tmpseq,function(i) {
#   thisrow<-parsdf[i,]
#   #get samp
#   samp<-thisrow$sample
#   if(samp=="base") {
#     this.samp<-demdf$gwf_st>1950 & 
#       demdf$gwf_dem==1 &
#       !is.na(demdf$gwf_st) & 
#       !is.na(demdf$gwf_dem)
#   } else {
#     this.samp<-rep(T,nrow(demdf))
#   }
#   #get df
#   tmpdf<-demdf[this.samp,tmpcols]
#   #get the right type of correlation
#   tmpoutput<-statsBy(tmpdf,"ccode")
#   mytype<-thisrow$type
#   if(mytype=="overall") {
#     returndf<-tmpoutput$raw
#   } else if(mytype=="between") {
#     returndf<-tmpoutput$rbg
#   } else if(mytype=="within") {
#     returndf<-tmpoutput$rwg
#   }
#   #remove ccode,year
#   row.names(returndf)<-str_replace(
#     row.names(returndf),
#     "(\\.bg$)|(\\.wg$)",""
#   ) 
#   colnames(returndf)<-str_replace(
#     colnames(returndf),
#     "(\\.bg$)|(\\.wg$)",""
#   ) 
#   tmprows<-row.names(returndf)%in%vdem.vars
#   tmpcols<-colnames(returndf)%in%vdem.vars
#   returndf[tmprows,tmpcols]
# })
# names(cortabs)<-parsdf$tabname
# 
# #output
# 
# #in-sample correlation table of the main dvs
# #overall
# tmptab<-cortabs$base.overall
# tmptab<-cortabs$out.overall
# #limit to maindvs
# tmp<-str_detect(varsdf$oldname,"v2") & 
#   varsdf$maindv==1 & varsdf$maindem==1
# maindvs<-varsdf$oldname[tmp]
# tmprows<-row.names(tmptab)%in%maindvs
# tmpcols<-colnames(tmptab)%in%maindvs
# tmptab<-tmptab[tmprows,tmpcols]
# 
# #make sparse
# for(i in 1:nrow(tmptab)) {
#   for(j in 1:ncol(tmptab)) {
#     if(i>=j) {
#       tmptab[i,j]<-""
#     } else {
#       tmptab[i,j]<-as.numeric(tmptab[i,j]) %>%
#         round(2) %>%
#         format(nsmall=2)
#     }
#   }
# }
# tmptab 
# tmpdf<-tmptab %>% 
#   as.data.frame
# names(tmpdf)<-sapply(names(tmpdf),getname,to="short") 
# tmpdf$row<-sapply(row.names(tmpdf),getname,to="short") 
# row.names(tmpdf)<-NULL
# idvars<-c("row"); othvars<-names(tmpdf)[!names(tmpdf)%in%idvars]
# colorder<-c(idvars,othvars)
# tmpdf<-tmpdf[,colorder]
# names(tmpdf)[1]<-""
# tmpdf[,2]<-NULL
# tmpdf
# setwd(outputdir)
# write.csv(
#   tmpdf,
#   "tab_maincors.csv",
#   row.names=F
# )
# 
# #for appendix
# #in-sample correlations of all dvs,
# #overall, between, within
# #table 
# tmptab<-cortabs$base.overall
# 
# #make sparse
# for(i in 1:nrow(tmptab)) {
#   for(j in 1:ncol(tmptab)) {
#     if(i>=j) {
#       tmptab[i,j]<-""
#     } else {
#       tmptab[i,j]<-as.numeric(tmptab[i,j]) %>%
#         round(2) %>%
#         format(nsmall=2)
#     }
#   }
# }
# tmptab 
# tmpdf<-tmptab %>% 
#   as.data.frame
# names(tmpdf)<-sapply(names(tmpdf),getname,to="short") 
# tmpdf$row<-sapply(row.names(tmpdf),getname,to="short") 
# row.names(tmpdf)<-NULL
# idvars<-c("row"); othvars<-names(tmpdf)[!names(tmpdf)%in%idvars]
# colorder<-c(idvars,othvars)
# tmpdf<-tmpdf[,colorder]
# names(tmpdf)[1]<-""
# tmpdf
# setwd(outputdir)
# write.csv(
#   tmpdf,
#   "tab_fullcors.csv",
#   row.names=F
# )
# 
# #this will be a tile graph
# names(cortabs)
# mynames<-c("base.overall","base.between","base.within")
# tmptabs<-cortabs[mynames]
# tmpseq<-seq_along(tmptabs)
# tmpdf<-lapply(tmpseq,function(k) {
#   #k<-1
#   tmptab<-tmptabs[[k]]
#   #make sparse
#   for(i in 1:nrow(tmptab)) {
#     for(j in 1:ncol(tmptab)) {
#       if(i>=j) {
#         tmptab[i,j]<- -999
#       } else {
#         tmptab[i,j]<-as.numeric(tmptab[i,j])
#       }
#     }
#   }
#   tmptab 
#   tmpdf<-tmptab %>% 
#     as.data.frame(stringsAsFactors=F)
#   tmpdf$xvar<-row.names(tmpdf)
#   row.names(tmpdf)<-NULL
#   idvars<-c("xvar"); othvars<-names(tmpdf)[!names(tmpdf)%in%idvars]
#   colorder<-c(idvars,othvars)
#   tmpdf<-tmpdf[,colorder]
#   
#   #put in graph form
#   tmpdf<-gather_(
#     tmpdf,
#     "yvar",
#     "cor",
#     othvars
#   )
#   tmp<-tmpdf$cor==-999 |
#     is.na(tmpdf$cor)
#   tmpdf<-tmpdf[!tmp,]
#   tmpdf$type<-mynames[k]
#   #return
#   tmpdf
# }) %>% rbind.fill
# 
# #plot
# plotdf<-tmpdf
# 
# #get parent
# parent.x<-sapply(plotdf$xvar,getname,to="parent")
# parent.y<-sapply(plotdf$yvar,getname,to="parent")
# plotdf$same<-parent.x==parent.y & 
#   !is.na(parent.x) & !is.na(parent.y)
# 
# #add asterixes
# #where it is a correlation above 0.8
# #and it is between parts of same component
# #+ sign
# #where it is a correlation abovce 0.8
# #and it is not between parts of same componeent
# #asterix
# highcors<-plotdf$cor>0.8
# plotdf$cor.bin<-""
# #DEPRECATE
# #plotdf$cor.bin[highcors & plotdf$same]<-"+"
# plotdf$cor.bin[highcors & !plotdf$same]<-"*"
# 
# #fix order of x and y
# tmplevels<-c(plotdf$xvar,plotdf$yvar) %>% unique
# tmporder<-sapply(tmplevels,getname,to="order")
# tmplevels<-tmplevels[tmporder]
# tmplabels<-sapply(tmplevels,getname,to="pretty")
# plotdf$xvar<-factor(
#   plotdf$xvar,
#   tmplevels,
#   tmplabels
# )
# plotdf$yvar<-factor(
#   plotdf$yvar,
#   rev(tmplevels),
#   rev(tmplabels)
# )
# 
# #fix order of facets
# tmp<-plotdf$type=="base.overall"
# plotdf<-plotdf[tmp,]
# # plotdf$type %>% 
# #   unique
# # tmplevels<-c(
# #   "base.overall",
# #   "base.between",
# #   "base.within"
# # )
# # tmplabels<-c(
# #   "Overall",
# #   "Between-Country",
# #   "Within-Country"
# # )
# # plotdf$type<-factor(
# #   plotdf$type,
# #   tmplevels,
# #   tmplabels
# # )
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=xvar,
#     y=yvar,
#     fill=cor,
#     label=cor.bin
#   )
# ) + 
#   geom_tile() +
#   geom_text() +
#   #facet_wrap(~ type,ncol=1) +
#   scale_fill_gradient(
#    name="",
#    low="#e7e1ef",
#    high="#dd1c77"
#   ) +
#   xlab("") + ylab("") +
#   theme_bw(
#     base_family="Garamond",
#     base_size=14
#   ) +
#   theme(
#     axis.text.x = element_text(
#       angle = 60,
#       hjust = 1
#     )
#   )
# #g.tmp
# 
# gs.list[["fig_dvcors.pdf"]]<-list(
#   graph=g.tmp,
#   filename="fig_dvcors.pdf",
#   width=10,
#   height=8
# )
# ?scale_label

#########################################################
#########################################################

# #PAK VS BRAZIL
# 
# 
# tmprows<-demdf$ccode==140 & demdf$year>1980 & 
#   !is.na(demdf$gwf_casename)
# idvars<-c("ccode","countryname","year")
# maindvs<-varsdf$oldname[varsdf$maindv==1 & varsdf$maindem==1]
# tmpcols<-c(
#   idvars,
#   maindvs
# )
# tmpdf<-demdf[tmprows,tmpcols]
# 
# 
# 
# #########################################################
# #########################################################
# 
# #BRAZIL GRAPH
# 
# tmprows<-demdf$ccode==140 & demdf$year>=1986 
# tmprows<-tmprows | (demdf$ccode==130 & demdf$year>=1980)
# tmprows<-tmprows & !is.na(demdf$gwf_casename)
# idvars<-c("ccode","countryname","year","gwf_casename")
# maindvs<-varsdf$oldname[varsdf$maindv==1 & varsdf$maindem==1]
# tmpcols<-c(
#   idvars,
#   maindvs
# )
# tmpdf<-demdf[tmprows,tmpcols]
# tmpdf$gwf_casename %>% unique
# tmpdf$gwf_casename<-NULL
# tapply(tmpdf$year,tmpdf$country,summary)
# 
# plotdf<-gather_(
#   tmpdf,
#   "dv",
#   "val",
#   maindvs
# )
# 
# #create diff from start
# tmplist<-list(plotdf$countryname,plotdf$dv)
# plotdf<-by(plotdf,tmplist,function(df) {
#   df$dyear<-df$year - df$year[1]
#   df$dval<-df$val - df$val[1]
#   df
# }) %>% rbind.fill
# 
# #put in order
# tmp<-sapply(plotdf$dv,getorder)
# tmplevels<-names(sort(tmp)) %>% 
#   unique
# tmplabels<-sapply(tmplevels,getname)
# tmplabels<-str_replace(tmplabels,"\\sDimension|\\sDemocracy","")
# plotdf$dv<-factor(
#   plotdf$dv,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# tmplevels<-c(
#   "Brazil",
#   "Ecuador"
# )
# tmplabels<-c(
#   "Brazil, 1986-2010",
#   "Ecuador, 1980-2010"
# )
# plotdf$countryname<-factor(
#   plotdf$countryname,
#   tmplevels,
#   tmplabels
# )
# 
# #plot
# tmptab<-table(plotdf$dyear)/5
# maxyear<-max(as.numeric(names(tmptab[tmptab==2])))
# plotdf<-plotdf[plotdf$dyear<=maxyear,]
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=dyear,
#     y=dval,
#     group=dv,
#     color=dv
#   )
# ) +
#   geom_line() +
#   geom_point() +
#   scale_color_discrete(
#     name=""
#   ) +
#   xlab("\nYears Since Transition") + ylab("Gains Since Transition\n") +
#   facet_wrap(
#     ~ countryname
#   ) +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) +
#   theme(
#     legend.position='top',
#     legend.direction='horizontal'
#   )
# 
# tmpname<-"fig_contrast.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=7,
#   height=4
# )
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=dyear,
#     y=val,
#     group=dv,
#     color=dv
#   )
# ) +
#   geom_line() +
#   geom_point() +
#   scale_color_discrete(
#     name=""
#   ) +
#   xlab("\nYears Since Transition") + ylab("Democracy Score\n") +
#   facet_wrap(
#     ~ countryname
#   ) +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) +
#   theme(
#     legend.position='top',
#     legend.direction='horizontal'
#   )
# 
# tmpname<-"fig_contrastraw.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=7,
#   height=4
# )
# 
# #########################################################
# #########################################################
# 
# #PAK GRAPH
# 
# tmprows<-demdf$ccode==770 & demdf$year>1980 & 
#   !is.na(demdf$gwf_casename)
# idvars<-c("ccode","countryname","year")
# maindvs<-varsdf$oldname[varsdf$maindv==1 & varsdf$maindem==1]
# tmpcols<-c(
#   idvars,
#   maindvs
# )
# tmpdf<-demdf[tmprows,tmpcols]
# 
# plotdf<-gather_(
#   tmpdf,
#   "dv",
#   "val",
#   maindvs
# )
# 
# #put in order
# tmp<-sapply(plotdf$dv,getorder)
# tmplevels<-names(sort(tmp)) %>% 
#   unique
# tmplabels<-sapply(tmplevels,getname)
# plotdf$dv<-factor(
#   plotdf$dv,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #plot
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=year,
#     y=val
#   )
# ) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(
#     ~ dv,
#     ncol=2
#   ) +  
#   geom_vline(
#     xintercept=1987.5,
#     color='red',
#     linetype='dashed'
#   ) + 
#   xlab("") + ylab("") +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) +
#   theme(
#     legend.position = c(0.75, 0.17)
#   )
# 
# gs.list[["fig_pakistan"]]<-list(
#   graph=g.tmp,
#   filename="fig_pakistan.pdf",
#   width=8,
#   height=10
# )
# 
# #########################################################
# #########################################################
# 
# #SA GRAPH
# 
# 
# tmprows<-demdf$ccode==560 & demdf$year>1980 & 
#   !is.na(demdf$gwf_casename)
# idvars<-c("ccode","countryname","year")
# maindvs<-varsdf$oldname[varsdf$maindv==1 & varsdf$maindem==1]
# tmpcols<-c(
#   idvars,
#   maindvs
# )
# tmpdf<-demdf[tmprows,tmpcols]
# 
# plotdf<-gather_(
#   tmpdf,
#   "dv",
#   "val",
#   maindvs
# )
# 
# #put in order
# tmp<-sapply(plotdf$dv,getorder)
# tmplevels<-names(sort(tmp)) %>% 
#   unique
# tmplabels<-sapply(tmplevels,getname)
# plotdf$dv<-factor(
#   plotdf$dv,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #plot
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=year,
#     y=val
#   )
# ) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(
#     ~ dv,
#     ncol=2
#   ) +  
#   geom_vline(
#     xintercept=1993.5,
#     color='red',
#     linetype='dashed'
#   ) + 
#   xlab("") + ylab("") +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) +
#   theme(
#     legend.position = c(0.75, 0.17)
#   )
# 
# gs.list[["fig_sa"]]<-list(
#   graph=g.tmp,
#   filename="fig_sa.pdf",
#   width=8,
#   height=10
# )
# 
# 
# #########################################################
# #########################################################
# 
# #OUTPUT
# #output graphlist
# setwd(outputdir)
# this.sequence<-seq_along(gs.list)
# for(i in this.sequence) {
#   thiselement<-gs.list[[i]]
#   ggsave(
#     filename="tmp.pdf",
#     plot=thiselement$graph,
#     width=thiselement$width,
#     height=thiselement$height
#   )
#   #embed font
#   embed_fonts(
#     file="tmp.pdf",
#     outfile=thiselement$filename
#   )
#   file.remove(
#     "tmp.pdf"
#   )
# }

