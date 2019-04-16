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

#load the robustness mods
load('robusts.RData')

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

#plotting prelims
require(ggthemes)
require(extrafont)
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

##COEF TABLE/GRAPH
#for output
#pref models
#sparse models
#key iv
#key dvs

tmp<-rep(T,nrow(estsdf))
tmp<-tmp & estsdf$mod%in%c(
  "pref_clSE"
)
tmp<-tmp & estsdf$iv%in%c(
  "nv_regime_dur"
)
tmp<-tmp & estsdf$dv%in%c(
  "v2xdl_delib",
  "v2x_partip",
  "v2x_egal"
)
plotdf<-estsdf[tmp,]

#plot estimates in terms of SD's
tmpfunction<-function(x,dv)
  x/sdsdf$sd[sdsdf$var==dv]
tmpseq.i<-1:nrow(plotdf)
plotdf$mu<-sapply(tmpseq.i,function(i) {
  tmpfunction(plotdf$mu[i],plotdf$dv[i])
})
plotdf$mu.min<-sapply(tmpseq.i,function(i) {
  tmpfunction(plotdf$mu.min[i],plotdf$dv[i])
})
plotdf$mu.max<-sapply(tmpseq.i,function(i) {
  tmpfunction(plotdf$mu.max[i],plotdf$dv[i])
})

#put in order
tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp,decreasing=T)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname)

#make factor
plotdf$dv.disp<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#fix ivs
tmp<-sapply(plotdf$iv,getorder)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$iv.disp<-factor(
  plotdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

#fix effects
tmplevels<-c("pref_clSE","sparse_clSE","sparse_all_clSE")
tmplabels<-c(
  "Controls, Country RE, Regional FE",
  "Country RE, Regional FE (Restricted)",
  "Country RE, Regional FE (All Obs.)"
)
plotdf$mod<-factor(
  plotdf$mod,
  levels=tmplevels,
  labels=tmplabels
)

#add pval info to shape of point
plotdf$pval.shp<-NA
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
plotdf$pval.shp<-factor(
  plotdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)
# #limit to those that are in the dataset
# tmp<-levels(plotdf$pval.shp)%in%plotdf$pval.shp
# tmpshapes<-tmpshapes[tmp]
# shp.labels<-shp.labels[tmp]

# #if there is some alpha=0.10 below, 
# #this needs to be adjusted
# tmp<-plotdf$pval.class=="at alpha=0.10"
# if(sum(tmp)>0)
#   stop('adjust graph below')

#make graph
g.tmp<-ggplot(
  plotdf,
  aes(
    x=dv.disp,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    shape=pval.shp
  )
) + 
  geom_point(
  ) +
  geom_errorbar(
    size=0.3,
    width=0.2
  ) + 
  geom_hline(
    yintercept=0,
    color='red',
    linetype='dashed'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  coord_flip() +
  xlab("") + ylab("\nLong-Run Effect (in SDs)") + 
  facet_wrap(
    ~ mod + iv.disp,
    nrow=3,ncol=2
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

gs.list[["fig_prefests"]]<-list(
  graph=g.tmp,
  filename="fig_prefests.pdf",
  width=8,
  height=6
)

#########################################################
#########################################################

#OUTPUT
#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  #embed font
  embed_fonts(
    file="tmp.pdf",
    outfile=thiselement$filename
  )
  file.remove(
    "tmp.pdf"
  )
}

