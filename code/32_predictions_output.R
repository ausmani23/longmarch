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
load("cfactuals_stats.RData")

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

#extra packages
require(mgcv)
require(tidyr)

#########################################################
#########################################################

#LEVELS ILLUSTRATION
#aggregated, by mean
tmp<-levelsdf$case=="overall"
tmp<-tmp & levelsdf$demyear<=10
tmp<-tmp & levelsdf$cfactual%in%c("observed","nvlong","nvabsent")
plotdf<-levelsdf[tmp,]

#put in order
tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#fix cf
tmp<-sapply(plotdf$cfactual,getorder.cf)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname.cf)
plotdf$cfactual<-factor(
  plotdf$cfactual,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=demyear,
    y=yhatP,
    group=cfactual,
    linetype=cfactual,
    shape=cfactual
  )
) + 
  geom_line() + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") +
  geom_point(size=0.5) +
  facet_wrap(
    ~ dv,
    ncol=2
  ) +
  xlab("\nYears After Transition") + 
  ylab("Gains in Standard Deviations\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) +
  theme(
    legend.position = c(0.75, 0.17)
  )

#add to list
gs.list[["g.allcfs"]]<-list(
  graph=g.tmp,
  filename="fig_allcfs.pdf",
  width=8,
  height=10
)

#########################################################
#########################################################

#DIFFERENCE ILLUSTRATION

#aggregated, by mean
tmp<-diffdf$case=="overall"
tmp<-tmp & diffdf$demyear<=10
tmp<-tmp & diffdf$cfactual%in%c("nvlong","nvabsent")
plotdf<-diffdf[tmp,]

#put in order
tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#fix cf
tmp<-sapply(plotdf$cfactual,getorder.cf)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname.cf)
plotdf$cfactual<-factor(
  plotdf$cfactual,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=demyear,
    y=yhatPdiff,
    group=cfactual,
    linetype=cfactual,
    shape=cfactual
  )
) + 
  geom_line() + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") +
  geom_point(size=0.5) +
  facet_wrap(
    ~ dv,
    ncol=2
  ) +
  xlab("\nYears After Transition") + 
  ylab("Gap with Observed in Standard Deviations\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) +
  theme(
    legend.position = c(0.75, 0.17)
  )

#add to list
gs.list[["g.diffcfs"]]<-list(
  graph=g.tmp,
  filename="fig_diffcfs.pdf",
  width=8,
  height=10
)

#########################################################
#########################################################

#GAIN ILLUSTRATION
#gain comparing nvlong to nvabsent,
#w/ year on the x axis

#trim
tmp<-mobgaindf$case=="overall"
tmp<-tmp & mobgaindf$demyear<=10
plotdf<-mobgaindf[tmp,]

#for the writeup
plotdf[plotdf$demyear==10,]

#put in order
tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp)) %>% 
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#change point.type if both are above or below 0
plotdf$sig<-"notsig"
tmp<-(plotdf$mingain<0 & plotdf$maxgain<0) |
  (plotdf$mingain>0 & plotdf$maxgain>0)
plotdf$sig[tmp]<-"sig"
plotdf$sig<-factor(
  plotdf$sig,
  levels=c("notsig","sig")
)
tmpshapes<-c(1,8)
names(tmpshapes)<-levels(plotdf$sig)

#plot
g.tmp<-ggplot(
  plotdf,
  aes(
    x=demyear,
    y=gain,
    ymin=maxgain,
    ymax=mingain
  )
) +
  geom_ribbon(
    alpha=0.1
  ) +
  geom_line() + 
  geom_line(
    aes(
      y=maxgain
    ),
    linetype='dotted'
  ) +
  geom_line(
    aes(
      y=mingain
    ),
    linetype='dotted'
  ) +
  #geom_point() + 
  geom_hline(
    yintercept=0,
    color='red',
    linetype='dashed'
  ) + 
  scale_shape_manual(
    guide=F,
    values=tmpshapes
  ) +
  xlab("\nYears After Transition") +
  ylab("Gains in Standard Deviations\n") + 
  facet_wrap(
    ~ dv,
    ncol=2
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 

#add to list
gs.list[["fig_mobgains"]]<-list(
  graph=g.tmp,
  filename="fig_mobgains.pdf",
  width=8,
  height=10
)

#########################################################
#########################################################

# #GAIN ILLUSTRATAION
# #gains, vs. observed
# 
# #trim for plotting
# diffdf
# tmp<-diffdf$case=="overall"
# tmp<-tmp & diffdf$demyear==10
# plotdf<-diffdf[tmp,]
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
# #fix cf
# tmp<-sapply(plotdf$cfactual,getorder.cf)
# tmplevels<-names(sort(tmp)) %>%
#   unique
# tmplabels<-sapply(tmplevels,getname.cf)
# plotdf$cfactual<-factor(
#   plotdf$cfactual,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #change point.type if both are above or below 0
# plotdf$sig<-"notsig"
# tmp<-(plotdf$mingain<0 & plotdf$maxgain<0) | 
#   (plotdf$mingain>0 & plotdf$maxgain>0)
# plotdf$sig[tmp]<-"sig"
# plotdf$sig<-factor(
#   plotdf$sig,
#   levels=c("notsig","sig")
# )
# tmpshapes<-c(1,8)
# names(tmpshapes)<-levels(plotdf$sig)
# 
# ###STANDARD DEVIATION GAIN
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=cfactual,
#     y=yhatdiff,
#     ymin=yhatdiff.min,
#     ymax=yhatdiff.max
#   )
# ) +
#   geom_point(
#     aes(
#       shape=sig
#     )
#   ) +
#   geom_errorbar(
#     width=0.2
#   ) +
#   geom_hline(
#     yintercept=0,
#     color='red',
#     linetype='dashed'
#   ) + 
#   scale_shape_manual(
#     guide=F,
#     values=tmpshapes
#   ) +
#   coord_flip() +
#   ylab("\nGains in Standard Deviations") + xlab("") +
#   facet_wrap(
#     ~ dv,
#     ncol=2
#   ) +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) 
# 
# #add to list
# gs.list[["fig_cfsdgains"]]<-list(
#   graph=g.tmp,
#   filename="fig_cfsdgains.pdf",
#   width=6,
#   height=9
# )

#PROGRESS ILLUSTRATION,
#how much pct of democratic progress
#vanishes if we take away mobilization
#how much pct more democratic progress,
#if we add mobilization to each scenario

pctdf.sum
tmp<-pctdf.sum$case=="overall"
tmp<-tmp & pctdf.sum$demyear==10
plotdf<-pctdf.sum[tmp,]

#put in order
tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp)) %>%
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$dv<-factor(
  plotdf$dv,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

#fix cf
plotdf$stat<-factor(
  plotdf$stat,
  levels=c("loss","gain"),
  labels=c(
    "Losses from Quiescence",
    "Gains from Mobilization"
  )
)

#change point.type if both are above or below 0
plotdf$sig<-"notsig"
tmp<-(plotdf$pctdiff.min<0 & plotdf$pctdiff.max<0) |
  (plotdf$pctdiff.min>0 & plotdf$pctdiff.max>0)
plotdf$sig[tmp]<-"sig"
plotdf$sig<-factor(
  plotdf$sig,
  levels=c("notsig","sig")
)
tmpshapes<-c(1,8)
names(tmpshapes)<-levels(plotdf$sig)

###PCT EXPLAINED
g.tmp<-ggplot(
  plotdf,
  aes(
    x=dv,
    y=pctdiff,
    ymin=pctdiff.min,
    ymax=pctdiff.max
  )
) +
  geom_point(
    aes(
      shape=sig
    )
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
    guide=F,
    values=tmpshapes
  ) +
  coord_flip() +
  ylab("\nProgress Explained (as %)") + xlab("") +
  facet_wrap(
    ~ stat,
    ncol=2
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

# #add to list
gs.list[["fig_progress"]]<-list(
  graph=g.tmp,
  filename="fig_progress.pdf",
  width=8,
  height=6
)

#########################################################
#########################################################

#save out graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(i)
  #i<-5
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
