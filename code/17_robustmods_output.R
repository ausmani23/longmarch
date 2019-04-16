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

#load the robustness mods
setwd(filesdir)
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

#browsing
tmp<-varsdf$maindv==1 & varsdf$maindem==1
maindvs<-varsdf$oldname[tmp]
tmp<-estsdf$dv%in%maindvs
tmpdf<-estsdf[tmp,]

#all that aren't significant
tmp<-str_detect(tmpdf$iv,"nv_regime") & 
  !str_detect(tmpdf$mod,"pref")
notsig<-tmp & tmpdf$pval.class=="not sig"
tmpdf[tmp,]; sum(notsig)
sig<-tmp & tmpdf$pval.class!="not sig"
tmpdf[tmp,]; sum(sig)

#pretrans
unique(tmpdf$mod)
tmp<-str_detect(tmpdf$mod,"pref|pretrans_sfc")
tmp<-tmp & str_detect(tmpdf$iv,"nv_regime")
tmpdf[tmp,]


#campaign lengths
unique(tmpdf$mod)
tmp<-str_detect(tmpdf$mod,"logdur|droplong|dummy|altcoding|navco")
tmp<-tmp & str_detect(tmpdf$iv,"nv_regime")
tmpdf[tmp,]

#varying controls
unique(tmpdf$mod)
tmptext<-"sparse|pretrans|4lags|wpolity|trade"
tmp<-str_detect(tmpdf$mod,tmptext)
tmp<-tmp & str_detect(tmpdf$iv,"nv_regime")
tmpdf[tmp,]
tmp<-tmp & tmpdf$pval.class=="not sig"
tmpdf[tmp,]


#varying sample
tmptext<-"latestart|earlystart|cgvspells"
tmp<-str_detect(tmpdf$mod,tmptext)
tmp<-tmp & str_detect(tmpdf$iv,"nv_regime")
tmpdf[tmp,]
tmp<-tmp & tmpdf$pval.class=="not sig"
tmpdf[tmp,]

#country fixed effects
tmptext<-"fe"
tmp<-str_detect(tmpdf$mod,tmptext)
tmp<-tmp & str_detect(tmpdf$iv,"nv_regime")
tmpdf[tmp,]
tmp<-tmp & tmpdf$pval.class=="not sig"
tmpdf[tmp,]


#########################################################
#########################################################

#we omit certain dv's, 
#which go undiscussed in the paper
tmp<-varsdf$display==F & !is.na(varsdf$display)
dvs.omit<-varsdf$oldname[tmp]
tmp<-estsdf$dv%in%dvs.omit
estsdf<-estsdf[!tmp,]

#set up shape and fill
estsdf$pval.fill<-NA
estsdf$pval.fill[estsdf$pval.class=="at alpha=0.01"]<-4
estsdf$pval.fill[estsdf$pval.class=="at alpha=0.05"]<-3
estsdf$pval.fill[estsdf$pval.class=="at alpha=0.10"]<-2
estsdf$pval.fill[estsdf$pval.class=="not sig"]<-1
negmu<-ifelse(estsdf$mu<0,-1,1)
estsdf$pval.fill<-estsdf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
table(estsdf$pval.fill)

#assign levels,colors
estsdf$pval.fill<-factor(
  estsdf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
require(RColorBrewer)
tmpcolors<-brewer.pal(8,"RdYlGn")
brewer.pal.info
#make the middle two grayer
# tmpcolors[4]<-""
# tmpcolors[5]<-""
names(tmpcolors)<-levels(estsdf$pval.fill)
fill.labels<-c(
  expression(paste(alpha==0.01,", ",beta<0)),
  expression(paste(alpha==0.05,", ",beta<0)),
  expression(paste(alpha==0.10,", ",beta<0)),
  expression(paste(beta<0)),
  expression(paste(beta>0)),
  expression(paste(alpha==0.10,", ",beta>0)),
  expression(paste(alpha==0.05,", ",beta>0)),
  expression(paste(alpha==0.01,", ",beta>0))
)

# plot estimates in terms of SD's
tmpfunction<-function(x,dv)
  x/sdsdf$sd[sdsdf$var==dv]
tmpseq.i<-1:nrow(estsdf)
estsdf$musd<-sapply(tmpseq.i,function(i) {
  tmpfunction(estsdf$mu[i],estsdf$dv[i])
})
estsdf$musd.min<-sapply(tmpseq.i,function(i) {
  tmpfunction(estsdf$mu.min[i],estsdf$dv[i])
})
estsdf$musd.max<-sapply(tmpseq.i,function(i) {
  tmpfunction(estsdf$mu.max[i],estsdf$dv[i])
})

#add pval shape
estsdf$pval.shp<-NA
estsdf$pval.shp[estsdf$pval.class=="at alpha=0.01"]<-1
estsdf$pval.shp[estsdf$pval.class=="at alpha=0.05"]<-2
estsdf$pval.shp[estsdf$pval.class=="at alpha=0.10"]<-3
estsdf$pval.shp[estsdf$pval.class=="not sig"]<-4
estsdf$pval.shp<-factor(
  estsdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(estsdf$pval.shp)

#get rootiv
estsdf$rootiv<-str_replace(
  estsdf$iv,
  "L([0-9])?\\.",
  ""
)

#########################################################
#########################################################

##TILEPLOT
#three dvs
#all ivs
plotdf<-estsdf

#only CLSE var
tmp<-str_detect(plotdf$mod,"clSE") 
plotdf<-plotdf[tmp,]

#only main dvs
tmp<-varsdf$maindv==1 & varsdf$maindem==1
maindvs<-varsdf$oldname[tmp]
tmp<-plotdf$dv%in%maindvs
#only ivs which are not lagdvs
tmp<-tmp & !plotdf$rootiv%in%maindvs

plotdf<-plotdf[tmp,]

tmp<-sapply(plotdf$dv,getorder)
tmplevels<-names(sort(tmp)) %>%
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#fix ivs
tmp<-sapply(plotdf$rootiv,getorder)
tmplevels<-names(sort(tmp)) %>%
  unique
tmplabels<-sapply(tmplevels,getname)
plotdf$rootiv<-factor(
  plotdf$rootiv,
  levels=tmplevels %>% rev,
  labels=tmplabels %>% rev
)

# #order the mods
# tmp<-sapply(plotdf$mod,getmodorder)
# tmplevels<-names(sort(tmp,decreasing=T)) %>%
#   unique
# tmplabels<-sapply(tmplevels,getmodname)
# #remove CRSE from label
# tmplabels<-str_replace(
#   tmplabels,
#   "\\s\\(CRSE\\)$",
#   ""
# )
# plotdf$mod<-factor(
#   plotdf$mod,
#   levels=tmplevels,
#   labels=tmplabels
# )

#order the mods
tmp<-sapply(plotdf$mod,getmodorder)
tmplevels<-names(sort(tmp,decreasing=F)) %>%
  unique
tmplabels<-LETTERS[1:length(tmplevels)]

#here, for assistance, 
#output which letters correspond 
tmpoutput<-paste0(
  tmplabels,
  ": ",
  tmplevels
)
setwd(metadir)
write(
  tmpoutput,
  "robustness_deets.txt"
)

plotdf$mod<-factor(
  plotdf$mod,
  levels=tmplevels,
  labels=tmplabels
)

#make this graph
g.tmp <- ggplot(
  plotdf,
  aes(
    x=mod,
    y=rootiv,
    fill=pval.fill
  )
) +
  geom_tile() + 
  facet_grid(
    ~dv
  ) + 
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("") +
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
tmpname<-"fig_robustness.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=14,
  height=8
)

# #########################################################
# #########################################################
# 
# #OUTPUT TABLES
# 
# #vars down the lhs
# #mods across the top
# #and then modelkey across the top
# head(estsdf)
# 
# #limit this to models that you want
# estsdf$mod %>% unique
# tmp<-str_detect(estsdf$mod,"clSE")
# tmp<-estsdf$iv=="nv_regime_dur"
# estsdf<-estsdf[tmp,]
# 
# #to flip, paste iv and mod together
# estsdf$ivmod<-paste0(estsdf$iv,".",estsdf$mod)
# #we want this to be ordered correctly, before flip
# tmplevels<-unique(estsdf$ivmod)
# estsdf$ivmod<-factor(
#   estsdf$ivmod,
#   levels=tmplevels
# )
# 
# #column for display is pval'd mu
# estsdf$mu.disp<-apply.pvals.class(
#   estsdf$mu,
#   estsdf$pval.class,
#   markers=c("***","**","*")
# )
# 
# #trim estsdf
# tmpcols<-c(
#   "dv",
#   "ivmod",
#   "mu.disp"
# )
# 
# #flip it
# require(tidyr)
# tmpdf<-spread(
#   estsdf[,tmpcols],
#   ivmod,
#   mu.disp
# )
# 
# #add ordering var
# tmpdf$order<-sapply(tmpdf$dv,getname,to="order")
# roworder<-order(
#   tmpdf$order
# )
# tmpdf<-tmpdf[roworder,]
# 
# setwd(outputdir)
# write.csv(
#   tmpdf,
#   "tab_robustness.csv",
#   row.names=F
# )
# 
# #output a version of this table w/o numbers
# #just w/ stars for significance
# tmpdf2<-tmpdf
# estvars<-names(tmpdf2)[!names(tmpdf2)%in%c("dv","order")]
# tmpdf2[,estvars]<-apply(tmpdf2[,estvars],2,function(x) {
#   x[x>0]<-paste0("+",x[x>0])
#   str_replace(x,"[0-9]([0-9])?\\.[0-9]{3}","")
# })  #%>% 
#   #str_replace("\\\\textsuperscript\\{","") %>%
#   #str_replace("\\}","") %>%
#   #str_replace("\\-$","")
# tmpdf[,estvars]
# 
# head(tmpdf2)
# setwd(outputdir)
# write.csv(
#   tmpdf2,
#   "tab_robustness2.csv",
#   row.names=F
# )
# 
# #########################################################
# #########################################################
# 
# #OUTPUT A TILE PLOT GRAPH
# 
# head(estsdf)
# plotdf<-estsdf
# 
# #before plotting, limit
# tmp<-plotdf$iv=="nv_regime_dur"
# plotdf<-plotdf[tmp,]
# 
# ##FIX DV
# #put in order
# tmp<-sapply(plotdf$dv,getorder)
# tmplevels<-names(sort(tmp,decreasing=T)) %>% 
#   unique
# tmplabels<-sapply(tmplevels,getname)
# #also, formatting
# #add spacing to labels
# tmptype<-sapply(
#   tmplevels,
#   getname,
#   to="maindv"
# )
# tmpspace<-rep("",length(tmptype))
# tmpspace[tmptype==1]<-"    "
# #make factor
# plotdf$dv.disp<-factor(
#   plotdf$dv,
#   levels=tmplevels,
#   labels=paste0(tmplabels,tmpspace)
# )
# #also, add fontface
# tmpface<-rep(NA,length(tmptype))
# tmpface[tmptype==1]<-"bold"
# tmpface[tmptype==2]<-"plain"
# 
# ##FIX MODS
# #order the mods
# tmp<-sapply(plotdf$mod,getmodorder)
# tmplevels<-names(sort(tmp,decreasing=F)) %>%
#   unique
# tmplabels<-paste0(
#   "(",LETTERS[1:length(tmplevels)],")"
# )
# 
# #here, for assistance, 
# #outptu which letters correspond 
# tmpoutput<-paste0(
#   tmplabels,
#   ": ",
#   tmplevels
# )
# setwd(metadir)
# write(
#   tmpoutput,
#   "robustness_deets.txt"
# )
# 
# #tmplabels<-sapply(tmplevels,getmodname)
# #remove CRSE from label
# # tmplabels<-str_replace(
# #   tmplabels,
# #   "\\s\\(CRSE\\)$",
# #   ""
# # )
# 
# plotdf$mod<-factor(
#   plotdf$mod,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #replace modnames w/ letters, 
# #and put details in the footnote
# # #mods<-names(tmptab)[-1]
# # #mods<-str_replace(mods,"[a-z\\_]+\\.","") %>%
# #   str_replace("\\_clSE","") %>% 
# #   unique
# # tmpdf<-data.frame(
# #   letter=LETTERS[1:length(mods)],
# #   mod=mods
# # )
# 
# ##FIX IVS
# tmp<-sapply(plotdf$iv,getorder)
# tmplevels<-names(sort(tmp)) %>%
#   unique
# tmplabels<-sapply(tmplevels,getname)
# plotdf$iv<-factor(
#   plotdf$iv,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #get pval fill, for tile
# plotdf$pval.fill<-NA
# plotdf$pval.fill[plotdf$pval.class=="at alpha=0.01"]<-4
# plotdf$pval.fill[plotdf$pval.class=="at alpha=0.05"]<-3
# plotdf$pval.fill[plotdf$pval.class=="at alpha=0.10"]<-2
# plotdf$pval.fill[plotdf$pval.class=="not sig"]<-1
# negmu<-ifelse(plotdf$mu<0,-1,1)
# plotdf$pval.fill<-plotdf$pval.fill * negmu
# pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
# tmplabels<-c(
#   paste0("- /",pval.labels),
#   paste0("+ /",rev(pval.labels))
# )
# table(plotdf$pval.fill)
# 
# #assign levels,colors
# plotdf$pval.fill<-factor(
#   plotdf$pval.fill,
#   levels=c(-4,-3,-2,-1,1,2,3,4),
#   labels=tmplabels
# )
# #for colors, consult brewer
# require(RColorBrewer)
# tmpcolors<-brewer.pal(8,"RdYlGn")
# brewer.pal.info
# 
# #make the middle two grayer
# # tmpcolors[4]<-""
# # tmpcolors[5]<-""
# names(tmpcolors)<-levels(plotdf$pval.fill)
# fill.labels<-c(
#   expression(paste(alpha==0.01,", ",beta<0)),
#   expression(paste(alpha==0.05,", ",beta<0)),
#   expression(paste(alpha==0.10,", ",beta<0)),
#   expression(paste(beta<0)),
#   expression(paste(beta>0)),
#   expression(paste(alpha==0.10,", ",beta>0)),
#   expression(paste(alpha==0.05,", ",beta>0)),
#   expression(paste(alpha==0.01,", ",beta>0))
# )
# #limit to those that are in the dataset
# tmp<-levels(plotdf$pval.fill)%in%plotdf$pval.fill
# tmpcolors<-tmpcolors[tmp]
# fill.labels<-fill.labels[tmp]
# 
# #make the graph
# g.tmp<-ggplot(
#   data=plotdf,
#   aes(
#     x=mod,
#     y=dv.disp,
#     fill=pval.fill
#   )
# ) +
#   geom_tile() +
#   scale_x_discrete(
#     position="bottom"
#   ) +
#   scale_fill_manual(
#     name="",
#     values=tmpcolors,
#     labels=fill.labels,
#     drop=F
#   ) +
#   xlab("") + ylab("") +
#   facet_wrap(~ iv) +
#   theme_bw(
#     base_family="CM Roman",
#     base_size=14
#   ) + 
#   theme(
#     axis.text.y=element_text(face=tmpface)
#   ) #+
# # theme(
# #   axis.text.x = element_text(
# #     angle = 70
# #   )
# # )
# g.tmp
# 
# gs.list[["g.allrobustness"]]<-list(
#   graph=g.tmp,
#   filename="fig_robustnessall.pdf",
#   width=12,
#   height=8
# )

#########################################################
#########################################################

#OUTPUT
#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  #i<-1
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height,
    limitsize=F
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