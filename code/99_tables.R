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
source('footnotes.R') #fn's
setwd(homedir)

#########################################################
#########################################################

#EXTRA HELPERS
#to clean table quickly
cleanup<-function(x,old="0",new="--") {
  x<-as.character(x)
  x[x==old]<-new
  return(x)
}

#where text goes from some to none
textchange<-function(x) {
  #x<-dd$dv
  y<-as.character(x)
  y[y!=""]<-"1"
  y[y==""]<-"0"
  y<-as.numeric(y)
  c(diff(y),1) #for ending
}

#remove repeating rows
remreps<-function(x) {
  #x<-tmptab$type  
  x2<-factor(x) %>%
    as.numeric
  tmp<-c(1,diff(x2))
  x[tmp==0]<-""
  x
}

#########################################################
#########################################################

#EXAMINE TABLES

#tables.csv lists all tables and their filenames
setwd(metadir); dir()
tabdf<-read.csv(
  "tables.csv",
  stringsAsFactors=F
)
tabdf

#drop any tables that have NA order
tabdf<-tabdf[!is.na(tabdf$order),]

getinfo<-function(x,from="label",to="caption") {
  tabdf[[to]][tabdf[[from]]==x]
}
getinfo("tab_classify")

getnum<-function(x,from="label",to="divider") {
  #x<-"tab_sample"; from="label"; to="right"
  y<-tabdf[[to]][tabdf[[from]]==x]
  y<-eval(parse(text=y)) %>%
    as.numeric
  if(length(y)==0) {
    y<-NULL
  }
  return(y)
}
getnum("tab_sample",to="right")

#wrappers for xtable, 
#given table and label, 
#standard things to do for each
get.xtable<-function(table,label) {
  #table<-tmptab; label<-"tab_sample"
  caption<-getinfo(label)
  #table preview
  #xtable(table,caption=caption,label=label)
  #fix alignment, add divider (add 1 b/c rownames)
  align<-rep("c",ncol(table) + 1 )
  align[getnum(label,to="right") + 1]<-"r"
  align[getnum(label,to="left") + 1]<-"l"
  newind<-getnum(label,to="divider") + 1 
  tmp<-c(align,rep("|",length(newind)))
  id<-c(seq_along(align),newind+0.5)
  align<-tmp[order(id)]
  if( length(align[align!="|"]) != ncol(table)+1 ) 
    stop(ncol(table)+1 - length(align[align!="|"]))
  #return xtable object
  xtable(
    table,
    caption=caption,
    label=label,
    align=align
  )
}

addsize<-function(thelabel,thefilename,tabenv='tabular') {
  #thelabel<-"tab_descriptive"; thefilename<-"tables_full.tex"; tabenv<-'tabular'
  output<-readLines(thefilename)
  thisline<-str_detect(output,"begin\\{(long)?table\\}") %>%
    which %>%
    max #last time that we put a table in
  size<-getinfo(thelabel,to="size")
  #if this is longtable, needs adjusting
  if(tabenv=='tabular') {
    write(
      c(
        output[1:thisline],
        paste0("\\",size),
        output[(thisline+1):length(output)]
      ),
      file=thefilename
    )
  } else if(tabenv=='longtable') {
    write(
      c(
        output[1:(thisline-1)],
        paste0("\\",size),
        output[thisline:length(output)]
      ),
      file=thefilename
    )
  } else {
    stop("Not implemented")
  }
}

addnote<-function(thelabel,thefilename) {
  note<-c(paste0("\\floatfoot{",fns[[thelabel]],"}"))
  output<-readLines(thefilename)
  write(
    c(
      output[1:(length(output)-1)],
      note,
      output[length(output)]
    ),
    file=thefilename
  )
}

#########################################################
#########################################################

#FINALIZE ALL TABLES

#all will be stored in
#with attrs, and main/app classifier
setwd(outputdir)
tablist<-list()

#########################################################
#########################################################

#DESCRIPTIVE
tmptab<-read.csv(
  "tab_descriptive.csv",
  stringsAsFactors=F
)

#exclude the extra dvs
tmp<-varsdf$display==T | is.na(varsdf$display)
tmp<-tmptab$varname%in%varsdf$pretty[tmp]
tmptab<-tmptab[tmp,]

#don't separate independent from controls
tmptab$type[tmptab$type=="control"]<-"iv"

#add dividing lines of text
#tmptab$type[tmptab$type=="control"]<-"iv"
tmp<-remreps(tmptab$type) %>%
  textchange
tmp.pos<-which(tmp==1)
tmp.pos<-c(0,tmp.pos[1:length(tmp.pos)-1])
tmp.commands<-paste0(
  "\n\n \\multicolumn{4}{l}{} \n \\\\ 
  \\multicolumn{4}{l}{\\textbf{",
  c("Dependent Variables","Independent Variables"),
  "}} \\\\ 
  \n \\multicolumn{4}{l}{} \\\\ \n\n"
)
tmp.commands[1]<-paste0("\\midrule",tmp.commands[1],"%") #add/rem midrule
tmpclist<-list(
  pos=as.list(tmp.pos),
  command=tmp.commands
)

#rem type
tmptab$type<-NULL
names(tmptab)<-
  c("","Average","SD","Within SD")

#add options to list entry
tmplist<-list(
  #the table
  tmptab=tmptab,
  #commandlist
  commandlist=tmpclist,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='tabular',
  floating=T
)

#add to tablist
tablist[['tab_descriptive']]<-tmplist

#########################################################

#SAMPLE

setwd(outputdir)
dir()[str_detect(dir(),"tab\\_")]
tmptab<-read.csv(
  "tab_sample.csv",
  stringsAsFactors=F
)

#clean up
tmptab$countryname<-
  remreps(tmptab$countryname)
tmptab$obs<-round(tmptab$obs) %>% as.integer

#colnames
names(tmptab)<-
  c("","Years When Democracy","\\% Years in Sample","Unarmed Mob.","Armed Mob.")

#add a commandlist

#add options to list entry
tmplist<-list(
  #the table
  tmptab=tmptab,
  #commandlist
  commandlist=NULL,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='longtable',
  floating=F
)

#add to tablist
tablist[['tab_sample']]<-tmplist

#########################################################

#REGTABLE,SR/LR
tmptab<-read.csv(
  "tab_regtable.csv",
  stringsAsFactors=F
)

names(tmptab)<-c(
  "","Deliberative","Participatory","Egalitarian"
)

#split into shortrun and longrun
splitter<-str_detect(tmptab[,1],"Long-Run Multiplier") %>%
  which
srtable<-1:(splitter-1)
lrtable<-(splitter-1):nrow(tmptab) #get extra space
tab_regtabsr<-tmptab[srtable,]
tab_regtablr<-tmptab[lrtable,]

#add to list
tmplist<-list(
  #the table
  tmptab=tab_regtabsr,
  #commandlist
  commandlist=NULL,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='tabular',
  floating=T
)

#add to tablist
tablist[['tab_regtabsr']]<-tmplist

tmplist<-list(
  #the table
  tmptab=tab_regtabsr,
  #commandlist
  commandlist=NULL,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='tabular',
  floating=T
)
tablist[['tab_regtabsr']]<-tmplist

tmplist<-list(
  #the table
  tmptab=tab_regtablr,
  #commandlist
  commandlist=NULL,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='tabular',
  floating=T
)
tablist[['tab_regtablr']]<-tmplist

#########################################################

#ROBMODS
setwd(outputdir); dir()
tmptab<-read.csv(
  'tab_robmods.csv',
  stringsAsFactors=F
)

names(tmptab)<-c("","Model Detail")

#add to list
tmplist<-list(
  #the table
  tmptab=tmptab,
  #commandlist
  commandlist=NULL,
  #options for print xtable
  #these change
  include.colnames=T,
  tabular.environment='tabular',
  floating=T,
  floating.environment='table'
)
tablist[['tab_robmods']]<-tmplist


#########################################################

#ROBUSTNESS
# tmptab<-read.csv(
#   "tab_robustness2.csv",
#   stringsAsFactors=F
# )
# 
# #make DV neat
# tmptab$dv<-sapply(tmptab$dv,getname,to="short")
# tmptab<-tmptab[order(tmptab$order),]
# tmptab$order<-NULL
# 
# #this will give me list of letters/models
# mods<-names(tmptab)[-1]
# mods<-str_replace(mods,"[a-z\\_]+\\.","") %>%
#   str_replace("\\_clSE","") %>%
#   unique
# 
# #these are new column headings
# tmpletters<-paste0(
#   "(",LETTERS[1:(ncol(tmptab)-1)],")"
# )
# names(tmptab)<-c("Dep. Var.",tmpletters)
# 
# #insert
# tmplist<-list(
#   #the table
#   tmptab=tmptab,
#   #commandlist
#   commandlist=NULL,
#   #options for print xtable
#   #these change
#   include.colnames=T,
#   tabular.environment='tabular',
#   floating=T,
#   floating.environment='table'
# )
# tablist[['tab_robustness']]<-tmplist

#DEPRECATED; NOT SHOWING VIOLENCE RESULT, HERE
# #split this into two tables, based on above
# #one does not fit on single page
# div<-which(tmpdf$mod=="tgame") * 2
# tmptab1<-tmptab[,c(1,2:(div-1))]
# tmptab2<-tmptab[,c(1,div:ncol(tmptab))]
# 
# #tmptab1
# modcols<-str_detect(names(tmptab1),"v_regime")
# tmpletters<-LETTERS[1:(length(modcols)/2)]
# header<-paste0(
#   "\\multicolumn{1}{c}{(",tmpletters,")}",
#   collapse=" & "
# ) %>%
#   paste0(" \\\\ \n\n")
# header<-paste0("\\toprule \\multicolumn{1}{c}{} & ",header) #for first col
# #add second header
# tmp<-c("$\\beta_{N}$","$\\beta_{V}$")
# tmp<-rep(tmp,(length(modcols)/2))
# tmp<-c("",tmp) %>% paste0(collapse=" & ")
# tmp<-paste0(tmp,"\\\\ %")
# header<-paste0(header,tmp,collapse="\n\n")
# #for second col
# tmpclist<-list(
#   pos=c(-1) %>%
#     as.list,
#   command=header
# )
# tmplist<-list(
#   #the table
#   tmptab=tmptab1,
#   #commandlist
#   commandlist=tmpclist,
#   #options for print xtable
#   #these change
#   include.colnames=F,
#   tabular.environment='tabular',
#   floating=T
# )
# tablist[['tab_robustness1']]<-tmplist
# 
# ############
# 
# #tmptab2
# modcols<-str_detect(names(tmptab2),"v_regime")
# #w/ letters adjustment
# tmp<-LETTERS[!LETTERS%in%tmpletters]
# tmp<-tmp[1:(length(modcols)/2)]
# header<-paste0("\\multicolumn{2}{c}{(",tmp,")}",collapse=" & ") %>%
#   paste0(" \\\\ \n\n")
# header<-paste0("\\toprule \\multicolumn{1}{c}{} & ",header) #for first col
# #add second header
# tmp<-c("$\\beta_{N}$","$\\beta_{V}$")
# tmp<-rep(tmp,(length(modcols)/2))
# tmp<-c("",tmp) %>% paste0(collapse=" & ")
# tmp<-paste0(tmp,"\\\\ %")
# header<-paste0(header,tmp,collapse="\n\n")
# #for second col
# tmpclist<-list(
#   pos=c(-1) %>%
#     as.list,
#   command=header
# )
# tmplist<-list(
#   #the table
#   tmptab=tmptab2,
#   #commandlist
#   commandlist=tmpclist,
#   #options for print xtable
#   #these change
#   include.colnames=F,
#   tabular.environment='tabular',
#   floating=T
# )
# tablist[['tab_robustness2']]<-tmplist

#########################################################
#########################################################

#OUTPUT TEX FILES

require(xtable)

setwd(metadir); dir()
myheader<-readLines("header.txt") %>%
  paste0(collapse=" \n ")

#make sure all tables in tablist
#are in tables.csv, and vice-versa
tmp<-tabdf$label%in%names(tablist)
if(sum(!tmp)>0) {
  print(tabdf$label[!tmp])
  stop('missing table here')
}
tmp<-names(tablist)%in%tabdf$label
if(sum(!tmp)>0) {
  print(names(tablist)[!tmp])
  stop('missing table in meta')
}

#loop through and generate all four
setwd(outputdir)
tmpdf<-expand.grid(
  loc=c("main","app"),
  type=c("bit","full"),
  stringsAsFactors=F
)

#loop
tmpseq<-1:nrow(tmpdf) 
for(i in tmpseq) {
  #i<-1
  print(i)
  #get params
  thisrow<-tmpdf[i,]
  thisloc<-thisrow$loc
  thistype<-thisrow$type
  isfull<-thistype=="full"
  #get filename
  filename<-paste0(
    thisloc,
    "tables",
    ifelse(isfull,"_full",""),
    ".tex"
  )
  #write header
  opening<-ifelse(isfull,myheader,"")
  write(opening,file=filename)
  #identify the tables in question
  tmp<-sapply(names(tablist),function(tabname) {
    getinfo(tabname,to="loc")==thisloc
  })
  tmplist<-tablist[tmp]
  #put them in order, if something to do
  if(length(tmplist)>0) {
    tmp<-sapply(names(tmplist),function(tabname) {
      getinfo(tabname,to="order")
    }) %>% sort %>% names
    tmplist<-tmplist[tmp]
  } 
  #loop through
  tab.sequence<-seq_along(tmplist)
  for(i in tab.sequence) {
    #i<-2
    tmpel<-tmplist[[i]]    
    thistab<-names(tmplist)[i]
    #get temptable
    temptable<-get.xtable(tmpel$tmptab,thistab)
    #if longtable
    if(tmpel$tabular.environment=="longtable") {
      write(
        "\\clearpage",
        file=filename,
        append=T
      ) 
    }
    #print table
    print(
      temptable,
      file=filename,
      ###optional commands
      add.to.row=tmpel$commandlist,
      tabular.environment=tmpel$tabular.environment,
      include.colnames=tmpel$include.colnames,
      floating=tmpel$floating,
      ##preset commands, same for all
      append=T,
      caption.placement="top",
      booktabs=T,
      include.rownames=F,
      sanitize.text.function=identity
    )
    #add size
    addsize(
      thistab,
      filename,
      tabenv = tmpel$tabular.environment
    )
    if(tmpel$tabular.environment=="tabular") {
      addnote(thistab,filename) #not for longtable
    }
    #add spacing
    write("\n \n \n",file=filename,append=T)
  }
  ############
  #CLOSING
  closing<-ifelse(isfull,"\\end{document}","")
  write(closing,file=filename,append=T)
  
}

#########################################################
#########################################################

#EDIT
setwd(metadir); dir()
extrabit<-readLines('extratable.txt')

#we want to add command to input extra table
#into mainntables.tex, both full and not
files<-c(
  "maintables.tex",
  "maintables_full.tex"
  )
for(thisfile in files) {
  #thisfile<-"maintables.tex"
  setwd(outputdir); dir()
  output<-readLines(
    thisfile
  )
  #input before sample, which is first table
  tmpnum<-which(str_detect(output,"% latex table generated"))[1]
  
  write(
    c(
      output[1:tmpnum],
      extrabit,
      output[(tmpnum+1):length(output)]
    ),
    file=thisfile
  )
}
