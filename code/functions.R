#stared with this directory
olddir<-getwd()

#########################################################

#helper functions
normalize<-function(x,na.rm=T)
  return((x-min(x,na.rm=na.rm))/(max(x,na.rm=na.rm)-min(x,na.rm=na.rm)))
diff.apply<-function(var,index,n) #t+1 - t
  return(unlist(tapply(var,index,function(x) c(diff(x,n),rep(NA,n)))))
diff.apply2<-function(var,index,n) #t - t-1
  return(unlist(tapply(var,index,function(x) c(rep(NA,n),diff(x,n)))))
lag.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) c(dplyr::lag(x,n))))) #lag auto generates NA's
lead.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) c(dplyr::lead(x,n))))) #lead auto generates NA's
unskew<-function(x) {
  sign<-ifelse(x<0,-1,1)
  sqrt(abs(x))*sign
}

#trivial functions
pct.change<-function(x,y) 100*(y-x)/x

#########################################################

#a function to get good names for the variables in this

#get pretty and short names from the varnames
setwd(metadir)
varsdf<-read.csv(
  "varnames.csv",
  stringsAsFactors=F
)

require(stringr)
getname<-function(thisvar,
                  from="oldname",
                  to="pretty") {
  #thisvar<-"L.beopct_all"; from="oldname"; to="pretty"
  thisrow<-varsdf[[from]]==thisvar
  if(sum(thisrow)==0) {
    if(str_detect(thisvar,"^L([0-9]?)\\.")) {
      oldnamevar<-str_replace(thisvar,"^L([0-9]?)\\.","")
      newvar<-getname(oldnamevar,from=from,to=to) #call function again
    } else if(str_detect(thisvar,"^D\\.")) {
      oldnamevar<-str_replace(thisvar,"^D\\.","")
      newvar<-getname(oldnamevar,from=from,to=to) #call function again
    } else {
      newvar<-NA
    } 
  } else {
    newvar<-varsdf[thisrow,to]
  }
  return(newvar)
}

getname("union.ratio")
getname("L.union.ratio")

#is it a dummy var? 
getdummy<-function(varname,from="oldname") {
  thisrow<-varsdf[[from]]==varname
  if(sum(thisrow)==0) {
    if(str_detect(varname,"^L([0-9]?)\\.")) {
      oldnamevar<-str_replace(varname,"^L([0-9]?)\\.","")
      tmp<-getdummy(oldnamevar,from=from) #call function again
    } else if(str_detect(varname,"^D\\.")) {
      oldnamevar<-str_replace(varname,"^D\\.","")
      tmp<-getdummy(oldnamevar,from=from) #call function again
    } else {
      tmp<-NA
    } 
  } else {
    tmp<-varsdf$dummy[thisrow]
  }
  ifelse(tmp==1,T,F)
}
getdummy("L.gdpcapl")

#tells us how many lags are in this var
getlag<-function(varname) {
  #varname<-"L.union.ratio"
  laginfo<-str_replace(varname,"^(L([0-9]+)?)\\..*$","\\1")
  if(laginfo=="L") {
    y<-1
  } else if (str_detect(laginfo,"L")) {
    y<-str_replace(laginfo,"L","") %>%
      as.numeric
  } else {
    y<-NA
  }
  return(y)
}
getlag("L2.union.ratio")

#tells us how many diffs are in this var
getdiff<-function(varname) {
  #varname<-"L.D2.union.ratio"
  laginfo<-str_replace(varname,"^.*?(D([0-9]+)?)\\..*$","\\1")
  if(laginfo=="D") {
    y<-1
  } else if (str_detect(laginfo,"D")) {
    y<-str_replace(laginfo,"D","") %>%
      as.numeric
  } else {
    y<-NA
  }
  return(y)
}
getdiff("D.union.ratio")

#and cfoldnames
cfnames<-read.csv(
  "cfactualnames.csv",
  stringsAsFactors=F
)

getname.cf<-function(
  oldname,
  from="oldname",
  to="pretty"
) {
  thisrow<-cfnames[[from]]==oldname
  if(sum(thisrow)==0) {
    y<-NA
  } else {
    y<-cfnames[[to]][thisrow]
  }
  return(y)
}

getname.cf("observed")

getorder.cf<-function(oldname,from="oldname",to="order") {
  getname.cf(oldname,from,to)
}
getorder.cf("observed")

#########################################################

#uses varhelper to get order in a list of these vars

getorder<-function(thisvar,from="oldname") {
  #from<-"oldname"
  #thisvar<-"highcapratio"
  thisrow<-varsdf[[from]]==thisvar
  if(sum(thisrow)==0) {
    y<-NA 
  } else {
    y<-varsdf$order[thisrow]
  }
  return(y)
}

#########################################################

#adds a lag (or series of lags) to a variable
addlag<-function(varname,lagnumber) {
  #varname<-"highcapratio"
  #lagnumber<-1
  if(lagnumber>1) {
    newterm<-paste0("L",lagnumber,".",varname)
  } else if(lagnumber==1) {
    newterm<-paste0("L.",varname)
  } else if(lagnumber==0) {
    newterm<-varname
  }
}

addlag("highcapratio",1)

addlags<-function(varname,lagnumbers) {
  #varname<-"highcapratio"
  #lagnumbers<-c(1:4)
  sapply(lagnumbers,function(i) addlag(varname,i))
}

addlags("highcapratio",1:4)

addlags.term<-function(varname,lagnumbers) {
  paste0(addlags(varname,lagnumbers),collapse=" + ")
}
addlags.term("highcapratio",1:2)

#########################################################

#adds a diff to a variable, which could itself be diff
add.diff<-function(varname) {
  #varname<-"D.highcapratio"
  #we have levels, first D, and 2 D
  tmp<-str_count(varname,"D\\.")
  diffterm<-paste0("D.",varname)
  #adjust diffterm, if
  if(tmp==1) {
    diffterm<-str_replace_all(diffterm,"D\\.","")
    diffterm<-paste0("D2.",diffterm)
  } else if(tmp==2) {
    diffterm<-str_replace_all(diffterm,"D\\.","")
    diffterm<-paste0("D3.",diffterm)
  } else if(tmp>2) {
    stop("Not implemented.")
  }
  return(diffterm)
}

add.diff("highcapratio")
add.diff("D.highcapratio")
add.diff("D.D.highcapratio")

#########################################################

#takes a IV at a given lag,
#creates the appropriate ECM term

createECM<-function(varname,atlag) {
  #varname<-"D.highcapratio"
  #atlag<-2
  baseterm<-addlag(varname,atlag)
  #make the diffterm
  diffterm<-add.diff(baseterm)
  #reverse L and D order
  diffterm<-str_replace(diffterm,"(D([0-9]+)?\\.)(L([0-9]+)?\\.)","\\3\\1")
  #make the lagterm
  lagterm<-addlag(varname,atlag+1)
  #make the fullterm
  fullterm<-paste0(diffterm," + ",lagterm)
  return(fullterm)
}

createECMs<-function(varname,lags) {
  #varname<-"highcapratio"
  #lags<-1:3
  sapply(lags,function(l) createECM(varname,l)) %>%
    paste0(collapse=" + ")
}

#########################################################

#a function to add Latex stars to estimates, based on pval
#obviously, will return a string and not a number
apply.pvals<-function(ests,pvals,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals<0.1 & pvals>=0.05]<-
    paste0(
      ests[pvals<0.1 & pvals>=0.05],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals<0.05 & pvals>=0.01]<-
    paste0(
      ests[pvals<0.05 & pvals>=0.01],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals<0.01]<-
    paste0(
      ests[pvals<0.01],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#test
apply.pvals(ests=rnorm(30),pvals=rep(c(0.11,0.06,0.01),10))
apply.pvals(ests=sprintf("%.2f",rnorm(30)),pvals=rep(c(0.11,0.06,0.01),10))

#this function uses class rather than numeric pval
apply.pvals.class<-function(ests,pvals.class,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals.class=="at alpha=0.10"]<-
    paste0(
      ests[pvals.class=="at alpha=0.10"],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals.class=="at alpha=0.05"]<-
    paste0(
      ests[pvals.class=="at alpha=0.05"],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals.class=="at alpha=0.01"]<-
    paste0(
      ests[pvals.class=="at alpha=0.01"],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#this function takes an est, pval, and se, 
#and gives something for a regtable
gimmie.est<-function(mu,pval,se,nrow=4) {
  tmp1<-apply.pvals(mu,pval)
  tmp2<-paste0("(",sprintf("%.3f",se),")")
  matrix(c(tmp1,tmp2),nrow=nrow)
}

#this function takes an est, a pval class and CI
#and gives something for a regtable
#to be used with simulated long run multipliers
gimmie.est2<-function(mu,pval.class=NULL,mu.min,mu.max,nrow=4) {
  # mu<-estdf$mu
  # pval.class<-estdf$pval.class
  # mu.min<-estdf$mu.min
  # mu.max<-estdf$mu.max
  # nrow<-2
  if(length(mu)==0) {
    tmp1<-tmp2<-""
  } else {
    if(!is.null(pval.class)) {
      tmp1<-apply.pvals.class(mu,pval.class)
    } else {
      tmp1<-format(round(mu,2),2)
    }
    tmp2<-paste0(
      "[",
      format(round(mu.min,2),2),
      " to ",
      format(round(mu.max,2),2),
      "]"
    )
  }
  matrix(
    c(tmp1,tmp2),
    nrow=nrow
  )
}

#########################################################

#this is a helper function
#calculates the BIC for PLM objects
#and for LM objects, since my function
#doesn't seem to match the built-in function

calcfits<-function(m) {
  #m<-m.test
  #m<-m
  #m<-mods$v2x_polyarchy.lagdv.0.0.controls.demsonly.re_lme4.no
  
  #what kind of model?
  plm.fit<-class(m)[1]=="plm"
  lmer.fit<-class(m)[1]=="lmerMod"
  #m<-m.cube
  
  #this routine undefined for lmer, fornow
  if(lmer.fit) {
    thisrow<-data.frame(
      r2=NA,
      stringsAsFactors=F
    )
  } else {
    #get residual sum of squares
    res<-m$residuals
    rss<-sum(res^2)
    #get n
    n<-nobs(m)
    #get k
    if(plm.fit) {
      k<-n-m$df.residual
    } else {
      k<-m$rank
    }
    #the stats
    df<-n-k #degrees of freedom
    w<-rep(1,n) #weights
    #log likelihood
    ll<-0.5 * (sum(log(w)) - n * 
                 (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
    df.ll<-k+1
    bic<- -2 * ll + log(n) * df.ll
    aic<- -2 * ll + 2 * df.ll 
    #bic<- n*log(rss/n)+k*log(n) #old way
    #aic<-2*k+n*log(rss) #old way
    #get summary object
    summary.m<-tryCatch(summary(m),error=function(e) "error")
    #temp fix for the plm problem
    if(summary.m[1]=="error") {
      print("summary error")
      r2<-adjr2<-NA
    } else {
      if(plm.fit) {
        r2<-summary.m$r.squared['rsq']
        adjr2<-summary.m$r.squared['adjrsq']
      } else {
        r2<-summary.m$r.squared
        adjr2<-summary.m$adj.r.squared
      }
    }
    #return
    thisrow<-data.frame(
      bic,
      aic,
      r2,
      adjr2,
      stringsAsFactors=F
    )
  }
  return(thisrow)  
}

#test
y<-rnorm(100)
x<-rnorm(100)
m.test<-lm(y ~ x)
BIC(m.test)
calcfits(m.test)

# 
# y<-rnorm(100)
# x<-rnorm(100)
# m.test<-lm(y ~ x) 
# 
# n<-100
# res<-m.test$residuals
# rss<-sum(res^2) 
# k<-3; df<-n-k; w<-rep(1,N) #params, dfs, weights
# ll<-0.5 * (sum(log(w)) - n *
#              (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
# ll.stats<-logLik(m.test)
# abs(ll.stats-ll)==0 #same, prob is not here
# 
# bic.mine<-n*log(rss/n)+k*log(n) #formula from wikipedia
# bic.exact<- -2 * ll + log(n) * df #suggestions from comments
# bic.stats<-BIC(m.test) #using stats package
# abs(bic.mine-bic.stats) #mine was off
# abs(bic.exact-bic.stats) #this is still off, though
# 
# 
# 

#########################################################

#rankings, within struggledf
#returns a rank, given a vector of fit sstatistics
require(plyr)
require(dplyr)
rankme<-function(x,stat=c("aic","bic","r2","adjr2")) {
  if(stat%in%c("aic","bic")) {
    y<-rank(x)
    ranker<-y %>%
      as.factor %>% 
      as.numeric
  } else {
    y<- -1 * rank(x) 
    ranker<-y %>%
      as.factor %>%
      as.numeric
  }
  return(ranker)
}

rankme(c(0.2,0.3),stat="r2")
rankme(c(100,200),stat="bic")



#########################################################

#for fe models, 
#re-estimate as lm() models
#assumes time is already specified as factor
convert.plm<-function(m.old,
                      unit="cowcode.num",
                      time="year") {
  #m.old<-mods$union.0.1.1.fe.10.all
  #init
  unitfactor<-paste0("factor(",unit,")")
  timefactor<-paste0("factor(",time,")")
  #add unit to form, re-estimate
  oldform<-deparse(m.old$formula)
  newform<-str_replace(oldform,"~\\s",paste0("~ ",unitfactor," + ")) %>%
    as.formula
  #use the same data
  tmpdf<-data.frame(attr(m.old$model,"index"))
  names(tmpdf)<-c(unit,time)
  tmpdf<-cbind(tmpdf,m.old$model)
  #estimate
  m.new<-lm(data=tmpdf,
            formula=newform)
  return(m.new)
}

#########################################################

require(Bolstad2)
getarea<-function(values,years=NA) {
  #years<-pred.year
  #values<-polityhat.normal
  if(is.na(years[1]))
    years<-1:length(values)
  if(max(diff(years))==1) { #if no missing years, no biggie
    return(sintegral(years,values)$int)
  } else { #if there are some missing years, need to adjust
    #need to split this up into running intervals
    #grab each instance of missing year, and the last year
    cutpoints<-which(c(diff(years)>1,NA) | years==max(years))
    cuts<-lapply(seq_along(cutpoints),function(i) {
      if(i==1) {
        return(1:cutpoints[i])
      } else {
        return((cutpoints[i-1]+1):cutpoints[i])
      }
    })
    #calculate area of each cut, and return
    area<-sapply(cuts,function(indices) {
      sintegral(years[indices],values[indices])$int
    }) %>% sum
    return(area)
  }
}

#########################################################

getgain<-function(x,y) { #wrt y
  if(length(x)!=length(y)) {
    warning("Different Lengths")
    avgx<-avgy<-pctgain.avg<-
      areax<-areay<-pctgain.area<-NA
  } else {
    #pct gain by average level
    avgx<-mean(x)
    avgy<-mean(y)
    pctgain.avg<-100*(avgx-avgy)/avgy
    #pct gain by area covered
    areax<-getarea(values=x)
    areay<-getarea(values=y)
    pctgain.area<-100*(areax-areay)/areay
  }
  return(list(avg1=avgx,avg2=avgy,
              pctgain.avg=pctgain.avg,
              area1=areax,area2=areay,
              pctgain.area=pctgain.area))
}

#########################################################

#takes a model and returns the robust vcov matrix
getrobust<-function(m,cluster) {
  
  #h/t: http://www.r-bloggers.com/easy-clustered-standard-errors-in-r/
  
  require(sandwich,quietly=T)
  require(lmtest,quietly=T)
  
  # m<-lm(data=beodf[fullsamples$oneyrdf$incrate,],
  #       formula = incrt_t_jur ~ L.beopct_all)
  # cluster<-beodf$state_alpha2[fullsamples$oneyrdf$incrate]
  
  M<-length(unique(cluster))
  N<-length(cluster)
  K<-m$rank
  
  #adjust df
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate uj's
  uj<-apply(estfun(m),2,function(x) tapply(x,cluster,sum))
  
  #use sandwich to get newvcov
  newvcov<-dfc * sandwich(m,meat=crossprod(uj)/N)
  return(newvcov)
  
}

getrobust.plm<-function(m.plm) {
  
  #m.plm<-allmodels[[1]]
  #cluster<-attr(m.plm$model,"index")[["state_alpha2"]]
  newvcov<-tryCatch({
    vcovHC(m.plm,
           type="HC0",
           cluster="group")
  }, error=function(e) {
    print("Couldn't compute clustered SE's")
    print(e)
    return(m.plm$vcov)
  })
  
  return(newvcov)
}


#########################################################

#will use bootstrap to return 
#an outlier-robust standard error
require(boot)

f.sd<-function(x,i) {
  y<-x[i]
  return(sd(y))
}

#DEPRECATED
#this relies on 'fake' p-val test, 
#better to just report CI's for long-run estimate
# summarize.distribution<-function(ests.distribution,
#                                  sequant=T) {
#   #get quantiles
#   quantiles<-quantile(ests.distribution,
#                       c(0.025,0.5,0.975))
#   mu<-quantiles[2]
#   mu.min<-quantiles[1]
#   mu.max<-quantiles[3]
#   #se
#   #est of se explodes when lagdv coef is over 1
#   #so need something that is robust to that scenario
#   se<-boot(ests.distribution,f.sd,R=500)$t %>% 
#     mean 
#   se.quantile <- ( quantiles[3] - quantiles[1] ) / 4
#   if(sequant) {
#     se<-se.quantile
#   }
#   #get something like a two-sided pval test
#   pval<-ecdf(ests.distribution)(0) 
#   pval<-ifelse(mu<0,(1-pval)*2,pval*2)
#   #return me
#   thisrow<-data.frame(mu,
#                       mu.min,
#                       mu.max,
#                       se=se,
#                       pval=pval)
#   return(thisrow)
# }

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  # #se
  # #est of se explodes when lagdv coef is over 1
  # #so need something that is robust to that scenario
  # tmpboot<-boot(
  #   ests.distribution,
  #   f.sd,
  #   R=500
  # )
  # se<-mean(tmpboot$t)
  # se.q <- ( quantiles[3] - quantiles[1] ) / 4
  #SE is less rather than more helpful
  se<-NA 
  #se.q<-NA
  #get something like a two-sided pval test
  #pval<-ecdf(ests.distribution)(0)
  #pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

#########################################################


require(MASS)

#this function takes a model,
#the name of the coefficien of the lagged DV
#the name of the coefficient of the lagged IV, 
#optionally, can calc LRM for more than one lagged IV 
#and simulates mvnorm distribution to evaluate SE of LRM
getlongrun<-function(m,lagdv,iv, 
                     reps=5000,
                     plm=T,
                     summary=T,
                     dvfactor=1,
                     ivfactor=1) {
  
  #test input
  #m=m;lagdv=lagdvs;iv=ivs;reps=5000;dvfactor=1;ivfactor=1
  
  #return NA if iv or ivs not found
  if(sum(sapply(iv,function(x) !x%in%names(m$coefficients)))>0)
    return(NA)
  #otherwise:
  means<-c(m$coefficients[lagdv],
           m$coefficients[iv]) 
  #you want the variance-covariance matrix that is adjusted for clustering
  new.vcov<-getrobust(m)
  rows<-row.names(new.vcov)%in%c(lagdv,iv)
  cols<-colnames(new.vcov)%in%c(lagdv,iv)
  vcov.useme<-new.vcov[rows,cols] #just these vars
  #vcov needs to be ordered in the same way as the means
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  #sample from the multivariate distribution defined here
  draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  numerator<-apply(draws[,iv] %>% as.matrix,1,sum) #if more than one lag of iv
  denominator<-1-apply(draws[,lagdv] %>% as.matrix,1,sum) #if more than one lag dv
  lrm.distribution<-numerator/denominator
  #put this distribution in meaningful units
  lrm.distribution<-lrm.distribution*ivfactor*dvfactor
  #now compute summary stats for return
  #hist(lrm.distribution)
  estimate<-quantile(lrm.distribution,c(0.5)) #median
  ci<-quantile(lrm.distribution,c(0.025,0.975)) #2.5th, 97.5th percentiles
  pval<-ecdf(lrm.distribution)(0) 
  #adjust pval to be something like a two-sided test
  pval<-ifelse(estimate<0,(1-pval)*2,pval*2)
  estimate.star<-apply.pvals(estimate,pval)
  if(summary) {
    return(list(est=estimate,
                est.output=estimate.star,
                pval=pval,
                ci=c(ci[1],ci[2]),
                se=sd(lrm.distribution),
                se.output=paste0("(",sprintf("%.3f",sd(lrm.distribution)),")")))
  } else {
    return(lrm.distribution)
  }
}

#########################################################

#two functions for summary stats

weighted.median<-function(data,weights) {
  #data=avgs;weights=obs
  new.data<-sapply(1:length(data),function(i) {
    rep(data[i],weights[i])
  }) %>% unlist 
  median(new.data)
}

getbootstrappedSE<-function(data,reps,
                            stat.function,
                            weights=rep(1,length(data))) {
  #data<-avgs; reps<-1000; weights<-obs
  #statistic<-weighted.mean
  #put data in a matrix with its weights
  data.matrix<-matrix(c(data,weights),ncol=2)
  #these are sample rows 
  resample.rows<-sapply(1:reps,function(i) 
    sample(1:nrow(data.matrix),replace=T))
  #retrieve row and apply the stat
  resamples.stat<-apply(resample.rows,2,function(x) {
    #x<-resample.rows[,1] %>% as.matrix
    x<-as.vector(x)
    d<-data.matrix[x,1] #get data from corresponding rows
    w<-data.matrix[x,2] #get weights from corresponding rows
    return(stat.function(d,w)) #weighted stat for this resample
  })
  se.stat<-sqrt(var(resamples.stat))
  return(se.stat)
}

#########################################################

#function to retrieve advanced/not, makes life easy
getadvanced<-function(cow) {
  return(unique(capdf$advanced[capdf$cowcode.num==cow]))
}

##########################################################
##########################################################

# require(plm)
# require(fUnitRoots)
# require(urca)
# require(metap)
# 
# fishertype.purtest<-function(splitresiduals,lags=3) {
#   #test
#   #splitresiduals<-splitxs
#   #ADF tests on each panel's residuals
#   pvals<-sapply(splitresiduals,function(x) {
#     pval<-tryCatch({
#       #x<-splitresiduals[[2]]
#       #I estimated an ADF regression with constant but no trend
#       #i skip the block F Test b/c I couldn't figure out how to 
#       #get p-vals for the F-statistic, in the event that I rejected it
#       #so I rely directly on the tau2 stat. this is not best practice, 
#       #but it will have to do for now.
#       output<-urdfTest(x,lags=lags,type=c("c"),doplot=F)
#       #get tau2
#       tau2<-output@test$test@teststat[,'tau2']
#       #get pval of tau1 under MAckinnon distribution
#       pval<-punitroot(tau2,N=length(x),trend="c",statistic="t") %>% as.vector
#     }, error = function(e) {
#       NA
#     })
#     return(pval)
#   })
#   pvals<-pvals[!is.na(pvals)]
#   #using package 'metap' to evaluate all these together, 
#   #using Fisher's proposed method
#   fisher<-sumlog(pvals)
#   #return results
#   return(list(logittest=fisher$p,
#               allpvals=pvals))
# }
# 
# #test with some simulated data
# df<-expand.grid(units=letters,t=1:50,x=1)
# df<-df[order(df$units,df$t,df$x),]
# gamma<-0.8
# #make each series unit root with some randomness
# df$x<-tapply(df$x,df$units,function(x) {
#   x[1]<-rnorm(n=1)
#   for(i in 2:length(x))  
#     x[i]<-x[i-1]*gamma+rnorm(n=1)
#   return(x)
# }) %>% unlist
# splitxs<-split(df$x,df$units)
# purtest(data.frame(splitxs),test="levinlin",exo="intercept",lags="AIC",pmax=3)
# fishertype.purtest(splitxs)

##########################################################

#this is a function which takes a list of unitrootvars
#checks to see if a given term is on that unitroot list
#and returns a first-difference of that term, if so..

ur.transform<-function(term,urlist) {
  #term<-"L.union.ratio"
  #urlist<-urdf$var[urdf$ur]
  #loop through all urvars, and see if you can find term
  tmp<-sapply(urlist,function(x) str_detect(term,x)) %>%
    sum
  if(sum(tmp)>0) {
    #transform
    ur.term<-add.diff(term)
    #D and L need to be switched
    y<-str_replace(
      ur.term,
      "(D([0-9]+)?\\.)(L([0-9]+)?\\.)",
      "\\3\\1"
    )
  } else {
    y<-term
  }
  #return
  return(y)
}

#wrapper for ur.transform
#splits up formulas, etc.
ur.transform2<-function(term,urlist) {
  # term<-"L.union.ratio + L2.union.ratio + L3.union.ratio"
  # urlist<-unitrootvars
  if(term!="") {
    #split the terms
    terms.split<-str_split(term,"\\s\\+\\s")[[1]]
    newterms.split<-sapply(terms.split,ur.transform,urlist)
    #loop through terms split, 
    #replace w/ its counterpart
    this.sequence<-seq_along(terms.split)
    y<-term
    for(i in this.sequence)
      y<-sub(terms.split[i],newterms.split[i],y,fixed=T)
  } else {
    y<-term
  }
  return(y)
}

##########################################################
##########################################################

#summarize runs
sumruns<-function(x,sep.me=":") {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  coloned <- paste(x[start_indexes], x[end_indexes], sep=sep.me)
  paste0(coloned, collapse=", ")
}

#DEPRECATED
#use na.approx, w/ options
# #for interpolation
# require(zoo)
# ipolate<-function(x) { 
#   output<-tryCatch(
#     {
#       x.i<-na.approx(x)  
#       if (length(x.i)!=length(x)) {
#         firstmatch<-min(which(x%in%x.i)) #index of first match
#         lastmatch<-max(which(x%in%x.i)) #index of last match
#         return(c(x[index(x)<firstmatch],x.i,x[index(x)>lastmatch])) #vector of original length, interpolated vals in middle  
#       }
#       return(x.i)
#     },error=function(cond) {
#       return(x)
#     }
#   )
#   return(output)
# } #end function


##########################################################
##########################################################

#end with this directory
setwd(olddir)

