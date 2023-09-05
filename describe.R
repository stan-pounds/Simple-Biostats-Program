

############################################
# Compute descriptive stats, tables, figures, and narrative


describe=function(clm.name,    # name of column in quotation marks
                  data,        # data set as a data.frame
                  tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                  fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                  txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                  clr=NULL,    # color(s) to use
                  y.name=NULL, # name of x in quotation marks for narrative
                  use.all=T)   # indicates whether to include all data regardless of missingness
  
{
  try.clm=try(clm.name,silent=T)
  if (class(try.clm)=="try-error")
  {
    temp=deparse(match.call())
    clm.name=get.arg(temp,"clm.name")
  }

  data=data.frame(data)
  x=get.y.clm(clm.name,data)
  cls=class(x)
  
  if (is.null(y.name))
  {
    y.name=attr(x,"clm.name")
  }



  res=NULL
  res.est=NULL
  res.out=NULL
  if(any(cls%in%c("numeric","double","integer")))
    res=describe.numeric(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name)
  
  if(any(cls%in%c("character","factor","ordered")))
    res=describe.categorical(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name,use.all=use.all)
  
  if(any(cls%in%c("Surv","competing.events")))
    res=describe.event.timing(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=y.name)
  
  if (is.null(res))
    stop("Invalid input class.")
  
  res.est = estimate(clm.name, data, null=NULL, tbl, fig, txt, clr, y.name)
  
  #out.clm=get.y.clm(clm.name,data)
  res.out = outliers(clm.name, data, y.name, fig, txt, clr)

  comb.res=list(txt=c(res.est$txt,res.out$txt), #Multiple output with relevant info
                tbl=res.est$tbl, #Only need one of them or it double lists
                method=c(res.out$method,res.est$method), #res would double print that Shapiro Wilks was used
                ref=unique(c(res$ref,res.out$ref,res.est$ref)))
  
  return(comb.res)
  
}


###########################################
# Describe event time distribution with tables, figures, and a narrative

describe.event.timing=function(x,
                               tbl=1,
                               fig=1,
                               txt=1,
                               clr=NULL,
                               x.name=NULL)
  
{
  ######################################
  # check for proper input
  cls=class(x)
  if (!(any(cls%in%c("Surv","competing.events"))))
    stop("x must be of class Surv or competing.events.")
  
  
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name

  
  # Kaplan-Meier curves
  if(any(cls%in%"Surv"))
  {
    km=survfit(x~1)
    
    ylbl=paste0("Pr(",nmx,")")
    xlbl="time"

    if (fig>0)
    {
      temp.dset=cbind.data.frame(x=x)
      event.plot("x",temp.dset,y.name=nmx,clr=clr)
    }

    res.tbl=summary(km,times=pretty(c(0,max(x[,1]))))
    cum.event=cumsum(res.tbl$n.event)
    
    
    res.txt=NULL
    if (txt>0)
    {
      txt1=paste0("The event-time variable ",nmx," was observed for a cohort of ",
                  nrow(x)," subjects.  ",
                  "The data indicate that ",text.list(res.tbl$n.risk),
                  " subjects ",
                  "were respectively observed for least ",text.list(res.tbl$time),
                  " units of time without experiencing the ", nmx," event.  ")
      txt2=paste0("Also, ",text.list(cum.event)," subjects experience the ",nmx," event ",
                  "before times ",text.list(res.tbl$time),", respectively.")
      txt3=paste0("Based on this information, it is estimated that ",
                  text.list(paste0(round(100*res.tbl$surv,2),"%")),
                  " subjects in the population do not experience the ",nmx," event before times ",
                  text.list(res.tbl$time),", respectively.  ")
      
      res.txt=c(txt1,txt2,txt3)
      
    }
    
    method=paste0("The Kaplan-Meier (1958) method was used to estimate the distribution of ",
                  nmx," while accounting for censoring of event times.")
    
    ref='Kaplan, E. L.; Meier, P. (1958). "Nonparametric estimation from incomplete observations". J. Amer. Statist. Assoc. 53 (282): 457-481. doi:10.2307/2281868. JSTOR 2281868.'
    
    fnl.tbl=NULL
    
    if (tbl>0)
    {
      fnl.tbl=cbind.data.frame(time=res.tbl$time,
                               n.risk=res.tbl$n.risk,
                               n.event=res.tbl$n.event,
                               surv=res.tbl$surv)
    }
    if (tbl>1)
    {
      fnl.tbl$std.err=res.tbl$std.err
      fnl.tbl$CILB=res.tbl$lower
      fnl.tbl$CIUB=res.tbl$upper
    }
    

    
    res=list(tbl=fnl.tbl,
             txt=res.txt,
             method=method,
             ref=ref)
    
    class(res)="SBP.result"
    
    return(res)
    
  }
  
  
  if (any(cls%in%"competing.events"))
  {
    # Compute Cumulative Incidence Curve
    evnt.types=unique(x[,2])
    evnt.types=evnt.types[evnt.types!=0]
    clrs=define.colors(length(evnt.types),clr)
    ci=cuminc(x[,1],x[,2])
    
    # label the output
    names(ci)=substring(names(ci),3)
    ev.key=attr(x,"ev.key")
    for (i in 1:length(ev.key))
      names(ci)=gsub(names(ev.key)[i],ev.key[i],names(ci),fixed=T)
    
    # generate the plot
    xlbl="time"
    
    if (fig>0)
    {
      plot(ci,las=1,col=clrs,lty=1,lwd=2,
           xlab=xlbl,ylab="cumulative incidence")    
    }
    
    res.tbl=timepoints(ci,times=pretty(c(0,max(x[,1]))))$est
    res.tbl=t(res.tbl)
    rw=rowSums(res.tbl)
    res.tbl=res.tbl[!is.na(rw),]
    
    for (i in 1:length(ev.key))
      colnames(res.tbl)=gsub(names(ev.key)[i],ev.key[i],
                             colnames(res.tbl),fixed=T)
    
    res.txt=NULL
    if (txt>0)
    {
      
      for (i in 1:ncol(res.tbl))
      {
        txt1=paste0("Based on this event timing data for ",nrow(x)," subjects, ",
                    "it is estimated that ")
        txt2=paste0(text.list(paste0(100*round(res.tbl[,i],2),"%"))," of subjects in the population ",
                    "experience ",colnames(res.tbl)[i]," before times ",text.list(rownames(res.tbl)),", respectively.")
        res.txt=c(res.txt,paste0(txt1,txt2))
      }
    }
    
    method=paste0("Gray's (1988) method was used to estimate the cumulative incidence of each type of ",
                  nmx," event as a function of time accounting for censoring and competing events.")
    
    ref="Gray RJ (1988) A class of K-sample tests for comparing the cumulative incidence of a competing risk, ANNALS OF STATISTICS, 16:1141-1154."
    
    
    na.row=which(rowSums(is.na(res.tbl))==ncol(res.tbl))
    if (length(na.row)>0)  res.tbl=res.tbl[-na.row,]
    
    res.tbl=cbind(time=as.numeric(rownames(res.tbl)),res.tbl)
    
    res=list(tbl=res.tbl,
             txt=res.txt,
             method=method,
             ref=ref)
    
    class(res)="SBP.result"
    
    return(res)
  }
  
  
}


##########################################
# Describe a categorical variable with tables, figures, and a narrative

describe.categorical=function(x,
                             tbl=1,
                             fig=1,
                             txt=1,
                             clr=NULL,
                             x.name=NULL,
                             use.all=T)

{
  ######################################
  # check for proper input
  cls=class(x)
  if (!any(cls%in%c("character","factor","ordered")))
    stop("non-categorical x.")
  
  
  ######################################
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name

  ######################################
  # pick color scheme if not specified
  if (is.null(clr)) clr="rainbow"
  
  
  all.tbl=table(x,exclude=NULL)
  avl.tbl=table(x)
  
  n.miss=sum(all.tbl)-sum(avl.tbl)
  
  res.tbl=avl.tbl
  if (use.all) res.tbl=all.tbl

  pct.tbl=100*res.tbl/sum(res.tbl)
  
  res.txt=""
  
  ##############################################
  # produce figures
  
  temp.dset=cbind.data.frame(x=x)
  if (fig>0)
  {
    bar.plot("x",temp.dset,y.name=nmx,clr=clr,all=use.all)
    if (fig>1) pie.plot("x",temp.dset,nmx,clr=clr,all=use.all)
  }


  
  #######################################
  # Generate final table
  final.tbl=NULL
  final.tbl=cbind.data.frame(names(res.tbl),
                               n=as.vector(res.tbl),
                               percent=as.vector(pct.tbl))
    
  colnames(final.tbl)=c(nmx,"n","percent")

  
  ############################################
  # generate narrative
  
  res.txt=NULL
  if (txt>0)
  {
    res.txt=paste0("The categorical variable ",nmx," has ", length(x),
                   " observations:  ")
    n.txt=paste0(final.tbl[,"n"]," ",
                 final.tbl[,nmx],
                 " (",round(final.tbl[,"percent"],2),"%)")
    n.txt=paste0(n.txt,collapse=", ")
    n.txt=paste0(n.txt,".  ")
    res.txt=paste0(res.txt,n.txt)
    
    if ((!use.all)&(n.miss>0))
      res.txt=c(res.txt,
                paste0("This analysis ignores ",n.miss," missing data observations."))
    
    
  }

  
  
  if (tbl<1) final.tbl=NULL
  
  res=list(tbl=final.tbl,
           txt=res.txt,
           method=NULL,
           ref=NULL)
  
  class(res)="SBP.result"
  
  return(res)
}


#########################################
# Describe a numeric variable with tables, figures, and a narrative

describe.numeric=function(x,            # variable to describe
                          tbl=1,        # table: 0 = none, 1 = basic
                          fig=2,        # figure: 0 = none, 1 = boxplot, 2= boxplot+histogram, 3 = boxplot+histogram+qqnorm
                          txt=1,        # narrative detail (0 = none; 1 = basic; 2 = detailed)
                          clr=NULL,     # color to be used in the figures
                          x.name=NULL)  # character string giving name of x variable
  
{
  

  ######################################
  # check for proper input
  
  cls=class(x)
  if (!(cls%in%c("numeric","double","integer")))
    stop("non-numeric x.")
  
  ######################################
  # get name of x from input
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name
  
  ######################################
  # pick color if not specified
  if (is.null(clr)) clr="gray"
  

  ######################################
  # descriptive statistics
  nml.test=normality.test(x)
  smry.stats=c(n.total=length(x),
            n.missing=sum(is.na(x)),
            n.available=sum(!is.na(x)),
            mean=mean(x,na.rm=T),
            stdev=sd(x,na.rm=T),
            median=median(x,na.rm=T),
            lower.quartile=quantile(x,0.25,na.rm=T),
            upper.quartile=quantile(x,0.75,na.rm=T),
            minimum=min(x,na.rm=T),
            maximum=max(x,na.rm=T),
            normality.pvalue=nml.test$p.value)
  names(smry.stats)=c("n.total","n.missing","n.available",
                      "mean","stdev","median",
                      "lower.quartile","upper.quartile",
                      "minimum","maximum","normality.pvalue")
  
  ######################################
  # tables
  res.tbl=NULL
  if (tbl>0) res.tbl=smry.stats
  
  ####################################
  # figures
  xlbl=nmx
  temp.dset=as.data.frame(x=x)
  colnames(temp.dset)=x.name

  if (fig>0) box.plot(x.name,temp.dset,x.name,clr)
  if (fig>1) bar.plot(x.name,temp.dset,x.name,clr=clr)
  if (fig>2) nqq.plot(x.name,temp.dset,x.name,clr=clr)

  ##############################################
  # narrative text
  dgts=ceiling(diff(range(log10(abs(as.numeric(unlist(x)))),na.rm=T)))
  res.txt=NULL
  if (txt>0)
  {
    res.txt=paste0("The variable ",nmx," has ", smry.stats["n.total"], 
                   " observations (",
                   smry.stats["n.available"]," available; ",
                   smry.stats["n.missing"]," missing)  ",
                   "with mean ",round(smry.stats["mean"],dgts),
                   ", standard deviation ",round(smry.stats["stdev"],dgts),
                   ", median ",round(smry.stats["median"],dgts),
                   ", lower quartile ",round(smry.stats["lower.quartile"],dgts),
                   ", upper quartile ",round(smry.stats["upper.quartile"],dgts),
                   ", minimum ",round(smry.stats["minimum"],dgts),
                   ", and maximum ",round(smry.stats["maximum"],dgts),".  ")
  }
  
  if (txt>1)
  {
    more.txt=paste0("The distribution of ",nmx,
                    c(" differs "," does not differ ")[1+(smry.stats["normality.pvalue"]>0.05)],
                    "significantly from a normal distribution at the 0.05 level (p = ",smry.stats["normality.pvalue"],").  ")
    res.txt=c(res.txt,more.txt)
  }
  
  if (txt>0) res.txt=paste0(res.txt,collapse="")
  
  method=paste0("The ",nml.test$method ," was used to evaluate the normality of the distribution of ",nmx,".  ")
  
  ref=NULL
  if (grepl("Shapiro",nml.test$method))
    ref='Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3-4): 591-611. doi:10.1093/biomet/52.3-4.591. JSTOR 2333709. MR 0205384.'
  
  if (grepl("Smirnov",nml.test$method))
    ref="George Marsaglia, Wai Wan Tsang and Jingbo Wang (2003). Evaluating Kolmogorov's distribution. Journal of Statistical Software, 8/18. doi:10.18637/jss.v008.i18."
  

  
  res=list(tbl=res.tbl,
           txt=res.txt,
           method=method,
           ref=ref)
  
  attr(res,"result.type")="describe.numeric"
  attr(res,"result.name")=x.name
  
  class(res)="SBP.result"
  
  return(res)
}


###################################
# 

numeric.descriptive.table=function(x)
  
{
  res=describe.numeric(x,tbl=1,fig=0,txt=0)$tbl
  return(res)
}


################################
# Create a competing event time variable

competing.event.time=function(obs.time,
                              obs.event,
                              ev.key=NULL)
  
{
  res=cbind(obs.time,obs.event)
  
  if (is.null(ev.key))
  {
    ev.key=sort(unique(obs.event))
    names(ev.key)=ev.key
  }
  
  attr(res,"ev.key")=ev.key
  class(res)=c("matrix","competing.events")
  return(res)
}


###########################################
# write a list of items as a narrative

text.list=function(x)
  
{
  if (length(x)==1) return(x)
  if (length(x)==2) return(paste0(x,collapse=" and "))
  
  n=length(x)
  return(paste0(paste0(x[-n],collapse=", ")," and ",x[n]))
}

#######################################
#######################################
# Estimate

estimate=function(clm.name,
                  data,
                  null=NULL,
                  tbl=1,
                  fig=1,
                  txt=1,
                  clr=NULL,
                  x.name=NULL)
  
{
  data=data.frame(data)
  x=get.y.clm(clm.name,data)
  if (is.null(x.name))
    x.name=attr(x,"clm.name")
  res=estimate.pop.value(x,null,tbl,fig,txt,clr,x.name)
  return(res)
}


###################################################
# Estimate population values

estimate.pop.value=function(x,           # categorical variable to describe
                            null=NULL,   # hypothesized null value
                            tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                            fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                            txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                            clr=NULL,    # color(s) to use
                            x.name=NULL) # name of x variable to use in narrative output
  
{
  if(is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name
  
  x.class=class(x)
  
  if (any(x.class%in%c("numeric","integer","double")))
  {
    res=estimate.center(x,null,tbl,fig,txt,clr,nmx)
    return(res)
  }
  
  if (any(x.class%in%c("factor","character","ordered")))
  {
    res=estimate.proportion(x,tbl,fig,txt,clr,nmx)
    return(res)
  }
  
  if (any(x.class%in%c("competing.events","Surv")))
  {
    res=estimate.events(x,tbl,fig,txt,clr,nmx)
    return(res)
  }
  
  stop(paste0("class(x) is ",x.class,"; this function is defined only for numeric, factor, character, competing.events, or Surv variables.  No output produced."))
  
  
}


#####################################################
# Estimate events

estimate.events=function(x,           # categorical variable to describe
                         tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                         fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                         txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                         clr=NULL,    # color(s) to use
                         x.name=NULL) # name of x variable to use in narrative output
  
{
  if(is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name
  
  res=describe.event.timing(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=nmx)
  class(res)="SBP.result"
  return(res)
}


###################################
# estimate the proportion(s) of a population belonging to a category or categories

estimate.proportion=function(x,           # categorical variable to describe
                             tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                             fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                             txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                             clr=NULL,    # color(s) to use
                             x.name=NULL) # name of x variable to use in narrative output
{
  if(is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name
  
  desc.res=describe.categorical(x,tbl=1,x.name=nmx,clr=clr,fig=fig,txt=txt)
  
  n=sum(desc.res$tbl$n)
  k=nrow(desc.res$tbl)
  ci.tbl=matrix(NA,k,2)
  colnames(ci.tbl)=c("95% LB","95% UB")
  
  for (i in 1:k)
  {
    bnm.res=binom.test(desc.res$tbl$n[i],n)
    ci.tbl[i,1]=bnm.res$conf.int[1]
    ci.tbl[i,2]=bnm.res$conf.int[2]
  }
  
  tbl=cbind.data.frame(desc.res$tbl,ci.tbl)
  
  txt2=paste0("Based on this data, it is estimated that ",
              text.list(paste0(round(desc.res$tbl$percent,2),"% (95% CI: ",
                               round(100*ci.tbl[,1],2),"%, ",
                               round(100*ci.tbl[,2],2),"%)")),
              " of subjects in the population are ",nmx," categories ",
              text.list(desc.res$tbl[,1]),", respectively.")
  
  res.txt=c(desc.res$txt,txt2)
  
  method=paste0("The binomial distribution was used to estimate confidence intervals for the ",
                "proportions of ",nmx," ",text.list(tbl[,1])," in the population.  ")
  
  res=list(tbl=tbl,
           txt=res.txt,
           method=method,
           ref=NULL)
  
  class(res)="SBP.result"
  
  return(res)
  
}

#####################################################
# Estimate the center of the distribution of a numeric variable

estimate.center=function(x,          # numeric variable to describe
                         null=NULL,  # hypothesized null value
                         tbl=1,      # tabular output (0=none; 1=basic; 2=detailed)
                         fig=1,      # figure output (0=none; 1=basic; 2 and higher = more)
                         txt=1,      # narrative output (0=none; 1=basic; 2=detailed)
                         clr=NULL,   # color(s) to use
                         x.name=NULL)
  
{
  
  ###
  # if nmx not provided then extract it from the input
  if(is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name
  
  dgts=ceiling(diff(range(log10(as.numeric(unlist(x))),na.rm=T)))
  
  desc.res=describe.numeric(x,x.name=x.name,fig=fig,txt=txt,clr=clr,tbl=tbl)
  
  input.null=null
  if (is.null(null)) null=0
  
  sw.res=shapiro.test(x)
  sym.res=symmetry.test(x,boot=F,option="CM")
  
  test.num=1+(sw.res$p.value<0.05)+(sym.res$p.value<0.05)
  test.name=c("t-test","Wilcoxon signed-rank test","sign test")[test.num]
  
  t.res=t.test(x,mu=null)
  wilcox.res=wilcox.test(x,mu=null,conf.int=T)
  sign.res=SignTest(x,mu=null)
  
  mn=mean(x,na.rm=T)
  mdn=median(x,na.rm=T)
  
  p=c(t.res$p.value,
      wilcox.res$p.value,
      sign.res$p.value)[test.num]
  
  cilb=c(t.res$conf.int[1],
         wilcox.res$conf.int[1],
         sign.res$conf.int[1])[test.num]
  
  ciub=c(t.res$conf.int[2],
         wilcox.res$conf.int[2],
         sign.res$conf.int[2])[test.num]
  
  
  txt1=paste0("The Shapiro-Wilk test ",
              " did ",c("","not ")[1+(sw.res$p.value>0.05)],"find statistically compelling evidence that ",
              "the distribution of ",nmx," is not normal (p = ",sw.res$p.value,").  ")
  txt2=paste0("The Cabilio-Masaro symmetry test ",
              c("finds","does not find")[1+(sym.res$p.value>0.05)]," statistically compelling evidence that ",
              "the distribution of ",nmx," is asymetric.  ",
              "(p = ",sym.res$p.value,").  ")
  txt3=paste0("Therefore, the ",test.name," was used to evaluate the null hypothesis that the ",
              "population ",c("mean","median","median")[test.num]," equals ",null,".  ")
  
  txt4=paste0("In this data set, the ",c("mean","median","median")[test.num]," of ",nmx,
              " was ",round(c(mn,mdn,mdn)[test.num],dgts),
              " (95% CI: ",round(cilb,dgts),", ",round(ciub,dgts),").  ")
  
  if (!is.null(input.null))
    txt4=c(txt4,
           paste0("This data ",
                  c("provides","does not provide")[1+(p>0.05)]," statistically compelling evidence",
                  " against the null hypothesis that the population ",c("mean","median","median")[test.num],
                  " equals ",null," (p = ",p,")."))
  
  
  full.txt=NULL
  if (txt>0) full.txt=c(desc.res$txt,txt4)
  if (txt>1) full.txt=c(full.txt,txt2,txt3)
  
  method=paste0("To determine the best statistical procedure to use for estimating the center of ",
                "the population distribution of ",nmx,
                ", the Shapiro-Wilk (1965) test was used to evaluate the normality of ",
                nmx," and the Cabilio-Masaro (1996) test was used to evaluate the symmetry of ",nmx,".  ")
  primary.method=paste0("Based on these evaluations, the ",test.name," was used to estimate a confidence interval for ",
                        "the population ",c("mean","median","median")[test.num]," of ",nmx,".  ")
  
  if (!is.null(input.null)) 
    primary.method=c(primary.method,
                     paste0("The ",test.name," was also used to test the null hypothesis that that the population ",
                            c("mean","median","median")[test.num]," of ",nmx," equals ",null,".  "))
  
  method=c(method,primary.method)
  
  
  
  ref=c('Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3-4): 591-611. doi:10.1093/biomet/52.3-4.591. JSTOR 2333709. MR 0205384.',
        'Cabilio P, Masaro J (1996). "A simple test of symmetry about an unknown median." Canadian Journal of Statistics, 24(3), 349-361. doi: 10.2307/3315744.')
  
  primary.ref=c("",
                'Wilcoxon, Frank (Dec 1945). "Individual comparisons by ranking methods" (PDF). Biometrics Bulletin. 1 (6): 80-83. doi:10.2307/3001968. hdl:10338.dmlcz/135688. JSTOR 3001968.',
                'Conover, W.J. (1999), "Chapter 3.4: The Sign Test", Practical Nonparametric Statistics (Third ed.), Wiley, pp. 157-176, ISBN 0-471-16068-7')[test.num]
  
  ref=c(ref,primary.ref)
  ref=ref[ref!=""]
  
  
  res=list(tbl=desc.res$tbl,
           txt=full.txt,
           method=method,
           ref=ref)
  
  class(res)="SBP.result"
  
  return(res)
  
}

#####################################################

outliers=function(clm,dset,y.name=NULL,fig=1,txt=1,
                  clr=c("gray","red"))
  
{
  dset=data.frame(dset)
  x=get.y.clm(clm,dset)
  
  if (is.null(y.name)) y.name=attr(x,"clm.name")
  x.lbl=y.name
  
  if (class(x)%in%c("numeric","double","integer"))
  {
    
    na=is.na(x)
    bx.out=boxplot.stats(x)$out # boxplot definition of outliers
    
    lts.res=ltsReg(x~1)
    lts.out=lts.res$Y[lts.res$lts.wt==0] # least trimmed squares
    
    clrs=define.colors(2,clr)
    x.out=x%in%c(bx.out,lts.out)
    x.clrs=rep(clrs[1],length(x))
    x.clrs[x.out]=clrs[2]
    
    
    if (fig>0) 
    {
      y=jitter(rep(1,length(x)))
      par(mar=rep(4,4))
      plot(x,y,cex=0.5,xlab=x.lbl,ylab="",yaxt="n",
           pch=19,col=x.clrs)
    }
    if (txt>0)
    {
      fnl.txt=paste0("No values of ",y.name," met the boxplot or least trimmed squares definition of outlier (Rousseeuw 1984).")
      if (sum(x.out)>0)
        fnl.txt=paste0("Outlier(s) in ",y.name," by the boxplot or least trimmed squares (Rousseeuw 1984) definitions include ",
                       text.list(sort(unique(x[x.out]))),".  ")
    }
    
    
    res=list(txt=fnl.txt,
             mthd="Outliers were detected using the boxplot and least trimmed squares definitions.",
             ref="Peter J. Rousseeuw (1984), Least Median of Squares Regression. Journal of the American Statistical Association 79, 871–881.")
    
    class(res)="SBP.result"
    return(res)
  }
  stop("Input must be a numeric data column to find outliers.")
}

