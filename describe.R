############################################
# Compute descriptive stats, tables, figures, and narrative


describe=function(clm.name,    # name of column in quotation marks
                  data,        # data set as a data.frame
                  tbl=1,       # tabular output (0=none; 1=basic; 2=detailed)
                  fig=1,       # figure output (0=none; 1=basic; 2 and higher = more)
                  txt=1,       # narrative output (0=none; 1=basic; 2=detailed)
                  clr=NULL,    # color(s) to use
                  x.name=NULL) # name of x in quotation marks for narrative
  
{
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"clm.name")
  }
  
  x=data[,clm.name]
  cls=class(x)
  
  res=NULL
  if(any(cls%in%c("numeric","double","integer")))
    res=describe.numeric(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=x.name)
  
  if(any(cls%in%c("character","factor","ordered")))
    res=describe.categorical(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=x.name)
  
  if(any(cls%in%c("Surv","competing.events")))
    res=describe.event.timing(x,tbl=tbl,fig=fig,txt=txt,clr=clr,x.name=x.name)
  
  if (is.null(res))
    stop("Invalid input class.")
  
  return(res)
  
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
  if (!(cls[1]%in%c("Surv","competing.events")))
    stop("x must be of class Surv or competing.events.")
  
  
  # if nmx not provided then extract it from the input
  if (is.null(x.name))
  {
    cll=deparse(match.call())
    x.name=get.arg(cll,"x")
  }
  nmx=x.name

  
  # Kaplan-Meier curves
  if(cls=="Surv")
  {
    km=survfit(x~1)
    
    ylbl=paste0("Pr(",nmx,")")
    xlbl="time"

    if (fig>0)
    {
      temp.dset=cbind.data.frame(x=x)
      event.plot("x",temp.dset,x.name=nmx,clr=clr)
    }

    res.tbl=summary(km,times=pretty(c(0,max(x[,1]))))
    cum.event=cumsum(res.tbl$n.event)
    
    
    res.txt=""
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
    
    fnl.tbl=cbind.data.frame(time=res.tbl$time,
                             n.risk=res.tbl$n.risk,
                             n.event=res.tbl$n.event,
                             surv=res.tbl$surv)
    
    res=list(tbl=fnl.tbl,
             txt=res.txt,
             method=method,
             ref=ref)
    
    class(res)="SBP.result"
    
    return(res)
    
  }
  
  
  if (cls=="competing.events")
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
                             x.name=NULL)

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
  
  res.tbl=table(x,exclude=NULL)
  pct.tbl=100*res.tbl/sum(res.tbl)
  
  res.txt=""
  
  ##############################################
  # produce figures
  
  temp.dset=cbind.data.frame(x=x)
  if (fig>0)
  {
    bar.plot("x",temp.dset,all=F,x.name=nmx,clr=clr)
    if (fig>1) pie.plot("x",temp.dset,nmx,clr=clr)
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
            shapiro.pvalue=shapiro.test(x)$p.value)
  names(smry.stats)=c("n.total","n.missing","n.available",
                      "mean","stdev","median",
                      "lower.quartile","upper.quartile",
                      "minimum","maximum","shapiro.pvalue")
  
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
  dgts=ceiling(diff(range(log10(as.numeric(unlist(x))),na.rm=T)))
  res.txt=""
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
                    c(" differs "," does not differ ")[1+(smry.stats["shapiro.pvalue"]>0.05)],
                    "significantly from a normal distribution at the 0.05 level (Shapiro-Wilk p = ",smry.stats["shapiro.pvalue"],").  ")
    res.txt=c(res.txt,more.txt)
  }
  
  res.txt=paste0(res.txt,collapse="")
  
  method=paste0("The Shapiro-Wilk (1965) test was used to evaluate the normality of the distribution of ",nmx,".  ")
  
  ref='Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3-4): 591-611. doi:10.1093/biomet/52.3-4.591. JSTOR 2333709. MR 0205384.'
  

  
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
  class(res)="competing.events"
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
# get the argument value


get.arg=function(call.string, # obtain with deparse(match.call())
                 arg.string)  # character string with argument name
{

  x.pos=regexpr(paste0(arg.string," = "),call.string,fixed=T)
  x.name=substring(call.string,x.pos+nchar(arg.string)+3)
  x.name=x.name[x.pos>0]

  
  comma.pos=regexpr(",",x.name,fixed=T)
  close.pos=regexpr(")",x.name,fixed=T)
  

  
  end.pos=close.pos
  if (comma.pos>0) end.pos=comma.pos
  x.name=substring(x.name,1,end.pos-1)
  

  x.name=gsub('\"','',x.name)
  
  return(x.name)
}