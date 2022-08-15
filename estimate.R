#install.packages("DescTools")



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

