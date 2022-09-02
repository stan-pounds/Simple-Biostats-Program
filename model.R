


model=function(form,data,
               tbl=1,fig=1,txt=1,
               clr=NULL)
  
{
  data=data.frame(data)
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]
  
  if (any(class(y)%in%c("competing.events","Surv")))
  {
    res=model.events(form,data,tbl,fig,txt,clr)
    return(res)
  }
  
  if (any(class(y)%in%c("numeric","double","integer")))
  {
    y.binary=all(is.element(y,c(0:1,NA)))
    if (is.null(clr)) clr=c("black","red")
    if (y.binary)
    {
      message(paste0("R is modeling Pr(",y.clm," = 1)."))
      res=model.binary(form,data,tbl,fig,txt,clr)
      return(res)
    }
    res=model.numeric(form,data,tbl,fig,txt,clr)
    return(res)
  }
  
  if (class(y)%in%c("factor","character"))
  {
    uniq.y=unique(y)
    uniq.y=setdiff(y,c(NA,NaN))
    if (length(uniq.y)>2)
    {
      stop(paste0(y.clm," is a categorical variable with more than two levels. \n",
                  "Define a new variable with dset$new.variable.name=(dset$",y.clm,
                  "== level.of.interest) and try to model the new variable."))
    }
    message(paste0(y.clm," is a categorical variable with exactly two levels.  \n",
                   "R is modeling Pr(",y.clm,"=",uniq.y[1],")."))
    data[,y.clm]=(data[,y.clm]==uniq.y[i])
    res=model.binary(form,data,tbl,fig,txt,clr)
    return(res)
  }
  
  if (class(y)%in%c("logical"))
  {
    res=model.binary(form,data,tbl,fig,txt,clr)
    return(res)
  }
  
  
}


library(survival)

model.events=function(form,data,
                      tbl=1,fig=3,txt=1,
                      clr="rainbow")
  
{
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]
  
  if (any(class(y)%in%"Surv"))
  {
    cox.res=coxph(form,data)
    cox.tbl=fit.cox(form,data)
    txtres=model.txt(cox.res)
    y.hat=cox.res$linear.predictors
    
    
    if (fig>0)
    {
      grp=cut(y.hat,c(-Inf,quantile(y.hat,(1:2)/3,na.rm=T),Inf))
      srv=cox.res$y
      temp.dset=cbind.data.frame(srv,grp,y.hat)
      event.plot(srv~grp,temp.dset,clr=clr,y.name=y.clm)
    }
    
    res=model.txt(cox.res)
    
    class(res)="SBP.result"
    
    return(res)
  }
  
  
  if (any(class(y)%in%"competing.events"))
  {
    mdl.frm=model.frame(form,data,na.action=na.omit)
    mdl.terms=attr(mdl.frm,"terms")
    term.class=attr(mdl.terms,"dataClasses")
    
    form.str=deparse(form)
    tilde.pos=regexpr("~",form.str,fixed=T)
    xvar.str=substring(form.str,tilde.pos)
    xvar.form=eval(parse(text=xvar.str))
    mdl.mtx=model.matrix(xvar.form,data=mdl.frm)
    #print(head(mdl.mtx))
    x.clms=setdiff(colnames(mdl.mtx),"(Intercept)")
    x.mtx=matrix(mdl.mtx[,x.clms],ncol=length(x.clms))
    colnames(x.mtx)=x.clms
    crr.res=crr(as.numeric(mdl.frm[,y.clm][,1]),
                mdl.frm[,y.clm][,2],
                x.mtx)
    tbl.crr=crr.tbl(crr.res)
    
    fnl.tbl=list(tbl.crr=tbl.crr)
    
    ev.key=attr(y,"ev.key")
    
    pr.event=ev.key[is.element(names(ev.key),1)]
    cmp.events=ev.key[!is.element(names(ev.key),0:1)]
    
    
    txt.crr=NULL
    num.terms=intersect(rownames(tbl.crr),
                        names(term.class)[term.class=="numeric"])
    if (length(num.terms)>0)
    {
      num.txt=paste0("The model estimates that the rate of ",pr.event," changes by a factor of ",
                     tbl.crr[num.terms,"hazard.ratio"]," with each unit increase of ",
                     num.terms," (95% CI: ",tbl.crr[num.terms,"LB95"],", ",tbl.crr[num.terms,"UB95"],
                     "; p = ",tbl.crr[num.terms,"p"],").  ")
      txt.crr=c(txt.crr,num.txt)
    }
    
    fct.terms=names(term.class)[term.class=="factor"]
    if (length(fct.terms)>0)
    {
      n.fct=length(fct.terms)
      fct.txt=NULL
      for (i in 1:n.fct)
      {
        lvls=levels(data[,fct.terms[i]])
        tbl.rws=paste0(fct.terms[i],lvls[-1])
        tbl.rws=intersect(tbl.rws,rownames(tbl.crr))
        if (length(tbl.rws)>0)
        {
          row.txt=paste0("The model estimates that the ",fct.terms[i]," group ",gsub(fct.terms[i],"",tbl.rws),
                         " experiences the primary ",y.clm," event ",pr.event," at ",
                         tbl.crr[tbl.rws,"hazard.ratio"]," times the rate of the ",
                         fct.terms[i]," group ",lvls[1],".  ")
          fct.txt=c(fct.txt,row.txt)
        }
      }
      txt.crr=c(txt.crr,fct.txt)
    }
    
    if (fig>0)
    {
      
      y.hat=predict(crr.res,x.mtx)
      y.hat.bar=colMeans(y.hat[,-1])
      grp=cut(y.hat.bar,c(-Inf,quantile(y.hat.bar,(1:2)/3,na.rm=T),Inf))
      grp=c("low","intermediate","high")[as.numeric(grp)]
      grp=factor(grp,c("low","intermediate","high"))
      tm=mdl.frm[,y.clm][,1]
      ev=mdl.frm[,y.clm][,2]
      evnt=ev.key[as.character(ev)]
      ci.res=cuminc(tm,
                    evnt,
                    grp,cencode=ev.key[as.character(0)])
      
      res.tbl=timepoints(ci.res,times=pretty(c(0,max(y[,1]))))$est
      res.tbl=t(res.tbl)
      
      res.tbl2=ci.res$Tests
      
      
      
      crv.names=colnames(res.tbl)
      grp.names=crv.names
      evt.names=crv.names
      for (i in 1:length(ev.key)) grp.names=gsub(paste0(" ",ev.key[i]),"",grp.names,fixed=T)
      
      uniq.grps=unique(grp.names)
      for (i in 1:length(uniq.grps)) evt.names=gsub(paste0(uniq.grps[i]," "),"",evt.names,fixed=T)
      uniq.evnts=unique(evt.names)      
      
      n.grps=length(uniq.grps)
      clrs=define.colors(n.grps,clr)
      names(clrs)=uniq.grps
      ci.clrs=clrs[grp.names]
      
      n.types=length(uniq.evnts)
      lty=1:n.types
      names(lty)=uniq.evnts
      ci.lty=lty[evt.names]
      
      fig.txt=paste0("To describe the predictions of the model, the cohort was trichotomized into groups with ",text.list(sort(unique(grp))),
                     " predicted rates of the primary ",y.clm," event ",pr.event,".  ",
                     "The cumulative incidence of each ",y.clm," event (",text.list(ev.key[!is.element(names(ev.key),0)]),") was computed for those with ",
                     text.list(sort(unique(grp)))," rates of the primary ",y.clm," event ",pr.event," according to ",
                     "the fitted model.  ")
      txt.crr=c(txt.crr,fig.txt)
      
      res.txt=NULL
      for (i in 1:ncol(res.tbl))
      {
        clm.txt=paste0("Based on this data, it is estimated that ",
                       text.list(paste0(round(100*res.tbl[,i],2),"%")),
                       " of subjects in the group with an estimated ",grp.names[i],
                       " rate of the primary ",y.clm," event ",pr.event, 
                       " have experiened ",evt.names[i]," by times ",
                       text.list(rownames(res.tbl)),", respectively.  ")
        res.txt=c(res.txt,clm.txt)
      }
      
      txt.crr=c(txt.crr,res.txt)
      
      plot(ci.res,col=ci.clrs,lty=ci.lty,las=1,xlab="Time",lwd=2)
      
      fnl.tbl=list(tbl.crr=tbl.crr,
                   tbl.cminc=res.tbl)
      
      
      
    }
    
    
    
    
    
    mtd.str=paste0("A Fine-Gray (1999) proportional hazards model with ",text.list(form.vars$x.vars)," as predictor(s) of ",
                   "the primary ",y.clm," event ", pr.event, " and adjusting for the ",y.clm, 
                   " competing events ",text.list(cmp.events)," was fit to the data.")
    
    
    res=list(tbl=fnl.tbl,
             txt=txt.crr,
             ref="Fine JP and Gray RJ (1999) A proportional hazards model for the subdistribution of a competing risk. JASA 94:496-509.",
             method=mtd.str)
    
    return(res)
    
  }
}


model.binary=function(form,data,
                      tbl=1,fig=3,txt=1,
                      clr="rainbow")
  
{
  glm.res=glm(formula=form,data=data,family=binomial)
  res.tbl=glm.tbl(glm.res)
  
  r=residuals(glm.res)
  y.hat=predict(glm.res)
  p.hat=exp(y.hat)/(1+exp(y.hat))
  y.obs=glm.res$y
  
  if (fig>1)
  {
    boxplot(p.hat~y.obs,horizontal=T,
            xlab="Model Probability Prediction",
            ylab="Observed Outcome",las=1)
  }
  
  
  res=list(tbl=res.tbl,
           txt="")
  
  return(res)
  
}


model.numeric=function(form,data,
                       tbl=1,fig=3,txt=1,
                       clr=c("black","red"))
  
{
  form.vars=get.vars(form)
  x.clm=form.vars$x.var
  y.clm=form.vars$y.var
  
  for (i in x.clm)
  {
    x=data[,i]
    cls=class(x)
    if (any(cls=="ordered"))
    {
      x=as.character(x)
      data[,i]=x
    }
  }
  
  clrs=define.colors(2,clr)
  
  print(class(data))
  glm.res=glm(formula=form,data=data,x=T,y=T)
  print("GLM Done")
  res.tbl=glm.tbl(glm.res)
  print("glm.tbl Done")
  
  r=residuals(glm.res)
  r.sw=normality.test(r)
  y.hat=predict(glm.res)
  y.obs=glm.res$y
  
  
  
  temp.dset=cbind.data.frame(y=y.obs,y.hat=y.hat,r=r)
  
  if (nrow(res.tbl)==2)
    temp.dset$x=glm.res$x[,2]
  
  if (fig>0)
  {
    if (nrow(res.tbl)==2)
    {
      print(class(temp.dset))
      scatter.plot(y~x,temp.dset,clrs,line=1,
                   x.name=x.clm,y.name=y.clm,txt=1)
    }
    
    if (nrow(res.tbl)>2)
      scatter.plot(y~y.hat,temp.dset,clrs,line=1,
                   x.name=paste0("Model Prediction of ",y.clm),
                   y.name=paste0("Observed Value of ",y.clm),txt=1)
    
  }
  
  
  if (fig>1) 
  {
    if(nrow(res.tbl)==2)
    {
      scatter.plot(r~x,temp.dset,clrs,line=1,
                   x.name=x.clm,y.name=paste0(y.clm," Error (Actual-Predicted"))
    }
    
    if (nrow(res.tbl)>2)
    {
      scatter.plot(r~y.hat,temp.dset,line=0,
                   x.name=paste0("Model Prediction of ",y.clm),
                   y.name=paste0(y.clm," Error (Actual-Predicted)"))
    }
  }
  
  
  if (fig>2)
  {
    if (nrow(res.tbl)==2)
    {
      scatter.plot(y~x,temp.dset,clrs,line=1,
                   x.name=x.clm,y.name=y.clm,txt=1)
      
      segments(temp.dset$x,
               temp.dset$y,
               temp.dset$x,
               temp.dset$y.hat,
               col=clrs[2])
      
      scatter.plot(r~x,temp.dset,clrs,line=1,
                   x.name=x.clm,y.name=paste0(y.clm," Error (Actual-Predicted"))
      
      segments(temp.dset$x,0,
               temp.dset$x,
               temp.dset$r,
               col=clrs[2])
      
    }
    
    if (nrow(res.tbl)>2)
    {
      scatter.plot(y~y.hat,temp.dset,clr,line=1,
                   x.name=paste0("Model Prediction of ",y.clm),
                   y.name=paste0("Observed Value of ",y.clm),txt=1)
      
      segments(temp.dset$y.hat,
               temp.dset$y.hat,
               temp.dset$y.hat,
               temp.dset$y,
               col=clrs[2])
      
      scatter.plot(r~y.hat,temp.dset,line=0,
                   x.name=paste0("Model Prediction of ",y.clm),
                   y.name=paste0(y.clm," Error (Actual-Predicted)"))
      
      segments(temp.dset$y.hat,0,
               temp.dset$y.hat,
               temp.dset$r,
               col=clrs[2])
      
      
    }
    
  }
  
  if (fig>3) bar.plot("r",temp.dset,paste0(y.clm," Error (Actual-Predicted)"),clr=clrs[2])
  if (fig>4) nqq.plot("r",temp.dset,paste0(y.clm," Error (Actual-Predicted)"),clr)
  
  res=model.txt(glm.res)
  
  
  
  return(res)
  
}


##########################################
# Generate narrative results for a model

model.txt=function(model.result)
  
{
  
  coef.tbl=coef(summary(model.result))
  CI.tbl=confint(model.result)
  coef.names=rownames(coef.tbl)
  
  model.form=deparse(model.result$formula)
  tilde.pos=regexpr(" ~ ",model.form,fixed=T)
  y.var=substring(model.form,1,tilde.pos-1)
  x.var=strsplit(substring(model.form,tilde.pos+3),split=" + ",fixed=T)
  x.var=unlist(x.var)
  
  
  res.terms=model.result$terms
  term.labels=attr(res.terms,"term.labels")
  term.classes=attr(res.terms,"dataClasses")
  res.levels=model.result$xlevels
  model.class=class(model.result)
  
  model.type=""
  y.txt=""
  ass.txt=NULL
  res.txt=NULL
  ref.grps=NULL
  grp.txt=NULL
  mth.txt=NULL
  ref.txt=NULL
  ass.mthd=NULL
  
  
  
  if (model.class[1]=="coxph")
  {
    CI.tbl=exp(CI.tbl)
    res.tbl=cbind(HR=coef.tbl[,"exp(coef)"],
                  LB95=CI.tbl[,1],
                  UB95=CI.tbl[,2],
                  p=coef.tbl[,"Pr(>|z|)"])
    rownames(res.tbl)=rownames(coef.tbl)
    model.type="Cox"
    y.txt=paste0("rate of ",y.var," events")
    
    zph.res=try(cox.zph(model.result,terms=F),silent=T)
    if (class(zph.res)!="try-error")
    {
      p.PHA=zph.res$table[,"p"]
      global.PHA=zph.res$table["GLOBAL","p"]
      p.PHA=p.PHA[rownames(res.tbl)]
      res.tbl=cbind.data.frame(res.tbl,p.PHA=p.PHA)
      ass.txt=paste0("The data ",c("do not ","")[1+(p.PHA<0.05)],
                     "provide statistically compelling evidence that ",
                     "the proportional hazards framework ",
                     "inaccurately represents ",
                     "the relationship of ",y.var," with ",
                     names(p.PHA)," (p = ",p.PHA,").  ")
    }
    ref.txt='Cox, David R (1972). "Regression Models and Life-Tables". Journal of the Royal Statistical Society, Series B. 34 (2): 187-220. JSTOR 2985181. MR 0341758.'
    
  }
  
  
  if ((model.class[1]=="glm"))
  {
    model.family=model.result$family$family
    model.link=model.result$family$link
    if ((model.family=="gaussian")&&(model.link=="identity"))
    {
      res.tbl=cbind(coef=coef.tbl[,"Estimate"],
                    LB95=CI.tbl[,1],
                    UB95=CI.tbl[,2],
                    p=coef.tbl[,"Pr(>|t|)"])
    }
    model.type="linear"
    y.txt=paste0("model's predicted value of ",y.var)
    r=residuals(model.result)
    sw.res=normality.test(r)
    ass.txt=paste0("The data ",c("do not ","")[1+(sw.res$p.value<0.05)],"provide statistically compelling evidence that ",
                   "the normal distribution inaccurately represents the distribution of the prediction errors",
                   " (actual-predicted value) of this model (p = ",sw.res$p.value,").  ")
    
    ass.mthd=paste0("The ",sw.res$method," was used to formally evaluate the normality of the ",
                    "prediction errors (actual-predicted value) of the model.")
    
    if (grepl("Shapiro",sw.res$method))
    {
      ref.txt='Shapiro, S. S.; Wilk, M. B. (1965). "An analysis of variance test for normality (complete samples)". Biometrika. 52 (3–4): 591–611. doi:10.1093/biomet/52.3-4.591'
    }
    if (grepl("Kolmogorov",sw.res$method))
    {
      ref.txt="George Marsaglia, Wai Wan Tsang and Jingbo Wang (2003). Evaluating Kolmogorov's distribution. Journal of Statistical Software, 8/18. doi:10.18637/jss.v008.i18."
    }
  }
  
  
  
  if (length(res.levels)>0)
  {
    for(i in 1:length(res.levels))
    {
      var.name=names(res.levels)[i]
      lvl.names=res.levels[[i]]
      cf.names=paste0(var.name,lvl.names[-1])
      temp=cbind.data.frame(var.name=var.name,
                            lvl.names=lvl.names[-1],
                            ref.name=lvl.names[1],
                            coef.name=cf.names)
      ref.grps=rbind.data.frame(ref.grps,temp)
    }
    
    
    rownames(ref.grps)=ref.grps$coef.name
    common.names=intersect(rownames(ref.grps),rownames(res.tbl))
    
    # write narratives for categorical predictors
    if (length(common.names)>0)
    {
      if (model.type=="Cox")
      {
        grp.txt=paste0("The model estimates that the ",y.txt," of the ",ref.grps[common.names,"lvl.names"]," ",
                       ref.grps[common.names,"var.name"]," group is ",res.tbl[common.names,1],
                       " times that of the ",ref.grps[common.names,"lvl.names"]," ",
                       ref.grps[common.names,"ref.name"]," ",ref.grps[common.names,"var.name"]," group (95% CI: ",
                       res.tbl[common.names,"LB95"],", ",res.tbl[common.names,"UB95"],
                       "; p = ",res.tbl[common.names,"p"],".")
        res.txt=c(res.txt,grp.txt)
      }
      
      if (model.type=="linear")
      {
        grp.txt=paste0("The ",y.txt," for the ",ref.grps[common.names,"lvl.names"]," ",
                       ref.grps[common.names,"var.name"]," group minus that for the ",
                       ref.grps[common.names,"ref.name"],ref.grps[common.names,"lvl.names"]," group is ",
                       res.tbl[common.names,1]," (95% CI: ",res.tbl[common.names,"LB95"],
                       ", ",res.tbl[common.names,"UB95"],"; p = ",res.tbl[common.names,"p"],").  ")
        res.txt=c(res.txt,grp.txt)
      }
    }
  }
  
  # write narratives for numeric predictors
  num.names=names(term.classes[term.classes=="numeric"])
  #print(num.names)
  #print(rownames(res.tbl))
  num.names=intersect(rownames(res.tbl),num.names)
  #print(num.names)
  if (length(num.names)>0)
  {
    if (model.type=="Cox")
    {
      num.txt=paste0("The model estimates that each unit increase of ",num.names,
                     " associates with changing the ",y.txt," by a factor of ",
                     res.tbl[num.names,1]," (95% CI: ",res.tbl[num.names,"LB95"],
                     ", ",res.tbl[num.names,"UB95"],"; p = ",res.tbl[num.names,"p"],").  ")
      res.txt=c(res.txt,num.txt)        
    }
    
    if (model.type=="linear")
    {
      num.txt=paste0("For each unit increase of ",num.names, ", the ",
                     y.txt," changes by ",res.tbl[num.names,1],
                     " units (95% CI: ",res.tbl[num.names,"LB95"],
                     ", ",res.tbl[num.names,"UB95"],"; p = ",
                     res.tbl[num.names,"p"],").  ")
      res.txt=c(res.txt,num.txt)
    }
    
  }
  
  
  
  
  
  
  mtd.txt=paste0("A ",model.type," regression model with ",
                 text.list(x.var)," as ",c("a ","")[1+(length(x.var)>1)],
                 "predictor",c("","s")[1+(length(x.var)>1)]," of ",y.var,
                 " was fit to the data.  ")
  mtd.txt=c(mtd.txt,ass.mthd)
  
  rsq.txt=NULL
  if (model.type=="linear")
  {
    rsq.value=1-sum(residuals(model.result)^2)/sum((model.result$y-mean(model.result$y,na.rm=T))^2,na.rm=T)
    rsq.txt=paste0("This model empirically accounts for ",
                   round(100*rsq.value,2),"% of the variability in ",y.var,".  ")
  }
  
  res.txt=c(res.txt,rsq.txt,ass.txt)
  
  res=list(tbl=res.tbl,
           txt=res.txt,
           method=mtd.txt,
           ref=ref.txt)
  
  class(res)="SBP.result"
  
  return(res)
  
}


####################################################
# Create a table of coefficient estimates for a competing risk regression model


crr.tbl=function(crr.result)
  
{
  smry=summary(crr.result)
  HR.tbl=cbind.data.frame(term=rownames(smry$conf.int),
                          hazard.ratio=smry$conf.int[,1],
                          LB95=smry$conf.int[,3],
                          UB95=smry$conf.int[,4],
                          p=smry$coef[,5])
  return(HR.tbl)
}


##########################################
# Fit and evaluate a Cox

fit.cox=function(form,data)
  
{
  fit.res=coxph(form,data,y=T,x=T)
  zph.res=cox.zph(fit.res,terms=F,global=F)
  est.tbl=coef(summary(fit.res))
  ci.tbl=confint(fit.res)
  
  HR.tbl=cbind.data.frame(predictor=rownames(est.tbl),
                          HR=est.tbl[,2],
                          LB95=exp(ci.tbl[,1]),
                          UB95=exp(ci.tbl[,2]),
                          p.HR1=est.tbl[,5],
                          p.PHA=zph.res$table[,3])
  zph.gbl=cox.zph(fit.res,global=T)
  p.PHA.global=zph.gbl$table["GLOBAL",3]
  
  res=list(HR.tbl=HR.tbl,
           p.PHA.global=p.PHA.global)
  
  return(res)
}


#####################################
# Create a table of hazard ratio estimates for a coxph model

coxph.tbl=function(coxph.result)
  
{
  est.tbl=coef(summary(coxph.result))
  ci.tbl=confint(coxph.result)
  HR.tbl=cbind.data.frame(term=rownames(est.tbl),
                          HR=est.tbl[,2],
                          LB95=exp(ci.tbl[,1]),
                          UB95=exp(ci.tbl[,2]),
                          p=est.tbl[,5])
  rownames(HR.tbl)=rownames(est.tbl)
  
  print(HR.tbl)
  
  
  zph.res=cox.zph(coxph.result)
  zph.tbl=zph.res$table
  
  res=list(HR.tbl=HR.tbl,
           PH.tbl=zph.res)
  
  return(res)
  
}


#######################################
# Create a table of coefficient estimates for a glm

glm.tbl=function(glm.result)
  
{
  ci.tbl=confint(glm.result)
  smry.tbl=coef(summary(glm.result))
  res.tbl=cbind.data.frame(coefficient=smry.tbl[,1],
                           LB95=ci.tbl[,1],
                           UB95=ci.tbl[,2],
                           p=smry.tbl[,4])
  
  if ((glm.result$family$family=="gaussian")&&(glm.result$family$link=="identity"))
  {
    return(res.tbl)
  }
  
  if ((glm.result$family$family=="binomial")&&(glm.result$family$link=="logit"))
  {
    res.tbl[,1:3]=exp(res.tbl[,1:3])
    colnames(res.tbl)[1]="odds.ratio"
    return(res.tbl)
  }
  
}

###########################################
# Linear Regression on Ranks Model 
# Iman and Conover (1979) The use of the rank transform in regression.  Technometrics 21:499-509.

monotone.rank.model=function(x,y)
{
  na=is.na(x)|is.na(y)
  x=x[!na]
  y=y[!na]
  n=length(x)
  rx=rank(x)
  ry=rank(y)
  k=n*(n+1)^2/4
  b=(sum(rx*ry)-k)/(sum(rx^2)-k)
  a=(1-b)*(n+1)/2
  ry.hat=a+b*rx
  y.hat=ry.hat
  ord.y=order(y)
  y.hat[ord.y]=approx(ry[ord.y],y[ord.y],xout=ry.hat[ord.y])$y
  res=cbind.data.frame(x=x,y=y,y.hat=y.hat)
  return(res)
}