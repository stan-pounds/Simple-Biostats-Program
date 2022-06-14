


model=function(form,data,
               tbl=1,fig=1,txt=1,
               clr="rainbow")
  
{
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  y=data[,y.clm]
  
  if (class(y)%in%c("competing.events","Surv"))
  {
    res=model.events(form,data,tbl,fig,txt,clr)
    return(res)
  }
  
  if (class(y)%in%c("numeric"))
  {
    y.binary=all(is.element(y,c(0:1,NA)))
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
  
  if (class(y)=="Surv")
  {
    cox.res=coxph(form,data,y=T)
    res=coxph.txt(cox.res)
    y.hat=cox.res$linear.predictors
    
    
    if (fig>0)
    {
      grp=cut(y.hat,c(-Inf,quantile(y.hat,(1:2)/3,na.rm=T),Inf))
      srv=cox.res$y
      
      sfit=survfit(srv~grp)
      clrs=define.colors(3,clr)
      plot(sfit,main=y.clm,
           xlim=c(0,1.25)*max(y[,1],na.rm=T),lwd=2,
           col=clrs,las=1,xlab="Time",ylab="Prob")
      legend(1.05*max(y[,1],na.rm=T),1,lwd=2,col=clrs,
             legend=paste0("model ",c("low","int","high")," risk"),
             cex=0.75)
      
    }
    
    return(res)
  }
  

  if (class(y)=="competing.events")
  {
    mdl.frm=model.frame(form,data,na.action=na.omit)
    form.str=deparse(form)
    tilde.pos=regexpr("~",form.str,fixed=T)
    xvar.str=substring(form.str,tilde.pos)
    xvar.form=eval(parse(text=xvar.str))
    mdl.mtx=model.matrix(xvar.form,data=mdl.frm)
    print(head(mdl.mtx))
    x.clms=setdiff(colnames(mdl.mtx),"(Intercept)")
    x.mtx=matrix(mdl.mtx[,x.clms],ncol=length(x.clms))
    colnames(x.mtx)=x.clms
    crr.res=crr(as.numeric(mdl.frm[,y.clm][,1]),
                mdl.frm[,y.clm][,2],
                x.mtx)
    tbl.crr=crr.tbl(crr.res)
    
    fnl.tbl=list(tbl.crr=tbl.crr)
    
    
    if (fig>0)
    {
      ev.key=attr(y,"ev.key")
      y.hat=predict(crr.res,x.mtx)
      y.hat.bar=colMeans(y.hat[,-1])
      grp=cut(y.hat.bar,c(-Inf,quantile(y.hat.bar,(1:2)/3,na.rm=T),Inf))
      grp=c("low","int","high")[as.numeric(grp)]
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
      
      
      plot(ci.res,col=ci.clrs,lty=ci.lty,las=1,xlab="Time")
      
      fnl.tbl=list(tbl.crr=tbl.crr,
                   tbl.cminc=res.tbl)
    }
    
    
    
    res=list(tbl=fnl.tbl,
             txt="")
    
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
  r.sw=shapiro.test(r)
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
                       clr="rainbow")
  
{
  glm.res=glm(formula=form,data=data)
  res.tbl=glm.tbl(glm.res)
  
  r=residuals(glm.res)
  r.sw=shapiro.test(r)
  y.hat=predict(glm.res)
  y.obs=glm.res$y
  
  if (fig>1)
  {
    plot(y.hat,y.obs,
         xlab="Model Prediction",
         ylab="Observed Value")
    abline(0,1)
  }

  if (fig>2)
  {
    plot(y.hat,r,
         xlab="Model Prediction",
         ylab="Model Error (Observed-Prediction)")
    abline(0,0)
  }

  if (fig>3)
  {
    qqnorm(r,xlab="Standard Normal Quantile",
           ylab="Quantile of Model Error",
           sub=paste0("Shapiro-Wilk p = ",r.sw$p.value))
    qqline(r)
  }

  
  
  res=list(tbl=res.tbl,
           txt="")
  
  return(res)
  
}


###############################################
# Generate narrative interpretation of a Cox model fit

coxph.txt=function(coxph.result)
  
{

  cox.res=coxph.result
  cox.tbl=coxph.tbl(cox.res)
  res.terms=cox.res$terms
  cox.terms=as.character(cox.res$terms)
  y.cox=cox.terms[2]
  x.cox=cox.terms[3]
  x.cox=unlist(strsplit(x.cox,split=" + ",fixed=T))
  
  res.txt=paste0("A Cox (1972) proportional hazards regression model was fit with ",
                 text.list(x.cox)," as ",
                 c("a predictor","predictors")[1+(length(x.cox)>1)]," of ",y.cox,".")
  n.asgn=length(cox.res$assign)
  for (i in 1:n.asgn)
  {
    fctr=is.element(names(cox.res$assign)[i],
                    names(cox.res$xlevels))
    fctr.name=names(cox.res$assign)[i]
    if (fctr)
    {
      
      fctr.lvls=cox.res$xlevels[[fctr.name]]
      for (j in 2:length(fctr.lvls))
      {
        rw.nm=paste0(fctr.name,fctr.lvls[j])
        term.txt=paste0("The fitted model estimates that ",
                        fctr.name," group ",fctr.lvls[j]," subjects experience events ",
                        "at a rate ",cox.tbl$HR.tbl[rw.nm,"HR"]," times that of ",
                        fctr.name," group ",fctr.lvls[1]," subjects ",
                        "(95% CI: ",cox.tbl$HR.tbl[rw.nm,"LB95"],
                        ", ",cox.tbl$HR.tbl[rw.nm,"UB95"],"; p = ",
                        cox.tbl$HR.tbl[rw.nm,"p"],").  ")
        res.txt=c(res.txt,term.txt)
      }
      
    } else
    {
      term.txt=paste0("The fitted model estimates that each one-unit increase in ",
                      fctr.name," is associated with a change in the rate of events ",
                      "by a factor of ",cox.tbl$HR.tbl[fctr.name,"HR"],
                      " (95% CI: ",cox.tbl$HR.tbl[fctr.name,"LB95"],", ",
                      cox.tbl$HR.tbl[fctr.name,"UB95"],"; p = ",
                      cox.tbl$HR.tbl[fctr.name,"p"],").  ")
      res.txt=c(res.txt,term.txt)
    }
    
  }
  
  for (i in 1:(nrow(cox.tbl$PH.tbl)-1))
  {
    term.txt=paste0("There is ",c("","not")[1+(cox.tbl$PH.tbl[i,"p"]>0.05)],
                    " statistically compelling evidence that the mathematical form ",
                    "of the model poorly represents the association of ",
                    rownames(cox.tbl$PH.tbl)[i]," with ",y.cox," (p = ",
                    cox.tbl$PH.tbl[i,"p"],").  ")
    res.txt=c(res.txt,term.txt)
  }
  
  term.txt=paste0("There is ",c("","not")[1+(cox.tbl$PH.tbl["GLOBAL","p"]>0.05)],
                  " statistically compelling evidence that the mathematical form of ",
                  "the model does not accurately represent the overall association of ",
                  text.list(x.cox)," with ",y.cox," (p = ",cox.tbl$PH.tbl["GLOBAL","p"],").  ")
  res.txt=c(res.txt,term.txt)
  
  
  method=paste0("A Cox (1972) proportional hazards regression model was fit with ",
                text.list(x.cox)," as ",
                c("a predictor","predictors")[1+(length(x.cox)>1)]," of ",y.cox,".")
  
  ref='Cox, David R (1972). "Regression Models and Life-Tables". Journal of the Royal Statistical Society, Series B. 34 (2): 187-220. JSTOR 2985181. MR 0341758.'
  
  res=list(tbl=cox.tbl,
           txt=res.txt,
           method=method,
           ref=ref)
  
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