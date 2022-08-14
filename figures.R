

################################
# SBP box plot

box.plot=function(input,data,y.name=NULL,clr="rainbow")
  
{
  data=data.frame(data)
  if (is.character(input))
  {
    if (is.null(y.name)) y.name=input
    xlbl=y.name
    
    x=data[,input]
    
    clr=define.colors(1,clr)
    
    x.rng=range(x,na.rm=T)
    rng.diff=diff(x.rng)
    x.rng[1]=x.rng[1]-0.05*rng.diff
    x.rng[2]=x.rng[2]+0.05*rng.diff
    par.opts=par()
    par(mar=c(6,4,4,1))
    boxplot(x,horizontal=T,pch=19,col=clr[1],
            ylim=x.rng,
            cex.lab=1.5,las=1,cex.axis=1.5,
            xlab=xlbl,cex=as.numeric(length(x)>=100))
    par(mar=par.opts$mai)
  }
  
  if (class(input)=="formula")
  {
    form.vars=get.vars(input)
    y.clm=form.vars$y.var
    y=data[,y.clm]
    
    grp.clm=form.vars$x.var[1]
    
    if (is.null(y.name)) y.name=input
    xlbl=y.clm
    
    y=data[,y.clm]
    grp=data[,grp.clm]
    
    clrs=define.colors(length(unique(grp)),clr)
    par.opts=par()
    par(mar=c(6,12,4,1))
    boxplot(y~grp,horizontal=T,col=clrs,xlab=xlbl,ylab="",
            cex.lab=1.5,las=1,cex.axis=1.5)
    par(mar=par.opts$mar)
  }
}

###########################################
# SBP pie plot

pie.plot=function(y.clm,data,y.name=NULL,all=F,clr=NULL)
  
{
  data=data.frame(data)
  x=data[,y.clm]
  
  avl.tbl=table(x)
  all.tbl=table(x,exclude=NULL)
  res.tbl=avl.tbl
  if (all) res.tbl=all.tbl
  
  n.miss=sum(all.tbl)-sum(avl.tbl)
  
  sub.txt=paste0("excludes ",n.miss," missing observations")
  if (all&&(n.miss>0)) sub.txt="includes missing data as a distinct category"
  if (n.miss==0) sub.txt="no missing observations"
  
  if (is.null(y.name)) y.name=y.clm
  
  clrs=define.colors(length(res.tbl),clr)
  
  pct.tbl=100*res.tbl/sum(res.tbl)
  
  par.opts=par()
  par(mar=rep(6,4)+0.1)
  pie(pct.tbl,col=clrs,
      main=y.name,cex.main=1.5,
      labels=paste0(names(res.tbl),
                    "\n (n = ",res.tbl,
                    "; ",round(pct.tbl,2),"%)"),
      sub=sub.txt)
  par(mar=par.opts$mar)
}


###############################################
# SBP bar plot

bar.plot=function(y.clm,data,all=F,y.name=NULL,clr=NULL)
  
{
  data=data.frame(data)
  if (is.null(y.name)) y.name=y.clm
    
  x=data[,y.clm]
  
  
  if (class(x)[1]%in%c("ordered","factor","character"))
  {
    avl.tbl=table(x)
    all.tbl=table(x,exclude=NULL)
    res.tbl=avl.tbl
    if (all) res.tbl=all.tbl
    
    n.miss=sum(all.tbl)-sum(avl.tbl)
    
    sub.txt=paste0("excludes ",n.miss," missing observations")
    if (all&&(n.miss>0)) sub.txt="includes missing data as a distinct category"
    if (n.miss==0) sub.txt="no missing observations"
    
    clrs=define.colors(length(res.tbl),clr)
    pct.tbl=100*res.tbl/sum(res.tbl)
    par.opts=par()
    par(mar=c(6,12,4,8)+0.1)
    bp.res=barplot(res.tbl,horiz=T,cex.names=1.5,
                   xlab=paste0(y.name," (n)"),col=clrs,las=1,
                   cex.axis=1.25,cex.lab=1.5,xlim=c(0,1.3)*max(res.tbl),
                   sub=sub.txt)
    text(res.tbl,bp.res,paste0("n = ",res.tbl,"\n (",round(pct.tbl,2)," %)"),cex=1,pos=4)
    par(mar=par.opts$mar)
  }
  
  if (class(x)[1]%in%c("numeric","integer","double"))
  {
    if (is.null(clr)) clr="gray"
    clrs=define.colors(1,clr)
    hst=hist(x,plot=F)
    par.opts=par()
    par(mar=c(6,6,2,1))
    hst=hist(x,col=clrs[1],cex.lab=1.5,prob=F,main="",cex.axis=1.5,
             xlab=y.name,las=1,ylim=c(0,1.1)*max(hst$counts),ylab="Number")
    text(hst$mids,hst$counts,hst$counts,pos=3)
    par(mar=par.opts$mar)
  }
}

##########################################################
# normal quantile-quantile plot

nqq.plot=function(y.clm,data,y.name=NULL,clr="black")
  
{
  data=data.frame(data)
  if (is.null(y.name)) y.name=y.clm
  x=data[,y.clm]
  clrs=define.colors(1,clr)
  par.opts=par()
  par(mar=c(6,6,2,1))
  qqnorm(x,xlab="Standard Normal Quantile",main="",
         ylab=paste0("Actual Value of ",y.name),
         cex.lab=1.5,cex.axis=1.5)
  qqline(x)
  par(mar=par.opts$mar)

}

#########################################################
# Event plot

event.plot=function(input,data,y.name=NULL,clr=NULL)
  
{
  data=data.frame(data)
  if (class(input)=="character")
  {
    x=data[,input]
    cls=class(x)
    if (!(cls%in%c("Surv","competing.events")))
      stop("x must be of class Surv or competing.events.")
    
    
    # if nmx not provided then extract it from the input
    if (is.null(y.name)) y.name=input
    
    # Kaplan-Meier curves
    if(cls=="Surv")
    {
      clr=define.colors(1,clr)
      km=survfit(x~1)
      
      ylbl=paste0("Pr(",y.name,")")
      plot(km,las=1,conf.int=F,mark.time=T,lwd=2,
           cex.axis=1.5,cex.lab=1.5,ylab=ylbl,
           xlab="Time",col=clr)
    }
    
    if (cls=="competing.events")
    {
      evnt.types=unique(x[,2])
      evnt.types=evnt.types[evnt.types!=0]
      clrs=define.colors(length(evnt.types),clr)
      ci=cuminc(x[,1],x[,2])
      
      # label the output
      names(ci)=substring(names(ci),3)
      ev.key=attr(x,"ev.key")
      for (i in 1:length(ev.key))
        names(ci)=gsub(names(ev.key)[i],ev.key[i],names(ci),fixed=T)
      
      plot(ci,las=1,col=clrs,lty=1,lwd=2,
           xlab="Time",ylab="Cumulative Incidence",
           cex.lab=1.5,cex.axis=1.5)  
    }
  }
  
  if (class(input)=="formula")
  {
    form.vars=get.vars(input)
    y.clm=form.vars$y.var
    y=data[,y.clm]
    
    if (class(y)=="Surv")
    {
      grp.clm=form.vars$x.var
      grp=data[,grp.clm]
      
      if (class(grp)[1]%in%c("numeric","integer"))
      {
        grp=cut.quantile(grp,3)
        Rcode=paste0('data[,"',grp.clm,'"]=grp')
        eval(parse(text=Rcode))
      }
      
      sfit=survfit(input,data=data)
      stbl=summary(sfit,times=pretty(c(0,max(y[,1]))))
      n.grp=length(levels(stbl$strata))
      clrs=define.colors(n.grp,clr)
      xlim=c(0,1+0.3*(length(sfit$strata)>1))*max(y[,1],na.rm=T)
      par.opts=par()
      par(mar=c(6,6,1,1))
      plot(sfit,col=clrs,lwd=2,las=1,
           xlab=paste0("Time"),
           ylab=y.clm,xlim=xlim,
           cex.axis=1.5,cex.lab=1.5)
      if (length(sfit$strata)>1)
      {
        legend(1.05*max(y[,1],na.rm=T),1,
               col=clrs,lty=1,lwd=2,
               names(sfit$strata),ncol=1,
               cex=0.60)
        par(mar=par.opts$mar)
      }
    }
    
    if (class(y)=="competing.events")
    {
      grp.clm=form.vars$x.var
      grp=data[,grp.clm]
      
      if (class(grp)[1]%in%c("numeric","integer"))
      {
        grp=cut.quantile(grp,3)
      }
      
      ev.key=attr(y,"ev.key")
      evnt=ev.key[as.character(y[,2])]
      ci.res=cuminc(y[,1],evnt,grp,cencode=ev.key[as.character(0)])
      
      res.tbl=timepoints(ci.res,times=pretty(c(0,max(y[,1]))))$est
      res.tbl=t(res.tbl)
      
      res.tbl2=ci.res$Tests
      
      crv.names=colnames(res.tbl)
      grp.names=as.character(sort(unique(grp)))
      evt.names=as.character(sort(as.numeric(ev.key[setdiff(names(ev.key),as.character(0))])))
      

      crv.evts=crv.names
      crv.grps=crv.names
      
      for (i in 1:length(grp.names))
      {
        grp.mtch=which(substring(crv.names,1,nchar(grp.names[i]))==grp.names[i])
        crv.grps[grp.mtch]=grp.names[i]
      }
      
      for (i in 1:length(evt.names))
      {
        evt.mtch=which(substring(crv.names,nchar(crv.names)-nchar(evt.names[i])+1)==evt.names[i])
        crv.evts[evt.mtch]=evt.names[i]
      }
      

      
      n.grps=length(unique(crv.grps))

      clrs=define.colors(n.grps,clr)
      names(clrs)=unique(crv.grps)

      ci.clrs=clrs[crv.grps]

      
      n.types=length(unique(crv.evts))
      lty=1:n.types
      names(lty)=unique(crv.evts)

      ci.lty=lty[crv.evts]

      
      
      plot(ci.res,col=ci.clrs,lty=ci.lty,las=1,lwd=2,
           xlab="Time",ylab="Cumulative Incidence",
           cex.axis=1.5,cex.lab=1.5)
    }
    

  }
}

################################################
# 

scatter.plot=function(form,data,
                      clr="black",
                      x.name=NULL,
                      y.name=NULL,
                      line=NA)
  
{
  data=data.frame(data)
  form.vars=get.vars(form)
  y.clm=form.vars$y.var
  x.clm=form.vars$x.var
  y=data[,y.clm]
  x=data[,x.clm]
  
  
  if(is.null(x.name)) x.name=x.clm
  if(is.null(y.name)) y.name=y.clm
  
  lm.fit=lm(y~x)
  r=residuals(lm.fit)
  sw.res=shapiro.test(r)
  sw.sig=(sw.res$p.value<=0.05)
  
  ry=rank(y)
  rx=rank(x)
  rr.fit=lm(ry~rx,x=T,y=T)
  
  sp.corr=cor.test(x,y,method="spearman",use="pairwise.complete.obs")
  pr.corr=cor.test(x,y,method="pearson",use="pairwise.complete.obs")

  corr.method=c("Pearson","Spearman")[1+sw.sig]
  corr.stat=c(pr.corr$estimate,sp.corr$estimate)[1+sw.sig]
  corr.pvalue=c(pr.corr$p.value,sp.corr$p.value)[1+sw.sig]

  
  sub.txt=""
  
  if (line%in%1)
  sub.txt=paste0(corr.method," r = ",round(corr.stat,3),
                 "; p = ",corr.pvalue)
  
  clrs=define.colors(1,clr)
  plot(x,y,xlab=x.name,ylab=y.name,
       main="",cex.axis=1.5,cex.lab=1.5,
       col=clrs,sub=sub.txt)
  
  if (line%in%1)
  {

    if (!sw.sig) abline(lm.fit,col=clrs)
    if (sw.sig)
    {
      y.hat=approx(ry,y,xout=rr.fit$fitted.values)$y
      x.plt=approx(rx,x,xout=rr.fit$x[,"rx"])$y
      ord=order(x.plt)
      lines(x.plt[ord],y.hat[ord],col=clrs)
    }
    
  }
  
  if (line%in%0)
  {
    y.mn=mean(y)
    abline(y.mn,0,col=clrs)
  }
  
}

############################################
# mosaic plot

mosaic.plot=function(form,data,clr="rainbow",
                     y.name=NULL,grp.name=NULL)
  
{
  data=data.frame(data)
  form.vars=get.vars(form)
  cty.clm=form.vars$y.var
  grp.clm=form.vars$x.var[1]
  
  cty=data[,cty.clm]
  grp=data[,grp.clm]
  
  full.tbl=table(grp,cty,exclude=NULL)
  avl.tbl=table(grp,cty)
  
  nm.cty=cty.clm
  nm.grp=grp.clm
  
  if (is.null(y.name)) y.name=nm.cty
  if (is.null(grp.name)) grp.name=nm.grp
    
  nm.cty=y.name
  nm.grp=grp.name
  
  names(dimnames(full.tbl))=c(nm.grp,nm.cty)
  names(dimnames(avl.tbl))=c(nm.grp,nm.cty)
  
  uniq.cty=unique(cty)
  clrs=define.colors(length(uniq.cty),clr)
  
  n.full=sum(full.tbl)
  n.avl=sum(avl.tbl)
  
  if (n.full==n.avl)
  {
    mosaicplot(full.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="No Missing Data")
  } else {
    
    mosaicplot(full.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="Includes Missing Data as a Distinct Category")
    mosaicplot(avl.tbl,col=clrs,las=1,xlab=nm.grp,ylab=nm.cty,main="",
               cex.axis=1,
               sub="Excludes Missing Data")
  }
  

}


###########################################
# categorize a numeric variable by quantile
  
cut.quantile=function(x,n=4)
  
{
  qntl=quantile(x,(1:(n-1))/n,na.rm=T)
  qntl=c(-Inf,qntl,Inf)
  grp=cut(x,qntl)
  levels(grp)=paste0("Ranked subgroup ",1:n," of ",n)
  return(grp)
  
}

#############################################################
# 