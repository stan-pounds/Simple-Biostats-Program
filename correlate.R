


correlate=function(form,data,
                   txt=1,tbl=1,fig=1,
                   y.name=NULL,
                   x.name=NULL,
                   clr="black",
                   line=1)

{
  data=data.frame(data)
  form.vars=get.vars(form)
  
  y=data[,form.vars$y.var]
  x=data[,form.vars$x.var]
  
  ry=rank(y)
  rx=rank(x)
  
  
  if (is.null(y.name)) y.name=form.vars$y.var
  if (is.null(x.name)) x.name=form.vars$x.var
  
  rr.fit=lm(ry~rx,x=T,y=T)
  
  lm.fit=lm(form,data)
  r=residuals(lm.fit)
  sw.res=shapiro.test(r)
  
  sw.sig=(sw.res$p.value<=0.05)
  
  sp.corr=cor.test(x,y,method="spearman",use="pairwise.complete.obs")
  pr.corr=cor.test(x,y,method="pearson",use="pairwise.complete.obs")
  
  corr.method=c("Pearson","Spearman")[1+sw.sig]
  corr.stat=c(pr.corr$estimate,sp.corr$estimate)[1+sw.sig]
  corr.pvalue=c(pr.corr$p.value,sp.corr$p.value)[1+sw.sig]
  corr.sig=(corr.pvalue<=0.05)
  
  if (fig>0)
  {
    scatter.plot(form,data,clr=clr,x.name=x.name,
                 y.name=y.name,line=line)
  }
  
  res.txt=NULL
  if (txt>0)
  {
    res.txt=paste0("The correlation of ",x.name," and ",
               y.name," is ",c("not","")[1+corr.sig],
               " statistically significant (",
               corr.method," r = ",round(corr.stat,3),
               "; p = ",corr.pvalue,").  ")
  }
  
  mtd.txt=paste0("To evaluate the association of ",x.name," and ",y.name,
                 ", a simple linear regression model with ",x.name," as the sole predictor of ",
                 y.name," was fit to the data.  ",
                 "The distribution of residuals of this model did",c(" not","")[1+sw.sig],
                 " differ significantly from the normal distribution ",
                 "(Shapiro-Wilk p = ",sw.res$p.value,").  ",
                 "Therefore, ",
                 corr.method,"'s correlation was used to evaluate the association of ",
                 x.name," and ",y.name,".  ")
  

  res=list(tbl=NULL,
           txt=res.txt,
           method=mtd.txt,
           ref=NULL)
  
  class(res)="SBP.result"

  
  return(res)
}