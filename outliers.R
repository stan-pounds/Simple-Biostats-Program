get.package("robustbase")

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