resample=function(input,data.set,
                  r=NULL,b=10000,
                  fig=2,txt=0,tbl=0,
                  clr=c("royalblue","red"))
  
{
  
  clm.name=try(input,silent=T)
  if (class(clm.name)=="try-error")
  {
    temp=deparse(match.call())
    clm.name=get.arg(temp,"input")
  }

  data.set=data.frame(data.set)
  if (is.null(r)) r=nrow(data.set)
  
  y=get.y.clm(clm.name,data.set)
  y.name=attr(y,"clm.name")

  desc.result=describe(y.name,data.set,fig=0,txt=0,tbl=1)
  n=nrow(data.set)
  indx=replicate(b,sample(n,r,T))
  tbls=vector("list",b)

  clrs=define.colors(2,clr)
  
  for (i in 1:b)
  {
    temp.data=data.set[indx[,i],]
    temp.result=describe(y.name,temp.data,
                         tbl=1,fig=0,txt=0)
    tbls[[i]]=temp.result$tbl
  }

    if (class(y)%in%c("numeric","double","integer"))
    {
      stat.names=c("mean")
      stat.values=desc.result$tbl[stat.names]
      rs.tbls=matrix(NA,b,length(stat.names))
      colnames(rs.tbls)=stat.names
      for (i in 1:b) 
        rs.tbls[i,stat.names]=unlist(tbls[[i]])[stat.names]
      

      
      if (fig>0)
      {
        par(mar=rep(4,4))
        hst=hist(rs.tbls[,"mean"],plot=F)
        x.nml=seq(from=min(hst$breaks),to=max(hst$breaks),
                  length=101)
        y.nml=dnorm(x.nml,desc.result$tbl["mean"],
                    sd(rs.tbls[,"mean"]))

        hist(rs.tbls[,"mean"],freq=F,
             xlab=paste0("Mean ",y.name," of ",
                             r," Subjects"),
             ylab="",
             main=paste0("Cohorts of ",r," Subjects"),
             las=1,cex.axis=1.25,cex.lab=1.25,
             col=clrs[1],ylim=c(0,max(y.nml,hst$density,na.rm=T)))
        lines(x.nml,y.nml,col=clrs[2],lwd=2)
      }
    }
    
    if (class(y)%in%c("character","factor","ordered"))
    {
      desc.tbl=desc.result$tbl
      rw.names=desc.tbl[,1]
      ok.rows=!is.element(rw.names,c("",NA,"NA"))
      rw.names=rw.names[ok.rows]
      desc.tbl=desc.tbl[ok.rows,]
      
      rownames(desc.tbl)=rw.names
      stat.names=desc.tbl[,1]
      rs.tbls=matrix(NA,b,length(stat.names))
      colnames(rs.tbls)=stat.names
      for (i in 1:b)
      {
        temp.stat=tbls[[i]][,"percent"]
        names(temp.stat)=tbls[[i]][,1]
        clm.names=intersect(names(temp.stat),stat.names)
        rs.tbls[i,clm.names]=temp.stat[clm.names]
      }
      
      if (fig>0)
      {
        for (i in 1:ncol(rs.tbls))
        {
          x.min=floor(min(r*rs.tbls[,stat.names[i]]/100))
          x.max=ceiling(max(r*rs.tbls[,stat.names[i]]/100))
          x=x.min:x.max

          brks=seq(from=x.min-0.5,to=x.max+0.5,by=1)

          hst=hist(rs.tbls[,stat.names[i]],plot=F,breaks=100*brks/r)

          y=dbinom(x,r,desc.tbl[stat.names[i],"percent"]/100)/(100/r)

          
          par(mar=rep(4,4))
          hst=hist(rs.tbls[,stat.names[i]],freq=F,
                   breaks=100*brks/r,
                   xlab=paste0("% ",stat.names[i]," among ",
                               r," Subjects"),
                   ylab="",
                   main=paste0("% ",stat.names[i]," in ",
                               "cohorts of ",r," subjects"),
                   cex.axis=1,cex.lab=1.25,las=1,
                   col=clrs[1],ylim=c(0,max(y,hst$density))) 

          segments(100*(x-0.5)/r,y,
                   100*(x+0.5)/r,y,col=clrs[2],
                   lwd=3)
          }
          
        }

        
      }

  res.txt=NULL
  res.tbl=NULL
  res.method=NULL
  res.ref=NULL
  
  if (tbl>0) res.tbl=rs.tbls
  res=list(txt=res.txt,
           tbl=res.tbl,
           method=res.method,
           ref=res.ref)
  
  class(res)="SBP.result"
  return(res)
}