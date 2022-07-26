resample=function(input,data.set,
                  r=NULL,b=10000,
                  fig=2,txt=0,tbl=0,
                  clr=c("royalblue","red"))
  
{
  if (is.null(r)) r=nrow(data.set)
  
  desc.result=describe(input,data.set,fig=0,txt=0,tbl=1)
  n=nrow(data.set)
  indx=replicate(b,sample(n,r,T))
  tbls=vector("list",b)
  y=data.set[,input]
  clrs=define.colors(2,clr)
  
  if (is.character(input))
  {
    for (i in 1:b)
    {
      temp.data=data.set[indx[,i],]
      temp.result=describe(input,temp.data,
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
        hst=hist(rs.tbls[,"mean"],plot=F)
        x.nml=seq(from=min(hst$breaks),to=max(hst$breaks),
                  length=101)
        y.nml=dnorm(x.nml,desc.result$tbl["mean"],
                    sd(rs.tbls[,"mean"]))

        hist(rs.tbls[,"mean"],freq=F,
             xlab=paste0("Mean ",input," of ",
                             r," Subjects"),
             ylab="",
             main=paste0("Cohorts of ",r," Subjects"),
             las=1,cex.axis=1.25,cex.lab=1.25,
             col=clrs[1],ylim=c(0,max(y.nml,hst$density)))
        lines(x.nml,y.nml,col=clrs[2],lwd=2)
      }
    }
    
    if (class(y)%in%c("character","factor","ordered"))
    {
      desc.tbl=desc.result$tbl
      rownames(desc.tbl)=desc.tbl[,1]
      stat.names=desc.tbl[,1]
      rs.tbls=matrix(NA,b,length(stat.names))
      colnames(rs.tbls)=stat.names
      for (i in 1:b)
      {
        temp.stat=tbls[[i]][,"percent"]
        names(temp.stat)=tbls[[i]][,1]
        rs.tbls[i,names(temp.stat)]=temp.stat
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

      
    }


}