
get.data.set=function(data.set)
{
  dset.name=arg.as.char(data.set)
  print(dset.name)
  
  ls.res=ls(topenv())
  print(ls.res)
  mtch=grep(dset.name,ls.res)
  print(mtch)
  if (length(mtch)==1)
  {
    res=eval(parse(text=dset.name))
    return(res)
  }

}


###########################################
# Extract a column of data from a data set

get.y.clm=function(y,dset)
  
{
  dset.clms=colnames(dset)
  clms=colnames(dset)
  y.clm=arg.as.char(y)
  
  mtch=which(is.element(clms,y.clm))
  if (length(mtch)==1)
  {
    y=dset[,mtch]
    attr(y,"clm.name")=dset.clms[mtch]
    return(y)
  }
  
  clms=tolower(clms)
  if (!any(duplicated(clms)))
  {
    y.clm=tolower(y.clm)
    mtch=which(is.element(clms,y.clm))
    if (length(mtch)==1)
    {
      y=dset[,mtch]
      attr(y,"clm.name")=dset.clms[mtch]
      return(y)
    }
    
    mtch=agrep(y.clm,clms)
    if (length(mtch)==1)
    {
      y=dset[,mtch]
      attr(y,"clm.name")=dset.clms[mtch]
      return(y)
    }
  }
  
  
  stop(paste0("Column ",y.clm," not found in dset."))
  
}

#################################
# Represent input argument as a character

arg.as.char=function(x)
  
{
  temp=try(x,silent=T)
  
  if(class(temp)=="character") return(temp)
  
  if (class(temp)=="try-error")
  {
    mtch=regexpr(": object ",temp)
    temp=substring(temp,mtch)
    temp=gsub(": object ","",temp,fixed=T)
    temp=gsub(" not found\n","",temp,fixed=T)
    temp=gsub("'","",temp,fixed=T)
    temp=as.character(temp)
    return(temp)
  }
  
  temp=deparse(match.call())
  temp=get.arg(temp,"x")
  return(temp)

}