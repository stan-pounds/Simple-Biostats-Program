
get.y.clm=function(y,dset)
  
{
  clms=colnames(dset)
  y.clm=arg.as.char(y)
  mtch=which(is.element(clms,y.clm))
  if (length(mtch)==1)
  {
    y=dset[,mtch]
    return(y)
  }
  stop(paste0("Column ",y.clm," not found in dset."))
  
}

arg.as.char=function(x)
  
{
  temp=try(is.character(x),silent=T)
  if (class(temp)!="try-error")
  {
    return(x)
  }

  mtch=regexpr(": object ",temp)
  temp=substring(temp,mtch)
  temp=gsub(": object ","",temp,fixed=T)
  temp=gsub(" not found\n","",temp,fixed=T)
  temp=gsub("'","",temp,fixed=T)
  temp=as.character(temp[1])
  
  return(temp)
}