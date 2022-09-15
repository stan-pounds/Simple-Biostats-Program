#################################################
# get a data set


get.data.set=function(data.set)
{
  temp=deparse(match.call())
  dset.name=get.arg(temp,"data.set")
  ls.res0=ls(topenv())
  ls.res=ls.res

  mtch=grep(dset.name,ls.res)
  if (length(mtch)==1)
  {
    res=eval(parse(text=dset.name))
    res=data.frame(res)
    return(res)
  }
  
  print(dset.name)
  mtch=agrep(dset.name,ls.res)
  if (length(mtch)==1)
  {
    print(ls.res0[mtch])
    res=eval(parse(text=ls.res0[mtch]))
    res=data.frame(res)
    return(res)
  }
  
  stop(paste0("Unable to find data.set ",data.set,".  "))

}


###########################################
# Extract a column of data from a data set

get.y.clm=function(y,dset)
  
{
  y.clm=arg.as.char(y)
  dset.clms=colnames(dset)
  clms=colnames(dset)
  
  
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
  if (class(temp)=="character")
    return(temp)
  
  temp=deparse(match.call())
  res=get.arg(temp,"x")
  return(res)
}

#######################################
# get the argument value


get.arg=function(call.string, # obtain with deparse(match.call())
                 arg.string)  # character string with argument name
{
  
  x.pos=regexpr(paste0(arg.string," = "),call.string,fixed=T)
  x.name=substring(call.string,x.pos+nchar(arg.string)+3)
  x.name=x.name[x.pos>0]
  
  
  comma.pos=regexpr(",",x.name,fixed=T)
  close.pos=regexpr(")",x.name,fixed=T)
  end.pos=close.pos
  
  if (length(comma.pos)>0)
  {
    if (comma.pos>0) end.pos=comma.pos
  }
  
  x.name=substring(x.name,1,end.pos-1)
  
  
  x.name=gsub('\"','',x.name)
  
  return(x.name)
}


###################################
# Normality test
# perform shapiro.test if n < 3000
# otherwise perform ks.test

normality.test=function(x)
{
  na=is.na(x)
  n=length(x)
  
  if (n<3000)
  {
    res=shapiro.test(x)
    return(res)
  }
  
  res=ks.test(x,"pnorm")
  return(res)
}


#################################
# fresh start
# clean out R memory and start over

fresh.start=function()
{
  rm(list=ls())
  SBP.code.path="https://raw.githubusercontent.com/stan-pounds/Simple-Biostats-Program/main/"
  source(paste0(SBP.code.path,"setup-SBP.R"))
}