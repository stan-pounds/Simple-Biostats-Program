#################################
# Define colors

define.colors=function(n,
                       clr.palette="rainbow")
{
  
  
  if (is.null(clr.palette))
    clr.palette="rainbow"
  
  clr.funcs=c("rainbow","heat.colors",
              "terrain.colors","topo.colors",
              "cm.colors")
  
  clr.names=colors()
  if (all(clr.palette%in%clr.names))
  {
    if (length(clr.names)>=n)
    {
      res=clr.palette[1:n]
      return(res)
    }
  }
  
  if (all(areColors(clr.palette)))
  {
    if (length(clr.names)>=n)
    {
      res=clr.palette[1:n]
      return(res)
    }
  }
  
  if (clr.palette%in%clr.funcs)
  {
    n.str=max(n,2)
    clr.code=paste0(clr.palette,"(",n,")")
    res=eval(parse(text=clr.code))
    return(res)
  }
  
  clr.palettes=hcl.pals()
  if (clr.palette%in%clr.palettes)
  {
    res=hcl.colors(max(n,2),palette=clr.palette)
    if (n<=1) res=res[1]
    return(res)
  }
  
  stop("clr.palette must be a vector of color names, the name of a color function, or the name of a color palette: see help(hcl.colors).")
  
}

###################################
# show color palettes

show.palettes=function(n.colors=8)
{
  plot(c(0,15),c(0,-25),type="n",axes=F,
       xlab="",ylab="",
       main="Color Palettes in R")
  
  available.palletes=c("rainbow",
                       "heat.colors",
                       "terrain.colors",
                       "topo.colors",
                       "cm.colors",
                       hcl.pals())
  
  for (i in 1:length(available.palletes))
  {
    x=3*((i-1)%/%24)
    y=-((i-1)%%24)
    
    
    rect(x+(0:(n.colors-1))/n.colors,y-0.2,
         x+(1:n.colors)/n.colors,y-0.8,
         col=define.colors(n.colors,available.palletes[i]))
    text(x+1,y-0.5,available.palletes[i],pos=4,cex=0.75)
  }
  
}


######################################
# show colors

show.colors=function()
  
{
  clrs=colors()
  
  for (i in 0:9)
    clrs=gsub(i,"",clrs)
  
  clrs=unique(clrs)
  
  plot(c(0,30),c(0,-25),type="n",axes=F,
       xlab="",ylab="",
       main="Named Colors in R")
  
  for (i in 1:length(clrs))
  {
    x=5*(i-1)%/%24
    y=-((i-1)%%24)
    
    rect(x,y-0.2,x+0.25,y-0.8,
         col=clrs[i])
    text(x+0.25,y-0.5,clrs[i],cex=0.70,pos=4)
    
    
  }
}

areColors <- function(x) 
  {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}