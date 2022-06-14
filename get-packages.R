#####################################
# get an R package and make it available for use

get.package=function(package.name)
  
{
  pack.list=installed.packages()
  if(!(package.name%in%pack.list))
  {
    res=try(install.packages(package.name),silent=T)
    if (class(res)=="try-error")
      stop(paste0("CRAN has no package named '",package.name,"'.  No package installed."))
  }
  library(as.character(package.name),character.only=T)
}

get.package("lawstat")
get.package("survival")
get.package("cmprsk")
get.package("Rfit")