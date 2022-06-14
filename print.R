print.SBP.result=function(sbp.result,knt=T,tbl=T,txt=T,method=T,ref=T)
  
{
  if ((!is.null(sbp.result$tbl))&&(tbl))
  {
    write(" **TABLES** \n ",file="")
    tbl.res=sbp.result$tbl
    if (is.data.frame(tbl.res)) tbl.res=list(tbl=sbp.result$tbl)
    if (!is.list(tbl.res)) tbl.res=list(tbl=sbp.result$tbl)
    n.tbl=length(tbl.res)
    for (i in 1:n.tbl)
    {
      if (!is.null(tbl.res[[i]]))
      {
        if (knt) 
        {
          res.name=NA
          if (is.numeric(tbl.res[[i]])&&(!is.null(attr(sbp.result,"result.name"))))
            res.name=attr(sbp.result,"result.name")
          write(kable(tbl.res[[i]],col.names=res.name),file="")
        }
          
        else print(tbl.res[[i]])
        write("\n ",file="")
      }
    }
  }


  if ((!is.null(sbp.result$txt))&&(txt))
  {
    write("\n **RESULTS** \n ",file="")
    
    write(paste(sbp.result$txt,collapse=""),file="")
  }

  if ((!is.null(sbp.result$method))&&(method))
  {
    write("\n \n **METHODS** \n ",file="")
    write(paste(sbp.result$method,collapse=""),file="")
  }

  
  if ((!is.null(sbp.result$ref))&&(ref))
  {
    write("\n \n **REFERENCES** \n ",file="")
    write(paste(sbp.result$ref,collapse="\n \n"),file="")
  }

}

word.table=function(SBP.result)
  
{
  if (!is.null(SBP.result$tbl))
  {
    tbl.res=SBP.result$tbl
    if (is.data.frame(tbl.res)) tbl.res=list(tbl=SBP.result$tbl)
    if (!is.list(tbl.res)) tbl.res=list(tbl=SBP.result$tbl)
    n.tbl=length(tbl.res)
    for (i in 1:n.tbl)
    {
      if (!is.null(tbl.res[[i]]))
      {
        res.name=NA
          if (is.numeric(tbl.res[[i]])&&(!is.null(attr(SBP.result,"result.name"))))
            res.name=attr(SBP.result,"result.name")
          write.table(tbl.res[[i]],col.names=res.name,quote=F,file="",sep=",")
      }
      write("\n \n",file="")
    }
    write("**INSTRUCTIONS**",file="")
    write("For each section of output above:")
    write("1. Copy the output into Word.",file="")
    write("2. Highlight the output in Word.",file="")
    write("3. Go to Insert>Table>Convert Text to Table.",file="")
  }
  
}

SBP.example.Rmd.slides=function(SBP.code.string,slide.title)
  
{
  write(paste0("## ",slide.title,"\n "),file="")
  write("```{r,results='hide'}",file="")
  write(SBP.code.string,file="")
  write("``` \n",file="")
  
  eq.pos=regexpr("=",SBP.code.string)
  res.name=substring(SBP.code.string,1,eq.pos-1)
  
  write(paste0("## ",slide.title,"\n "),file="")
  write("```{r,eval=F,echo=T}",file="")
  write(res.name,file="")
  write("``` \n",file="")  
  
  write("```{r,eval=T,echo=F}",file="")
  write(paste0("print.SBP.result(",res.name,",method=F,ref=F)"),file="")
  write("``` \n",file="")  
  
  write(paste0("## ",slide.title,"\n "),file="")

  write("```{r,eval=T,echo=F}",file="")
  write(paste0("print.SBP.result(",res.name,",txt=F,tbl=F)"),file="")
  write("``` \n",file="")  
  
  write(paste0("## ",slide.title,"\n "),file="")
  
  write("```{r,eval=T,echo=F,results='asis'}",file="")
  write(paste0("print.SBP.result(",res.name,",method=F,ref=F)"),file="")
  write("``` \n",file="")  
  
  write(paste0("## ",slide.title,"\n "),file="")
  
  write("```{r,eval=T,echo=F,results='asis'}",file="")
  write(paste0("print.SBP.result(",res.name,",txt=F,tbl=F)"),file="")
  write("``` \n",file="")  
  
}