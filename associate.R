##################################
# Written by Hasan Masrique ()

associate <- function(form, data, txt= 1, tbl= 1, fig= 1, y.name= NULL, x.name= NULL, clr= NULL, line= 1) {
  
  ### get the variables from formula
  
  if(class(data) != "data.frame"){
    data <- data.frame(data) 
  }
  
  form_vars <- get.vars(form)
  
  ### get variable in y
  depend_y <- form_vars$y.var
  y <- data[, depend_y]

  ### get variable in x
  independ_x <- form_vars$x.var
  x <- data[, independ_x]
  
  ## if both x and y variable are numerical, then use correlate()
  
  if ((class(x)[1] %in% c("numeric", "double", "integer")) &&
      (class(y)[1] %in% c("numeric", "double", "integer"))) {
    
    #print("use correlate method")
    res <- correlate(form, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, x.name= NULL, clr= "red", line= line)
    class(res) <- "SBP.result"
    return (res) 
  }  
    ## if both of the variables are not numerical, use compare() 
    else {  
    
    #print("use compare method")  
    res <- compare(form, data, txt= txt, tbl= tbl, fig= fig, y.name= NULL, grp.name= NULL, clr= "rainbow")
    class(res) <- "SBP.result" 
    return (res)
      
  } 
  
}



