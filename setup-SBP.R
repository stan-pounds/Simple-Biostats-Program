
SBP.code.path="https://raw.githubusercontent.com/stan-pounds/Simple-Biostats-Program/main/"

# specify the file names
SBP.code.files=c("get-packages.R",
               "describe.R","estimate.R","correlate.R",
               "compare.R","model.R","colors.R",
               "print.R","figures.R","read-data.R",
               "outliers.R")

SBP.code.files=paste0(SBP.code.path,SBP.code.files)

for (i in 1:length(SBP.code.files))
  source(SBP.code.files[i])

get.package("DescTools")
get.package("survival")
get.package("cmprsk")
get.package("penalized")
get.package("riskRegression")
get.package("knitr")
get.package("data.table")
get.package("readxl")
get.package("utils")
get.package("coin")
