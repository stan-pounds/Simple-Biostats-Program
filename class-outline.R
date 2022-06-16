
#####################################
# set up the intro-biostat system

setup.file="C:/Users/spounds/Box/Biostat-Teaching/Intro-Biostats/Code/ezR4students/2022-06-06/SJGS-intro-biostat-Rsetup.R"
source(setup.file)

get.package("penalized")
help(nki70)
data(nki70)

# Describe variables
describe("ER",nki70)
describe("Grade",nki70)

describe("ER",nki70,fig=3)
describe("Grade",nki70,fig=3)


describe("Age",nki70)
describe("Age",nki70,fig=3)


# Binomal Confidence Intervals
estimate("ER",nki70)

# One Sample t-test
estimate("FLT1",nki70)

# Signed-Rank Test
estimate("ESM1",nki70)

# Sign Test
estimate("Age",nki70)

# Fisher's Exact Test
compare(ER~Grade,nki70)

# Two-Samples t-test
compare(FLT1~ER,nki70)

# Rank-Sum test
compare(FLT1~Grade,nki70)

# One-Way ANOVA
compare(FLT1~Grade,nki70)

# Welch's One-Way Test
compare(BBC3~Grade,nki70)

# Kruskal-Wallis Test
compare(Age~Grade,nki70)

# Pearson Correlation
correlate(DIAPH3~FGF18,nki70)


# Spearman Correlation
correlate(QSCN6L1~RAB6B,nki70)

# Simple Linear Regression
model(DIAPH3~FGF18,nki70,fig=3)

# Multi-Predictor Linear Regression
model(DIAPH3~FGF18+FLT1+GSTM3+Age,nki70,fig=4)

# Kaplan-Meier Estimation
nki70$EFS=Surv(nki70$time,nki70$event)
describe("EFS",nki70)

# Log-Rank Test
compare(EFS~ER,nki70)
compare(EFS~Grade,nki70)

# One-Predictor Cox Regression
model(EFS~FGF18,nki70)

# Multi-Predictor Cox Regression
model(EFS~ER+FGF18,nki70)

# Cumulative Incidence Estimation
get.package("riskRegression")
data(Melanoma)

Melanoma$ev.code=Melanoma$event
Melanoma$ev.code=gsub("censored",0,Melanoma$event)
Melanoma$ev.code=gsub("death.malignant.melanoma",1,Melanoma$ev.code)
Melanoma$ev.code=gsub("death.other.causes",2,Melanoma$ev.code)

Melanoma$failure=competing.event.time(Melanoma$time/365.25,
                                      Melanoma$ev.code,
                                      ev.key=c("0"="none",
                                               "1"="death due to melanoma",
                                               "2"="death due to other causes"))

describe("failure",Melanoma)

# Gray's Test
compare(failure~sex,Melanoma)
compare(failure~invasion,Melanoma)


# Fine-Gray Regression
model(failure~age,Melanoma)

model(failure~invasion+age,Melanoma)




