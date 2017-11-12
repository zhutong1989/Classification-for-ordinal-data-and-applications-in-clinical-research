# Readin the raw data for depression;

setwd("C:/.../data")
install.packages("Hmisc")

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
install.packages("VGAM")
library(VGAM)
options(digits=15) # add digits option to keep all the digits.
mdddata<-read.table("work_.csv", header=T, sep=",")

# Load data
datMy<-mdddata[,c(2,5, 6, 7, 61:66, 77,78:301)]
datMy$sev=2-datMy["severity1"];  
table(datMy$sev)
datMy$sev=factor(unlist(datMy$sev), levels=c(0,1,2),labels = c("normal", "mild", "severe"));
save(datMy, file="all.RData")
load("all.RData")
raw=datMy[,-c(1,2,5:11)]

## Fit ordered logit model and store results 'm'
m <- polr(sev ~ m1 +m2 +m3, data =raw, Hess=TRUE)

## view a summary of the model
summary(m)


