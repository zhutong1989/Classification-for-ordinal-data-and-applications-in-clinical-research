# load the data first

rm(list=ls())
setwd("Z:/.../data")

# install.packages("ordinalgmifs")
# install.packages("rpartScore")
# install.packages("irr")
# install.packages("e1071")
# install.packages("randomForest")
#install.packages("nnet")
library("nnet")
library(irr)
library(e1071)
library(pROC)
library(caret)
library(Hmisc)
library(ordinalgmifs)
library("party")
library("rpartScore")

library(randomForest)


# load("ind.RData")
load("Z:/.../random_index_ord.RData")
# load("Z:/Mengru/20150817-Fusion Project/temp/ind.RData")
vlabels.list<-read.table("Z:/.../varlabel.csv", header=T, sep=",")


# dataOrd=en.OC.20.021316;


source('Z:/.../Performance.R')
source("Z:/.../novel_vims.txt")
source("Z:/.../Generalized Linear Regression.R")
source("Z:/.../Rpart Score.R")
source("Z:/.../Random Forest.R")
source("Z:/.../SVM with Robust Tree Decoding.R")
source("Z:/.../All nomial response models.R")
# 

is.data.frame(dataOrd)
sum(dataOrd[,1]==0) #39 25 52
sum(dataOrd[,1]==1) #35 39 79
sum(dataOrd[,1]==2) #25 35 68
#quite balanced

# vlabels.list<-read.table("varlabel.csv", header=T, sep=",")
# vlabels.top20<-read.table("20imp_var_fusion.csv", header=F, sep=",")
# label20 = as.character(vlabels.list[[3]][match(vlabels.top20[[1]], vlabels.list[[9]])])


##for reduced dataset
load("orig20label.RData")
dataOrd=xx[,-2]

## full data
load("Z:/.../data_for_oc_reverseCoding.RData") # dataset name: dataOrd
dataOrd=dataOrd[,-c(3:5)]



dataOrd[,-1]=scale(dataOrd[,-1])
topall=as.vector(colnames(dataOrd)[-1])
colnames(dataOrd)[1]="response"





####################################################################for full dataset;
# scale data ;

# per=numeric(0);
# taba=numeric(0)
# time1=proc.time()


# full dataset
out.1 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "Cumulative", lab = topall)
out.2 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "AdjCategory", lab = topall)
out.3 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "ForwardCR", lab = topall)
out.4 = tree_crossvalid(orig = dataOrd, cv = 5, lbs = topall)
out.5 = rf_crossvalid(orig = dataOrd, cv = 5, lbs = topall)
out.6 = svm_cross(orig = dataOrd, cv = 5, lbs = topall)

outp=rbind(out.4[[1]], out.5[[1]], out.6[[1]])
#   
outt=rbind(out.4[[2]], out.5[[2]], out.6[[2]])
print(outp)
print(outt)

write.table(outp,"performance.csv",sep=",")
write.table(outt,"table.csv",sep=",")
# Can be "Cumulative", "AdjCategory",
# "ForwardCR", "BackwardCR", or "Stereotype"
# ;



# nominal outcome full dataset;
nom.out.1 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "baseline", lbs = topall)
nom.out.2 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "tree", lbs = topall)
nom.out.3 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "rf", lbs = topall)
nom.out.4 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "svm", lbs = topall)
nom.outp = rbind(nom.out.1[[1]], nom.out.2[[1]], nom.out.3[[1]], nom.out.4[[1]])
nom.outt = rbind(nom.out.1[[2]], nom.out.2[[2]], nom.out.3[[2]], nom.out.4[[2]])

print(nom.outp)
print(nom.outt)
write.table(nom.outp, "performance.csv", sep=",")
write.table(nom.outt, "table.csv", sep=",")

run.time=proc.time()-time1



###############################################################reduced dataset1

out.1 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "Cumulative", lab = label20)
out.2 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "AdjCategory", lab = label20)
out.3 = reg_crossvalid(orig = dataOrd, cv = 5, modell = "ForwardCR", lab = label20)
out.4 = tree_crossvalid(orig = dataOrd, cv = 5, lbs = label20)
out.5 = rf_crossvalid(orig = dataOrd, cv = 5, lbs = label20)
out.6 = svm_cross(orig = dataOrd, cv = 5, lbs=label20)
outp=rbind(out.1[[1]],out.2[[1]], out.3[[1]], out.4[[1]], out.5[[1]], out.6[[1]])
#   
outt=rbind(out.1[[2]],out.2[[2]], out.3[[2]], out.4[[2]], out.5[[2]], out.6[[2]])
print(outp)
print(outt)

write.table(outp,"performance.csv",sep=",")
write.table(outt,"table.csv",sep=",")
# nominal outcome full dataset;
nom.out.1 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "baseline", lbs = label20)
nom.out.2 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "tree", lbs = label20)
nom.out.3 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "rf", lbs = label20)
nom.out.4 = nom_crossvalid(orig = dataOrd, cv = 5, selec = "svm", lbs = label20)
nom.outp=rbind(nom.out.1[[1]], nom.out.2[[1]], nom.out.3[[1]], nom.out.4[[1]])
nom.outt=rbind(nom.out.1[[2]], nom.out.2[[2]], nom.out.3[[2]], nom.out.4[[2]])

print(nom.outp)
print(nom.outt)
write.table(nom.outp,"performance.csv",sep = ",")
write.table(nom.outt,"table.csv",sep = ",")
#########################################reduced dataset2


############################################reduced dataset 3



