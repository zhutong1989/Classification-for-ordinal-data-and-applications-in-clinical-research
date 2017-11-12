#SVM for orinal classification
setwd("W:/.../temp")
load("data_for_oc_reverseCoding.RData") # dataset name: dataOrd
load("ind.RData")
load("random_index_ord.RData")
vlabels.list<-read.table("W:/.../varlabel.csv", header=T, sep=",")

library(e1071)
library(pROC)
library(caret)
library(Hmisc)

is.data.frame(dataOrd)
sum(dataOrd[,1]==0) # 25
sum(dataOrd[,1]==1) # 39
sum(dataOrd[,1]==2) # 35
#quite balanced

#check the rindOrd
dim(rindOrd)
dataOrd[,1][rindOrd[1,]]

C<-2^seq(-5, 15, 5) # 5 cost parameter
svmgrid<-expand.grid(.C=C)
svmgrid<-as.matrix(svmgrid)
head(svmgrid)
dim(svmgrid)

############################initialization############################
n = 5
percentcc<-NULL
pccMV<-NULL
pccDefault<-NULL
spm<-NULL
spmMV<-NULL
spmDefault<-NULL
knd<-NULL
kndMV<-NULL
kndDefault<-NULL
gammaGK<-NULL
gammaGKMV<-NULL
gammaGKDefault<-NULL


############################/initialization############################

############################robust decoding tree function############################
farfirst<-function(dv){ 
  # dv=decision values should be a row of three decision values, each for classifier 0/1, 0/2, 1/2, respectively;
  # and the order 0/1, 0/2, 1/2 is also necessary
  if(sign(dv[2]) < 0) {
    if(sign(dv[1]) < 0) return(0) else return(1)
  } else {
    if(sign(dv[3]) < 0) return(1) else return(2)
  }
}
# test feasibility
# tempdv<-c(-0.5267119316747323, -0.4608611882169786, -0.4217239210170315)
# farfirst(tempdv)

############################/robust decoding tree function############################



############################tuning function#######################################
svmOrd.tune<-function(C, data, cvindex, discard, scl){
 
    for(i in 1:n){      
        
        # scl<-TRUE;data=dataOrd;C=1;cvindex=rindOrd;i=4;scl=TRUE;  # for test feasibility;
      
        if(length(discard) == 1){
        #discard==0 means we use the full predictor set
        ##x, y in svm has to be in matrix form;
        ##as.factor(y) very important; else it might be a regression problem
        svmfit<-svm(x = data.matrix(data[cvindex[-i,], -1]),
                    y = as.factor(data.matrix(data[cvindex[-i,], 1])),
                    scale = scl,
                    method = "C-classification",
                    cost = C,
                    kernel = "linear",
                    probability = TRUE)
        pp = predict(svmfit, data.matrix(data[cvindex[i,], -1]), decision.values=TRUE, probability=TRUE);
        class=predict(svmfit, data.matrix(data[cvindex[i,], -1]))
        # class
        classdv<-attr(pp, "decision.values")
        classprob<-attr(pp, "probabilities")
   
      }  else { svmfit<-svm(x=data.matrix(data[cvindex[-i,],-c(1, discard)]),
                            y=as.factor(data.matrix(data[cvindex[-i,],1])),
                            scale=scl,
                            method="C-classification",
                            cost=C,
                            kernel="linear",
                            probability=TRUE);
      pp=predict(svmfit, data.matrix(data[cvindex[i,],-c(1, discard)]), decision.values=TRUE, probability=TRUE); 
      classdv<-attr(pp, "decision.values")
      classprob<-attr(pp, "probabilities")
      }
      
      predresult<-apply(classdv, 1, farfirst)
      predresult_mvote<-apply(classprob, 1, which.max)-1   
      # predresult
      # predresult_mvote
 
      ## performance measure=percentage correct classified
      percentcc.temp<-pcc(predresult, data[cvindex[i,],1])
      percentcc<-c(percentcc, percentcc.temp)
      
      pccMV.temp<-pcc(predresult_mvote, data[cvindex[i,],1])
      pccMV<-c(pccMV, pccMV.temp)
      
      pccDefault.temp<-pcc(class, data[cvindex[i,],1])
      pccDefault<-c(pccDefault, pccDefault.temp)
      
      # percentcc
      # pccMV
      # pccDefault
      
      ## performance measure = Spearman's rho
      spm.temp<-cor(predresult, data[cvindex[i,],1], method="spearman")
      spm<-c(spm, spm.temp)
      
      spmMV.temp<-cor(predresult_mvote, data[cvindex[i,],1], method="spearman")
      spmMV<-c(spmMV, spmMV.temp)
      
      spmDefault.temp<-cor(as.numeric(class)-1, data[cvindex[i,],1], method="spearman")
      spmDefault<-c(spmDefault, spmDefault.temp)
      
      # spm
      # spmMV
      # spmDefault
      
      ## performance measure = Kendall's tau
      knd.temp<-cor(predresult, data[cvindex[i,],1], method="kendall")
      knd<-c(knd, knd.temp)
      
      kndMV.temp<-cor(predresult_mvote, data[cvindex[i,],1], method="kendall")
      kndMV<-c(kndMV, kndMV.temp)
      
      kndDefault.temp<-cor(as.numeric(class)-1, data[cvindex[i,],1], method="kendall")
      kndDefault<-c(kndDefault, kndDefault.temp)
      
      # knd
      # kndMV
      # kndDefault
    
      ##performance measure = Goodman-Kruskal's gamma
      gammaGK.temp<-rcorr.cens(predresult, data[cvindex[i,],1],outx=T)[2]
      gammaGK<-c(gammaGK, gammaGK.temp)
      
      gammaGKMV.temp<-rcorr.cens(predresult_mvote, data[cvindex[i,],1],outx=T)[2]
      gammaGKMV<-c(gammaGKMV, gammaGKMV.temp)
      
      gammaGKDefault.temp<-rcorr.cens(as.numeric(class)-1, data[cvindex[i,],1],outx=T)[2]
      gammaGKDefault<-c(gammaGKDefault, gammaGKDefault.temp)
      
      
      # gammaGK
      # gammaGKMV
      # gammaGKDefault
      }
    
    train.pcc.cv<-mean(percentcc);
    train.pccMV.cv<-mean(pccMV);
    train.pccDefault.cv<-mean(pccDefault);
    
    train.spm.cv<-mean(spm);
    train.spmMV.cv<-mean(spmMV);
    train.spmDefault.cv<-mean(spmDefault);
    
    train.knd.cv<-mean(knd);
    train.kndMV.cv<-mean(kndMV);
    train.kndDefault.cv<-mean(kndDefault);
    
    train.gammaGK.cv<-mean(gammaGK);
    train.gammaGKMV.cv<-mean(gammaGKMV);
    train.gammaGKDefault.cv<-mean(gammaGKDefault);
    
    
    
    ##the reported results are: robust tree decoding, max vote, default
    result<-c(C, train.pcc.cv, train.pccMV.cv, train.pccDefault.cv , 
              train.spm.cv, train.spmMV.cv, train.spmDefault.cv ,
              train.knd.cv, train.kndMV.cv, train.kndDefault.cv ,
              train.gammaGK.cv, train.gammaGKMV.cv, train.gammaGKDefault.cv );
    names(result)<-c("Cost", "5-cv PCC of RTD", "5-cv PCC of MV", "5-cv PCC of Default",
                        "5-cv SPM of RTD", "5-cv SPM of MV", "5-cv SPM of Default",
                        "5-cv KND of RTD", "5-cv KND of MV", "5-cv KND of Default",
                        "5-cv gammaGK of RTD", "5-cv gammaGK of MV", "5-cv gammaGK of Default")
    return(result)
}


############################/tuning function############################



############################tuning execution part################################################

# test feasibility of the tuning function
result_test=svmOrd.tune(svmgrid[1,], data=dataOrd, cvindex=rindOrd, discard=0, scl=TRUE)
result_test

set.seed(910)
cvresult_noscale_full<-t(apply(svmgrid, 1, svmOrd.tune, data=dataOrd, cvindex=rindOrd, discard=0, scl = F))
cvresult_noscale_ind<-t(apply(svmgrid, 1, svmOrd.tune, data=dataOrd, cvindex=rindOrd, discard=ind, scl = F))


set.seed(910)
cvresult_scale_full<-t(apply(svmgrid, 1, svmOrd.tune, data=dataOrd, cvindex=rindOrd, discard = 0, scl = TRUE))
cvresult_scale_ind<-t(apply(svmgrid, 1, svmOrd.tune, data=dataOrd, cvindex=rindOrd, discard = ind, scl = TRUE))
write.table(cvresult_scale_full, file = "SVMOC_cvresultsFull_012516.csv", sep = ",") 
write.table(cvresult_scale_ind, file = "SVMOC_cvresultsind_012516.csv", sep = ",") 
write.table(cvresult_noscale_full, file = "SVMOC_cvresultsFullnoscl_012516.csv", sep = ",") 
write.table(cvresult_noscale_ind, file = "SVMOC_cvresultsindnoscl_012516.csv", sep = ",") 


##results reported cvresult_noscale_ind with max cv pcc=0.44
############################/tuning execution part################################################

############################final model fitting part################################################
set.seed(910)
svmOC.R1<-svm(x=data.matrix(dataOrd[,-c(1, ind)]),
            y=as.factor(data.matrix(dataOrd[,1])),
            scale=F,
            method="C-classification",
            cost=1,
            kernel="linear",
            probability=TRUE)
set.seed(910)
svmOC.Full<-svm(x=data.matrix(dataOrd[,-1]),
              y=as.factor(data.matrix(dataOrd[,1])),
              scale=F,
              method="C-classification",
              cost=32,
              kernel="linear",
              probability=TRUE)


############################/final model fitting part################################################


############################variable importance part################################################
# final model variable importance 
# svm.R1$coefs: weight of the original train set 
# svm.R1$SV: support vectors (scaled)
optvimp<-function(svmfit, typ){
 # svmfit=svmOC.Full; typ="FULL"
  w <- t(svmfit$coefs) %*% svmfit$SV
  w_<-(abs(w[1,])+abs(w[2,]))/2
 # length(w_)
  ww<-matrix(w_, ncol=1)
  # head(ww)
  # dim(ww)
  rownames(ww)<-colnames(w)
  # head(ww)
  w.r1<-ww
  # head(w.r1)
  varimp1_<-w.r1[order(w.r1[,1], decreasing=TRUE),]
  label<-vlabels.list[[9]][match(c(names(varimp1_)), vlabels.list[[3]])]
#  length(label)
  broad<-vlabels.list[[10]][match(c(names(varimp1_)), vlabels.list[[3]])]
 # length(broad)
  varimp1__<-cbind(as.matrix(broad, ncol=1) ,as.matrix(label, ncol=1), varimp1_, 1:length(w_))
  colnames(varimp1__)<-c("Broad Category","Variable", "Coefficient", "Rank")
  filename = paste("svm_vimp", "_", typ, "_", Sys.Date(), ".csv", sep='');
  write.csv(varimp1__, file = filename)
}
setwd("W:/Mengru/20150817-Fusion Project/temp/svmOC")
optvimp(svmOC.R1, "R1")
optvimp(svmOC.Full, "Full")



############################variable importance part################################################

# 01/25/2016 Monday

############################ final model fitting & prediction part again ################################################
set.seed(910)
svmOC.R1<-svm(x=data.matrix(dataOrd[,-c(1, ind)]),
              y=as.factor(data.matrix(dataOrd[,1])),
              scale=F,
              method="C-classification",
              cost=1024,
              kernel="linear",
              probability=TRUE)
pp=predict(svmOC.R1, data.matrix(dataOrd[,-c(1, ind)]), decision.values=TRUE, probability=TRUE); 
classdv<-attr(pp, "decision.values")
predresult<-apply(classdv, 1, farfirst)
predresult
############################ /final model fitting & prediction part again ################################################

############################ final model assessment part again ################################################
tab<-confusionMatrix(predresult, dataOrd[,1], dnn = c("Prediction", "Reference")) #dnn=dimension names
tab


############################ final model fitting part again ################################################

