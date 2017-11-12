# all multi-classs classification methods (nomial responses)
# baseline logit logistic regression, decision tree, random forest, svm;

nom_crossvalid=function(orig, cv,lbs,selec){
  x=numeric(0);
  y=numeric(0);
  
  for (i in 1:cv){
    a=numeric(0);
    b=numeric(0);
    sub_sample=numeric(0)
    
               print(i)
                
               if (selec=="baseline"){
          
                 file= orig[,c("response",lbs)]
                 sub_sample=file[rindOrd[-i,],]
                 set.seed(90)
                 sub_sample$response=as.factor(sub_sample$response)
                 res<- multinom(response ~ ., data = sub_sample)
                 
                 pred = predict(res, file[rindOrd[i,],lbs]); 
                 a=as.numeric(paste(pred))
                 b=file[rindOrd[i,],"response"];
                 
               } 
                
                
             
               
              if (selec=="tree"){
                file= orig[,c("response",lbs)]
                sub_sample=file[rindOrd[-i,],]
                set.seed(90)
                
                
                res = rpart(response ~ ., data = sub_sample, method="class")
#                 T.abs.mc.min.pos <- which.min(res$cptable[, 4])
#                 th.1std.rule.mc=res$cptable[T.abs.mc.min.pos, 4] + res$cptable[T.abs.mc.min.pos, 5]
#                 
#                 best.1std.rule.mc <- which.max(res$cptable[, 4] < th.1std.rule.mc)
#                 alpha <- res$cptable[best.1std.rule.mc, 1]
#                 zp=prune(res,cp=alpha)
                zp= prune(res, cp=res$cptable[which.min(res$cptable[,"xerror"]),"CP"])
                pred=predict(zp,newdata=file[rindOrd[i,],])
                a=apply(pred, 1, function(x) which(x==max(x))-1)
                b=file[rindOrd[i,],"response"];
               
                
              } 
             
                if (selec=="rf"){  
                  set.seed(90)
                  orig$sev=ordered(orig$response,level=c(0,1,2))
                  file= orig[,c("sev", lbs)]
                  sub_sample=file[rindOrd[-i,],]
                  res=randomForest(sev ~ ., data = sub_sample)
                  pred=predict(res,newdata=file[rindOrd[i,],] )
                  a=as.numeric(paste(pred))
                  b=as.numeric(paste(file[rindOrd[i,],"sev"]))
                
                
                }
                if (selec=="svm"){
                  set.seed(90)
                  file= orig[,c("response",lbs)]
                  sub_sample=file[rindOrd[-i,],]
                  
                  sub_sample$response=as.factor(sub_sample$response)
                  svmfit<-svm(response ~.,data=sub_sample,
                              method="C-classification",
                              kernel="linear", 
                              probability=TRUE)
                  
                  pred = predict(svmfit, file[rindOrd[i,],lbs]); 
                 a=as.numeric(paste(pred))
                 b=file[rindOrd[i,],"response"];
                  
                }
      
    
    x=c(x,a)
    y=c(y,b)
  }
  perf=performance(x=x,y=y)
  print(perf)
  
  tab=as.matrix(table(x,y))
  perfs=list(perf,tab)
  return(perfs)
}

