# orig=dataOrd
# topall=colnames(orig)[-c(1:5)]
# install.packages("caret")
# install.packages("party")
# library("caret")
# 
# library("party")
# library("rpartScore")
# source("Z:/.../novel_vims.txt")
# install.packages("randomForest")
# library(randomForest)



rf_crossvalid=function(orig, cv, lbs){
    x=numeric(0);
    y=numeric(0);
    for (i in 1:cv){
        a=numeric(0);
        b=numeric(0);
        sub_sample=numeric(0)
        print(i)
        set.seed(90)
        orig$sev=ordered(orig$response,level=c(0,1,2))
        file= orig[,c("sev", lbs)]
        sub_sample=file[rindOrd[-i,],]
        #res=randomForest(sev ~ ., data = sub_sample)
        res = cforest(sev ~ ., data = sub_sample, scores = list(sev = c(1, 9, 9.5)),control = cforest_unbiased(ntree = 1000))
        pred = predict(res,newdata=file[rindOrd[i,],] )
        a = as.numeric(paste(pred))
        b = as.numeric(paste(file[rindOrd[i,],"sev"]))
        x = c(x,a)
        y = c(y,b)
    }
    perf=performance(x=x,y=y)
    print(perf)
    tab=as.matrix(table(x,y))
    perfs=list(perf,tab)
    return(perfs)
}

