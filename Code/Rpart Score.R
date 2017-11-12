# rpartscore decision tree method

# install.packages("caret")
# install.packages("party")
# library("caret")
# 
# library("party")
# library("rpartScore")

tree_crossvalid=function(orig, cv, lbs, type){
    x=numeric(0);
    y=numeric(0);
  
    for (i in 1:cv){
        a=numeric(0);
        b=numeric(0);
        sub_sample=numeric(0)
    
        print(i)
  
        file= orig[,c("response",lbs)]
        file$response <- ifelse(file$response ==0, 0,
                            + ifelse(file$response == 1, 2, 2.5))
        sub_sample=file[rindOrd[-i,],]
        set.seed(90)
  
        res=rpartScore(response ~ ., data = sub_sample, split="abs")
   
        # res = rpart(response ~ ., data = sub_sample, method="class")
        # T.abs.mc.min.pos <- which.min(res$cptable[, 4])
        # th.1std.rule.mc=res$cptable[T.abs.mc.min.pos, 4] + res$cptable[T.abs.mc.min.pos, 5]
        # best.1std.rule.mc <- which.max(res$cptable[, 4] < th.1std.rule.mc)
        # alpha <- res$cptable[best.1std.rule.mc, 1]
        # zp=prune(res,cp=alpha)
        # zp= prune(res, cp=res$cptable[which.min(res$cptable[,"xerror"]),"CP"])
        zp= prune(res, cp=0.01)
        pred=predict(zp,newdata=file[rindOrd[i,],])
        a=pred;
    
        # a=apply(pred, 1, function(x) which(x==max(x))-1)
    
        b=file[rindOrd[i,],"response"];
        x=c(x,a)
        y=c(y,b)
    
        # rm(list=c("alpha","a","b"))
    }
    perf=performance(x=x,y=y)
    print(perf)

    tab=as.matrix(table(x,y))
    perfs=list(perf,tab)
    return(perfs)
}


  
