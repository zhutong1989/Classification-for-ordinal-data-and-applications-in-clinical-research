# created on 1/24/2016 to process ordinal data with generalized linear model
# rm(list=ls())
# install.packages("ordinalgmifs")
# setwd("Z:/.../data")

# modell="Cumulative";
# orig=dataOrd
# label=colnames(orig)[-c(1:5)]
# i=100
# modell="Cumulative";
# lab=label20

reg_crossvalid=function(orig, cv, modell, lab){
    x=numeric(0);
    y=numeric(0);

    for (i in 1:cv){
        print(i)
  
        a=numeric(0);
        b=numeric(0);
        sub_sample=numeric(0)
        sub_sample=orig[rindOrd[-i,],]
    
        set.seed(910)
        # res=ordinal.gmifs(response ~age_evaluation , data=sub_sample, x =label,probability.model = modell,scale=TRUE)
        # a=as.matrix(orig[rindOrd[i,],label])
        # b=as.matrix(orig[rindOrd[i,],c("age_evaluation")])
        # pred=predict(res,neww=b,newx=a)

        res=ordinal.gmifs(response ~1 , data=sub_sample,x =lab,probability.model = modell)
        new=as.matrix(orig[rindOrd[i,],lab])
   
        pred=predict(res,newx=new)
        
        a=pred$class;
        b=orig[rindOrd[i,],"response"]
  
        # res1=vglm(response~ ., family=cumulative(parallel=TRUE),data=sub_sample)
        # new=as.data.frame(orig[rindOrd[i,],lab])
        # pred=predict(res1,newdata=new,type="response")
        # a=apply(pred,1,function(x){which(x==max(x))})-1
        x=c(x,a)
        y=c(y,b)
    }
    perf=performance(x=x,y=y)
    print(perf)
  
    tab=as.matrix(table(x,y))
    perfs=list(perf,tab)
    return(perfs)
}




