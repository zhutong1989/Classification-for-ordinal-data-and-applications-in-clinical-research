orig=dataOrd

# label=colnames(orig)[-c(1:5)]
# i=100
# modell="Cumulative";
install.packages("VGAM")
library("VGAM")
install.packages("VGAM")
library("VGAM")
feature = function(ind){
    set.seed(910)
    y = orig$response;
    x = scale(orig[,ind])
    res1 = vglm(y~ x, family = cumulative(parallel=TRUE))
    res2 = vglm(y~ x, family = acat(reverse=TRUE, parallel = TRUE))
    res3 = vglm(y~ x,family = cratio(reverse=FALSE, parallel = TRUE))
    per = c(AIC(res1), AIC(res2), AIC(res3))
    return(per)
}


xx=numeric(0)
for (i in 5:dim(orig)[2]){
    print(i)
    xx=rbind(xx,feature(ind=i))
}

xx1=as.data.frame(xx)

label.list=colnames(orig)[c(5:dim(orig)[2])]

xx1$name=label.list

# Rank variables by model and select summarize the ranking and average it;
xx2=data.frame(v1=order(xx1[,1]), v2 = order(xx1[,2]),v3 = order(xx1[,3]))
    
a=rowSums(xx2)

out=data.frame(ord=a,name=label.list)
rank= as.character(out[order(out$ord),"name"])

save(rank,file="ranking_label.RData")
load(file="ranking_label.RData")

