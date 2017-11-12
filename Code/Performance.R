##created on 1/25/2016 to write a function for performane measure;
# load("Z:/.../ind.RData")
# load("Z:/.../random_index_ord.RData")
# x=out$class
# y=orig[rindOrd[i,],"yOrd.sub"];

performance=function(x,y){
  
  per.1=sum(x==y)/length(x);
  per.2=cor(x,y,method="spearman")
  per.3=cor(x,y,method="kendall")
  per.4=rcorr.cens(x,y,outx=T)[2]
  m=t(rbind(x,y))
  per.5=kappa2(m, "squared")$value
  perf=c(per.1,per.2,per.3,per.4,per.5)
  
 
  # tab = confusionMatrix(x, y, dnn = c("Prediction", "Reference")) #dnn=dimension names

  
  
  return(perf)
}


