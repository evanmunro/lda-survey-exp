library(dhlvm)
source("utils.R")
re_estimate=T

load("data/dhs_data.RData")

#estimate Filmer-Pritchett index 
index.vars <- c("water_qual","toilet_qual","floor_qual","electric","radio","tv","fridge","motorbike","car","phone")
polyt <- c("water_qual","toilet_qual","floor_qual")
data.fprit<- dhs.data 
data.fprit[,polyt] <- apply(data.fprit[,polyt],MARGIN=2,FUN=as.factor)
data.fprit <- model.matrix(~.-1,data=data.fprit[,index.vars])
fprit <- prcomp(data.fprit,center=TRUE,scale.=TRUE)

print(fprit$rotation[,1:4])

data.input <- dhs.data[,index.vars] 
group.input <- as.numeric(as.factor(dhs.data$cname)) 
#check sparsity is OK 
checkSparsity(data.input)
checkIdCondition
K = 3
G = length(unique(group.input))
alpha = matrix(1,nrow=G,ncol=K)
J=ncol(data.input)
L = apply(data.input,MARGIN=2,FUN=function(x) return(length(unique(x))))

eta= list() 
for(j in 1:J) { 
  eta[[j]] = matrix(1,nrow=K,ncol=L[j])
  for(k in 1:K) { 
    if ( k <= L[j]) {
      eta[[j]][k,k] = 10 
    } 
  }
}

if (re_estimate) {
  set.seed(1)
  steps=3000
  burn=1000
  skip=10
  posterior <- hlcModel(data.input,group.input,eta,alpha,steps,burn,skip)
  post.ev <- posteriorMeans(posterior)
  save(post.ev,file="posteriors/dhs_estimate.RData")
  
} else { 
  load(file="posteriors/dhs_estimate.RData")
}

plotBetas(post.ev$beta,questions=colnames(data.input),path="figures/")
print(bic(data.input,group.input,post.ev$pi,post.ev$beta)) 
pi = as.data.frame(post.ev$pi) 
rownames(pi) = unique(dhs.data$cname)
#For Table 2 
print(pi)

