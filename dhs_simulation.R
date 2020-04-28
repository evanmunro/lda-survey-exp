library(dhlvm) 

generate_data <- function(beta,pi,N) {
  G = nrow(pi)
  K = ncol(pi)
  J = length(beta)
  L = sapply(beta,FUN = function(x) return(ncol(x)))
  groups = sample(1:G,N,replace=T,prob=rep(1/G,G))
  
  X = matrix(0,nrow=N,ncol=J)
  z = numeric(N) 
  for (i in 1:N) {
    group = groups[i]
    z[i] = sample(1:K,1,prob=pi[group,])
    for (j in 1:J) {
      beta_j = beta[[j]]
      probs = beta_j[z[i],]
      X[i,j] = sample(1:L[j],1,prob=probs)
    }
  }
  return(list(X=X,z=z, grps = groups,pi=pi,beta=beta))
} 
 
  

#5 assets
#First 2 s are quality indicators (1 to 3), Next 3 are binary 


# Wealthy, rural poor, urban poor 

K=3 

beta = list() 
#roof type 
beta[[1]] = matrix(c(0.03, 0.37, 0.6, 
                     0.10, 0.80, 0.1, 
                     0.8,  0.18, 0.02), nrow=K,ncol=3, byrow=T) 
#water type 
beta[[2]] = matrix(c(0.01,0.01,0.4,0.58,  
                     0.05,0.21,0.64,0.10,  
                     0.1, 0.85,0.02, 0.03), nrow=K,ncol=4, byrow=T )


beta[[3]]  = matrix(c(0.02, 0.98, 
                      0.4, 0.6, 
                      0.9,0.1),nrow=K, ncol= 2,byrow=T )

beta[[4]] = matrix(c(0.01,0.99, 
                     0.6,0.4,
                     0.7,0.3), nrow=K, ncol=2, byrow=T) 


beta[[5]]  = matrix(c(0.4,0.6, 
                      0.95,0.05, 
                      0.99,0.01), nrow=K, ncol=2, byrow=T)

# 4 regions 
# Two cities, one with more poor and one with more middle class 
# Rural has rich + essential 
# Industrial area has essential + poor 

pi = matrix(c(0.3,0.2,0.5, 
                  0.2,0.6,0.2, 
                  0.05,0.8,0.15, 
                  0.15,0.05,0.8), nrow = 4, ncol =K, byrow=T )

simulation = generate_data(beta,pi,40000)
data = simulation$X
grps = simulation$grps

polyt = c(1,2) 

data.fprit<- data  
data.fprit <- apply(data.fprit,MARGIN=2,FUN=as.factor)
data.fprit <- model.matrix(as.formula("~.-1"),data=data.frame(data.fprit))
data.fprit <- data.fprit[,2:ncol(data.fprit)]
fprit <- prcomp(data.fprit,center=TRUE,scale.=TRUE)

#loadings
print(fprit$rotation[,1:3])

prc = data.frame(fprit$x) 
prc$grp = grps
prc$z = simulation$z

#factor by class 
aggregate(prc$PC1,by=list(prc$z),FUN=mean)

data.input <- data
group.input <- grps

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

set.seed(1)
steps=3000
burn=1000
skip=10
posterior <- hlcModel(data.input,group.input,eta,alpha,steps,burn,skip)
post.ev <- posteriorMeans(posterior)
