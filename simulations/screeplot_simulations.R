setwd("~/Desktop/figs/")
#First check the number of eigen values vs the rank structure of the matrix 
for (K in 2:7) { 
  
  J=3
  L_j = 8
  N=1000
  #K=2
  G = 12
  groups = sample(1:G,N,replace=T,prob=rep(1/G,G))
  beta=list() 
  eta= list() 
  
  for (j in 1:J){ 
    beta[[j]] = rdirichlet(K, rep(0.5,L_j))
  }
  
  alpha = rep(1,K)
  #alpha[1]=0.1
  pi = rdirichlet(G,alpha)
  print(colSums(pi))
  
  X = matrix(0,nrow=N,ncol=J) 
  
  for (i in 1:N) { 
    group = groups[i] 
    z_i = sample(1:K,1,prob=pi[group,])
    for (j in 1:J) { 
      beta_j = beta[[j]]
      probs = beta_j[z_i,] 
      X[i,j] = sample(1:L_j,1,prob=probs)
    }
  }
  
  
  
  Y = matrix(0,nrow=G,ncol=L_j*J)
  
  for (i in 1:N) {
    for (j in 1:J) {
      Y[groups[i],(j-1)*(L_j)+X[i,j]] = Y[groups[i],(j-1)*(L_j)+X[i,j]] + 1 
    } 
  }
  for (j in 1:J){
    for (g in 1:G) {
      Y[g,((j-1)*L_j+1):(j*L_j)] =Y[g,((j-1)*L_j+1):(j*L_j)]/sum(Y[g,((j-1)*L_j+1):(j*L_j)])
    }  
  }
  
  
  pca <- prcomp(Y)
  pr.var <- pca$sdev^2/sum(pca$sdev^2)
  png(paste("plot",K,sep=""),width=500,height=300)
  plot(pr.var,ylim=c(0,max(pr.var)+0.05),xlab="Component",ylab="Proportion Explained",main=paste("K=",K,sep=""),type="b")
  dev.off() 
}

#Check if the priors affect this 

#Check NMF results, if we re-normalize after 