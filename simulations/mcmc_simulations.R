library(future.apply)
library(dhlvm)

source("simulation_utils.R")

#set.seed(1)
plan(multiprocess, workers = 18)


J=1
L_j = 5
G=4
K=2
N=100
alpha.gen=matrix(1,nrow=G,ncol=K) 
#for(k in 1:K) {
#  alpha.gen[k,k] = 10 
#}
eta.gen=matrix(1,nrow=K,ncol=L_j)
for (i in 1:L_j) { 
  if(i<=K) { 
    eta.gen[i,i] = 10 
  } else { 
    eta.gen[K,i] = 10 
  }
}


#identified
for(N in c(100,500,1000,10000)) {
  #results <- future_replicate(10,check_recovery_of_parameters(eta.gen,alpha.gen,K,J,L_j,N,G))
  results <- future_replicate(10,check_recovery_of_parameters(eta.gen,alpha.gen,K,J,L_j,N,G))
  write(rowMeans(results),"sims1.txt",append=TRUE)
}


#underidentified

J=1
G=2
K=4
L_j = 5
checkIdCondition(G,L_j)


alpha.gen=matrix(1,nrow=G,ncol=K) 

eta.gen=matrix(1,nrow=K,ncol=L_j)
for (i in 1:L_j) { 
  if(i<=K) { 
    eta.gen[i,i] = 10 
  } else { 
    eta.gen[K,i] = 10 
  }
}


for (N in c(100,500,1000,10000)) {
  results <- future_replicate(500,check_recovery_of_parameters(eta.gen,alpha.gen,K,J,L_j,N,G))
  write(rowMeans(results),"sims2.txt",append=TRUE)
}


# for (k in 1:K ) {
#   print(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,mcmc.beta[k,]))}))
# }
