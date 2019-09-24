library(future.apply)
library(dhlvm) 

setwd("~/Documents/Github/hlvm-survey-paper/simulations")
source("~/Documents/Github/hlvm-survey-paper/simulations/simulation_utils.R")

set.seed(1) 
plan(multiprocess, workers = 4)


J=1
L_j = 3
G=4
K=2
N=1000
alpha.gen=1
eta.gen=0.8
checkIdCondition(G,L_j)

#identified 
for(N in c(1000,10000,100000)) { 
  results <- future_replicate(500,check_recovery_of_parameters(eta.gen,alpha.gen,K,J,L_j,N,G))
  write(rowMeans(results),"id_simulations.txt",append=TRUE)
}   

#underidentified 

G=2
K=4
L_j = 5 
checkIdCondition(G,L_j)
eta_gen = 0.8
for (N in c(1000,10000,100000)) {
  results <- future_replicate(500,check_recovery_of_parameters(eta.gen,alpha.gen,K,J,L_j,N,G))
  write(rowMeans(results),"non_id_simulations.txt",append=TRUE)
}


# for (k in 1:K ) {
#   print(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,mcmc.beta[k,]))}))
# } 
