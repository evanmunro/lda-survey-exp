library(NMF)
library(dhlvm) 
library(MCMCpack)
source("~/Documents/Github/hlvm-survey-paper/test_dhlvm/simulation_utils.R")

J=1
L_j = 10
G=5
K=3
N=1000

data <- generate_data(0.1,1,K,J,L_j,N,G)

print("Simulated Truth and Error vs Proportions Matrix")
print(data$pi)
print(data$beta)
print(mean((data$yhat.true - data$Y)^2))

nmf_result <- nmf(data$Y,K)
W.nmf <- nmf_result@fit@W
H.nmf <- nmf_result@fit@H

Yhat <- W.nmf%*% H.nmf 
nmf.sq <- sum((Yhat - data$Y)^2)
print(nmf.sq)

print("NMF Estimate and Error vs Proportions Matrix") 

W.norm <- W.nmf/rowSums(W.nmf)
H.norm <- H.nmf/rowSums(H.nmf) 
print(W.norm)
print(H.norm)
nmf.norm.sq <- sum((W.norm%*%H.norm - data$Y)^2) 
print(nmf.norm.sq)
for (k in 1:K ) {
  print(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,H.norm[k,]))}))
} 
#cor(t(H.norm),t(data$beta))
#cor(W.norm,data$pi) 

alpha.rep=1
eta.guess <- make_eta(J,K,L_j,N)
alpha.guess <- matrix(alpha.rep,nrow=G,ncol=K)

print("MCMC Estimate when priors are wrong and Error vs Proportions Matrix")

steps=1000
burn=100
skip=10
posterior <- hlcModel(data$X,data$grps,eta.guess,alpha.guess,steps,burn,skip)
post.ev <- posteriorMeans(posterior)


Yhat.mcmc <- post.ev$pi%*% post.ev$beta[[1]]
mcmc.sq <- sum((Yhat.mcmc - data$Y)^2)
print(mcmc.sq)
#cor(t(data$beta),t(post.ev$beta[[1]]))
#cor(data$pi,post.ev$pi) 


print("MCMC Estimate when priors are correct and error vs Proportions Matrix")

posterior <- hlcModel(data$X,data$grps,data$eta,alpha.guess,steps,burn,skip)
post.ev <- posteriorMeans(posterior)

Yhat.mcmc <- post.ev$pi%*% post.ev$beta[[1]]
mcmc.sq.true <- sum((Yhat.mcmc - data$Y)^2)
print(mcmc.sq.true)
#cor(t(data$beta),t(post.ev$beta[[1]]))
#cor(data$pi,post.ev$pi) 


lda_model <- LDA(data$count,K,method="Gibbs",control=list(alpha=alpha.rep,delta=eta.rep,iter=steps,burnin=burn,thin=skip))
post <-  posterior(lda_model)
mcmc.pi <- post$topics
mcmc.beta <- post$terms 


#Now do this in a monte-carlo simulation many times 
