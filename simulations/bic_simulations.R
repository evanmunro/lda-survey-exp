library(dhlvm) 
library(MCMCpack)

J=3
L_j = 8
N=1000
K=4
G = 8
groups = sample(1:G,N,replace=T,prob=rep(1/G,G))
beta=list() 
eta= list() 

for (j in 1:J){ 
  beta[[j]] = rdirichlet(K, rep(0.5,L_j))
}

alpha = rep(1,K)
#alpha[1]=0.1

pi = rdirichlet(G,alpha)

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



Y <- xtoAdjacency(X,groups)

for (K_model in 2:6) {

  alpha_guess = matrix(1,nrow=G,ncol=K_model)

  eta_guess= list()
  for(j in 1:J) {
    eta_guess[[j]] = matrix(0.1,nrow=K_model,ncol=L_j)
    for(k in 1:K_model) {
      if ( k <= L_j) {
       eta_guess[[j]][k,k] = 1
      }
    }
  }

  steps = 1000 
  burn = 100 
  skip = 10 

  posterior = hlcModel(X,groups,eta_guess,alpha_guess,steps,burn,skip)
  post.ev <- posteriorMeans(posterior)
  post.ev.beta <- unlist(post.ev$beta)
  dim(post.ev.beta) <- c(K_model,L_j*J)
  print(K_model) 
  #print(bic_alt(as.matrix(post.ev.beta),post.ev$pi,Y,N))
  print(bic(X,groups,post.ev$pi,post.ev$beta))
}
