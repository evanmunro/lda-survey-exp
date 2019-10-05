library(NMF)
library(MCMCpack)
check_recovery_of_parameters <- function(eta.rep,alpha,K,J,L_j,N,G,steps=3000,burn=1000,skip=50) {
  data <- generate_data(eta.rep,alpha,K,J,L_j,N,G)
  #print(data$beta)
  #nmf_result <- nmf(data$Y,K)
  #W.nmf <- nmf_result@fit@W
  #H.nmf <- nmf_result@fit@H
  #W.norm <- W.nmf/rowSums(W.nmf)
  #H.norm <- H.nmf/rowSums(H.nmf)
  #nmf.cor <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,H.norm[1,]))}))
  #nmf.err <- mean((data$Y  - W.nmf%*%H.nmf)^2)


  # lda_model <-topicmodels::LDA(data$count,K,method="Gibbs",control=list(alpha=alpha,delta=eta.rep,iter=steps,burnin=burn,thin=skip))
  # post <-  topicmodels::posterior(lda_model)
  # mcmc.pi <- post$topics
  # mcmc.beta <- post$terms
  # mcmc.cor <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,mcmc.beta[1,]))})) 

  posterior <- hlcModel(data$X,data$grps,data$eta,data$alpha,steps,burn,skip)
  post.ev <- posteriorMeans(posterior)
  #mcmc.cor.mine <- apply(data$beta,MARGIN=1,FUN=function(x){return(hellinger(x,post.ev$beta[[1]][1,]))})
  mcmc.cor.mine <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,post.ev$beta[[1]][1,]))})) 
  #mcmc.err <- mean((data$Y - post.ev$pi%*%post.ev$beta[[1]])^2)
  return(c(mcmc.cor.mine,0)) 
  #return(c(mcmc.cor.mine,mcmc.err))
}

hellinger <- function(p1,p2){
  dist <- 1/sqrt(2)*sqrt(sum((sqrt(p1)-sqrt(p2))^2))
}


generate_data <- function(eta,alpha,K,J,L_j,N,G) {
  beta =list()
  for (j in 1:J){ 
    beta[[j]] = matrix(0,nrow=K,ncol=L_j) 
    for (k in 1:K) {
      beta[[j]][k,] = rdirichlet(1, eta[k,])
    } 
  }
  groups = sample(1:G,N,replace=T,prob=rep(1/G,G))
  pi = matrix(0,nrow=G,ncol=K)
  for (g in 1:G) { 
    pi[g,] = rdirichlet(1,alpha[g,])
  }
 

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
  Y = matrix(0,nrow=G,ncol=sum(L_j))
  for (i in 1:N) {
    Y[groups[i],X[i,1]] = Y[groups[i],X[i,1]]+1
  }
  
  max_bycol = apply(Y,MARGIN=2,FUN = function(x){return(max(x))})
  if(min(max_bycol) < 0.05){
    warning("Near singular matrix")
  }
  count = Y
  Y <- Y/rowSums(Y)
  return(list(X=X,Y=Y,count=count,grps = groups,eta=list(eta),alpha=alpha,pi=pi,beta=beta[[1]],yhat.true = pi%*%beta[[1]]))
}


make_eta <- function(J,K,L,N,type="normal",X=NULL) {
  eta <- list()
  if (type=="normal"){

    for (j in 1:J) {
      eta[[j]] = matrix(0.1,nrow=K,ncol=L)
      for(k in 1:K) {
        if ( k <= L) {
          eta[[j]][k,k] = 1
        }
      }
    }
  }
  else if (type=="emp") {
    print("here2")
    Y = matrix(0,nrow=1,ncol=L)
    for (i in 1:N) {
      Y[1,X[i,1]] = Y[1,X[i,1]]+1
    }
    for (j in 1:J) {
      eta[[j]] = matrix(type,nrow=K,ncol=L)
    }
  }

  else {
    print("here3")
    for (j in 1:J) {
      eta[[j]] = matrix(type,nrow=K,ncol=L)
    }
  }

  return(eta)
}
