
check_recovery_of_parameters <- function(eta.rep,alpha,K,J,L_j,N,G,steps=1000,burn=100,skip=10) {
  data <- generate_data(eta.rep,alpha,K,J,L_j,N,G)
  nmf_result <- nmf(data$Y,K)
  W.nmf <- nmf_result@fit@W
  H.nmf <- nmf_result@fit@H
  W.norm <- W.nmf/rowSums(W.nmf)
  H.norm <- H.nmf/rowSums(H.nmf) 
  nmf.cor <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,H.norm[1,]))}))
  nmf.err <- mean((data$Y  - W.norm%*%H.norm)^2)

  
  # lda_model <- LDA(data$count,K,method="Gibbs",control=list(alpha=alpha,delta=eta.rep,iter=steps,burnin=burn,thin=skip))
  # post <-  posterior(lda_model)
  # mcmc.pi <- post$topics
  # mcmc.beta <- post$terms 
  # mcmc.cor <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,mcmc.beta[1,]))}))
  # 
  
  posterior <- hlcModel(data$X,data$grps,data$eta,data$alpha,steps,burn,skip)
  post.ev <- posteriorMeans(posterior)
  mcmc.cor.mine <- max(apply(data$beta,MARGIN=1,FUN=function(x){return(cor(x,post.ev$beta[[1]][1,]))}))
  mcmc.err <- mean((data$Y - post.ev$pi%*%post.ev$beta[[1]])^2)
  
  return(c(nmf.cor,mcmc.cor.mine,nmf.err,mcmc.err)) 
}


generate_data <- function(eta.rep,alpha,K,J,L_j,N,G) { 
  eta.rep <- matrix(eta.rep,nrow=K,ncol=L_j) 
  beta =list() 
  for (j in 1:J){ 
    beta[[j]] = rdirichlet(K, eta.rep[1,])
  }
  groups = sample(1:G,N,replace=T,prob=rep(1/G,G))
  pi = rdirichlet(G,rep(alpha,K))
  
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
  count = Y 
  Y <- Y/rowSums(Y) 
  return(list(X=X,Y=Y,count=count,grps = groups,eta=list(eta.rep),alpha=matrix(alpha,nrow=G,ncol=K),pi=pi,beta=beta[[1]],yhat.true = pi%*%beta[[1]])) 
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

