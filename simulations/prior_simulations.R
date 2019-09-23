library(ggplot2)
library(dhlvm)
library(MCMCpack)

J=1 
L_j = 10 
G= 3 
K=2
N= 1000 
groups = sample(1:G,N,replace=T,prob=rep(1/G,G))

generate_data <- function(eta,alpha,K,J,L_j) { 
  beta =list() 
  for (j in 1:J){ 
    beta[[j]] = rdirichlet(K, rep(eta,L_j))
  }
  
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
  return(Y/rowSums(Y)) 
}
# 
# build_eta <- function(prior) { 
#   
# }
# 
plot_Y <- function(Y,G,p) { 
  
  Y = t(Y) 
  L_j = nrow(Y) 
  beta_df = data.frame(Y)
  colnames(beta_df) = paste("G",1:G,sep="") 
  x=1:L_j 
  title=paste("Prior on beta of: ",p,sep="") 
  beta_df$response = factor(x)
  data_long = reshape2::melt(beta_df,id.vars=c("response"))
  colnames(data_long) = c("Response","Group","Proportion")
  filename  = paste("~/Desktop/response_mat",p,".pdf",sep="") 
  ggplot2::ggplot(data_long,ggplot2::aes(x=Response,y=Proportion,fill=Group)) + 
    ggplot2::geom_bar(stat='identity', position='dodge')+ggplot2::ggtitle(title)
  ggplot2::ggsave(filename) 
   
}

for (p in c(0.01,0.1,1,10)) { 
  data <- generate_data(p,1,K,J,L_j)
  plot_Y(data,G,p)
}