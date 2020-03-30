library(dhlvm)
source("utils.R")
library(parallel)
#options(future.globals.maxSize = +Inf)
#plan(multiprocess(workers=4))


estimateBoth <- function(data.input,group.input,step) { 
  data.fh<- data.input[group.input<step,vars]
  group.fh = group.input[group.input<step]
  N= nrow(data.fh)
  J=ncol(data.fh)
  L = apply(data.fh,MARGIN=2,FUN=function(x) return(length(unique(x))))
  K=4
  eta= list()
  for(j in 1:J) {
    eta[[j]] = matrix(1,nrow=K,ncol=L[j])
    for(k in 1:K) {
      if ( k <= L[j]) {
        eta[[j]][k,k] = 10
      }
    }
  }
  v0=10
  s0=1
  steps = 15
  burn = 2
  skip = 2
  tune=0.01
  
  G = length(unique(group.fh))
  alpha = matrix(1,nrow=G,ncol=K,byrow=T)
  
  posterior.dyn = dhlcModel(data.fh, group.fh, eta, v0, s0, tune, K, steps, burn, skip,saveZ=F)
  posterior.static <- hlcModel(data.fh, group.fh, eta, alpha, steps, burn, skip,saveZ=F)
  
  
  return(c(meanPredictions(data.input, group.input,posterior.static,alpha=alpha[1,]), 
           meanPredictions(data.input, group.input,posterior.dyn)))
}


meanPredictions <- function(data,group,posterior,n=100,alpha=NULL) {
  sampled = sample(posterior$out,n,replace=T)
  var =8 
  step = dim(posterior$pi)[1] +1
  if(is.null(posterior$sigma)) {
    pi.prior = gtools::rdirichlet(length(posterior$beta),alpha)
    likelihoods = sapply(sampled, function(x) { likelihood(data, group,
                                                           rbind(posterior$pi[,,x], pi.prior[x,]),
                                                           posterior$beta[[x]], select=step)
    }) 
    responseProbs = t(sapply(sampled, function(x) { pi.prior[x,]%*%posterior$beta[[x]][[var]]
    }) ) 
  } else {
    gamma_lasts = posterior$gamma[step-1,,]
    pis = t(sapply(sampled, function(x) { 
      gamma = gamma_lasts[,x] 
      sigma = posterior$sigma[,,x]
      pi.sample = MASS::mvrnorm(1,gamma,sigma) 
      return(exp(pi.sample)/sum(exp(pi.sample)))
    }))
    likelihoods = sapply(sampled,function(x) {likelihood(data, group,
                                                         rbind(posterior$pi[,,x], pis[which(sampled==x)[1],]), 
                                                         posterior$beta[[x]], select=step)})
    
    responseProbs = t(sapply(sampled, function(x) { pis[which(sampled==x)[1],]%*%posterior$beta[[x]][[var]]
    }))
    
  }
  
  return(c(mean(likelihoods),colMeans(responseProbs))) 
}

na.codes <- c(0,8,9,98,99)
vars <- c("PAGO", "PEXP","RINC","BAGO","BEXP","BUS12","BUS5","UNEMP","GOVT", "RATEX","PX1Q1","DUR","HOM","CAR")
data <- read.csv("data/michigan/mich_raw.csv")

data.input <- clean_data(data,na.codes)
group.input <- as.numeric(factor(data$YYYYMM))
data.input <- data.input[,vars]

#360:379
months=360:379
#months2 = 363:365

forecast = mclapply(months,FUN=function(x) estimateBoth(data.input,group.input,x), mc.cores=10)
save(forecast,file="forecast.RData")

#started at 12:10 pm 
#ended at 1:23 pm 
# took 1 hour 13 minutes for a single run 
#results = list() 

#for (month in months) {
 # print(month)
 # results[[paste0("y",month)]] = estimateBoth(data.input,group.input,month)
  #save(results,file="results.RData")
#}#
#forecast_metrics = future_sapply(months1, function(x) estimateBoth(data.input,group.input,x)) 

#started parallel at 2:37 pm 
#ended at ?? 
#forecast_metrics = future_sapply(months, function(x) estimateBoth(data.input,group.input,x)) 
#save(forecast_metrics,file="forecast1.RData")

#save(forecast_metrics,file="forecast2.RData")


