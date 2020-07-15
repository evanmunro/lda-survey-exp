library(dhlvm)
#switch to T if you would like to re-estimate the model before replicating the tables
load("data/card/card_data.RData")
data <- card.data
groups <- paste(data$LIBCRD14,data$SINMOM14,sep="")
groups <- as.numeric(factor(groups))
data <- data[!data$NOROTTER,]
auxiliary <- c("rotter_A","rotter_B","rotter_C","rotter_D","rotter_E","rotter_F",
               "rotter_G","rotter_H","rotter_I","rotter_J","rotter_K",
               "subj_liked","subj_dislik","attitude_hs")
aux.data <- data[,auxiliary]

checkSparsity(aux.data, limit=0.05)
#set up priors
K = 3
G = length(unique(groups))
alpha = matrix(c(1,1,1),nrow=G,ncol=K,byrow=T)
J=ncol(aux.data)
L = apply(aux.data,MARGIN=2,FUN=function(x) return(length(unique(x))))

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
steps=20000
burn=10000
skip=50
posterior <- hlcModel(aux.data, groups, eta, alpha, steps, burn, skip)
post.ev <- posteriorMeans(posterior)
save(post.ev, file="posteriors/card_estimate.RData")

bic(aux.data, groups,post.ev$pi, post.ev$beta)

addCoefs=""
for (k in 1:(K-1)){
  name = paste("Z",k,sep="")
  data[,name] = post.ev$z_prob[,k]
  addCoefs = paste(addCoefs,"+",name,"*","ED76",sep="")
}

#see how evenly distributed
data[,"Z3"] = post.ev$z_prob[,3]
data$z <- apply(data[,c("Z1","Z2","Z3")],MARGIN=1,FUN=which.max)
table(data$z)

##Check which questions correspond to biggest difference in classes
raoDistance <- function(x,k1,k2) {
  return(2*acos(sum(sqrt(x[k2,]*x[k1,]))))
}
raoCalc = unlist(lapply(post.ev$beta,FUN= raoDistance,1,2))
names(raoCalc) = colnames(aux.data)
tail(sort(raoCalc))

##Additional Checks

#examine convergence of MCMC chain
#plot(posterior$pi[1,1,],type="l")

#check that heterogeneity in returns does not appear in observed heterogeneity
base1 = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R+ED76*LIBCRD14*SINMOM14"
summary(lm(base1,data=data))

#Check mean education and income for each group

for (z in 1:3) {
  print(mean(data$ED76[data$z==z]))
  print(mean(as.numeric(data$LWAGE76[data$z==z])))
}
