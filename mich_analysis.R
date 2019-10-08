library(dhlvm)
source("utils.R")
re_estimate=T
# from the SDA Codebook
na.codes <- c(0,8,9,98,99)
vars <- c("PAGO", "PEXP","RINC","BAGO","BEXP","BUS12","BUS5","UNEMP","GOVT", "RATEX","PX1Q1","DUR","HOM","CAR")

data <- read.csv("data/michigan/mich_raw.csv")

#check which variables are not available in the first year:
data.input <- clean_data(data,na.codes)
data.input <- data.input[,vars]
group.input <- as.numeric(factor(data$YYYYMM))

#checkSparsity(data.input)

N= nrow(data.input)
J=ncol(data.input)
L = apply(data.input,MARGIN=2,FUN=function(x) return(length(unique(x))))


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
if (re_estimate) {
  print("starting")
  set.seed(1)
  v0=10
  s0=1
  steps = 3000
  burn = 1000
  skip = 10
  tune=0.01

  posterior = dhlcModel(data.input,group.input,eta,v0,s0,tune,K,steps,burn,skip)
  post.ev <- posteriorMeans(posterior)
  save(post.ev,file="posteriors/mich_estimate.RData")

} else {
  load("posteriors/mich_estimate.RData")

}
#check BIC
print(bic(data.input,group.input,post.ev$pi,post.ev$beta,dynamic=T))

plotBetas(post.ev$beta,path="figures/", questions=colnames(data.input))

# check eigen values of Y
Y <-xtoAdjacency(data.input,group.input)


dates <- paste(unique(data$YYYYMM),"01",sep="")
dates <- as.Date(dates,"%Y%m%d")
umcsent <- read.csv("data/michigan/UMCSENT.csv")
umcsent <- (umcsent$UMCSENT - min(umcsent$UMCSENT))/(max(umcsent$UMCSENT)- min(umcsent$UMCSENT))*max(post.ev$pi[,1])
data.plot1 <- data.frame(dates = dates,index_1=post.ev$pi[,1],ics =umcsent )


#second figure for pi
unrate <- read.csv("data/michigan/UNRATE.csv")
epu <- read.csv("data/michigan/epu.csv")
epu <- epu$epu
pi3.short <- post.ev$pi[1:length(epu),3]
pi4.short <- post.ev$pi[1:length(epu),4]
dates.short <- dates[1:length(epu)]
unrate <- unrate$UNRATE[1:length(epu)]
unrate <- 0.6*(unrate - min(unrate))/(max(unrate)- min(unrate))
epu <- (epu - mean(epu))/sd(epu)*sd(pi4.short)+mean(pi4.short)
data.plot2 <- data.frame(dates=dates.short,index_4 = pi4.short,epu=epu)
data.plot3 <- data.frame(dates=dates.short,index_3 = pi3.short,unemp=unrate)
#Figures for Figure 3
plotPis(data.plot1,T,path="figures/mich1_")
plotPis(data.plot2,T,path="figures/mich4_")
plotPis(data.plot3,T,path="figures/mich3_")

#sentiment vs education
sp500 <- read.csv("~/Documents/Data/Michigan/sp500_shiller.csv")
sp500_ret <- (sp500$SP500[2:length(sp500$SP500)] - sp500$SP500[1:(length(sp500$SP500)-1)])/sp500$SP500[1:(length(sp500$SP500)-1)]
sp500_ret <- sp500_ret[1:length(epu)]

reg.data = data.frame(dates=unique(data$YYYYMM)[1:length(epu)],epu=epu,sp500=sp500_ret, unrate=unrate) 

educ_index <- aggregate(post.ev$z_prob[,4],by=list(factor(data$EDUC),data$YYYYMM),FUN=mean) 

high_educ_index <- educ_index$x[educ_index$Group.1==6]
low_educ_index <- educ_index$x[educ_index$Group.1==1]

reg.data$high_educ <- high_educ_index[1:length(epu)]
reg.data$low_educ <- low_educ_index[1:length(epu)]
summary(lm(high_educ ~ sp500+unrate+epu,data=reg.data))
summary(lm(low_educ ~sp500+unrate+epu,data=reg.data))

#stargazer(lm(low_educ_index~sp500+unrate+epu,data=reg.low.data), lm(high_educ_index~sp500+unrate+epu,data=reg.high.data))



#see how unemployment vs news  predicts profile probabilities
#for educated vs uneducated, or high vs low income people



#save some data for serena

input.data <- read.csv("~/Dropbox/evan/results/Michigan/mich_PCA.csv")

factored.data <- apply(data.for.input,MARGIN=2,FUN=factor)
dummy.vars <- model.matrix(~.,data.frame(factored.data))
mich_FP <- data.frame(dummy.vars[,2:ncol(dummy.vars)])
write.csv(mich_FP,"mich_FP.csv")

na.value <- apply(data.for.input,MARGIN=2,FUN=max)
which.rows.na <- apply(data.for.input,MARGIN=1,FUN=function(x){return(sum(x==na.value)==0)})

mich_PCA <- data.frame(data.for.input[which.rows.na,])
mich_PCA$Time <- data$YYYYMM[which.rows.na]

#K_2: 28
#K_3: 27
#K_4: 26.61
#K_5: 26.6
#K_6: 27.86
