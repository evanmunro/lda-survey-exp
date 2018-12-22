setwd("~/Dropbox/evan/code/R")
library(dhlvm)
library(quantmod)
library(depmixS4)
library(dplyr)
library(ggplot2)

load("responses_count_extended.RData")
load("dataPrepUtils.R")
set.seed(1)
K=2 
V = ncol(responses_count)
Time = nrow(responses_count)
id_restrict = which(colnames(responses_count)=="11111")
eta = matrix(0.1,nrow=K,ncol=V)
alpha = matrix(1,nrow=K,ncol=K)

estimate = discreteMSFast(responses_count,eta,alpha,K,1000,100,10)

betaEst = posteriorMean(estimate$beta,estimate$out)
transEst = posteriorMean(estimate$P,estimate$out)
stateEst = posteriorMean(estimate$S,estimate$out)

dates <- paste(rownames(responses_count),"01",sep="")
dates <- as.Date(dates,"%Y%m%d")

pce = getSymbols('PCE',src='FRED', auto.assign=F) 
pce.df = data.frame(date=time(pce), pce = coredata(pce) )
pce.df$PCE = log(pce.df$PCE) - log(lag(pce.df$PCE,1)) 
pce.df$PCE_l1 = lag(pce.df$PCE,1)
pce.trim  = subset(pce.df,date >=min(dates))
pce.trim = subset(pce.trim,date <=max(dates))

hmm <- depmix(PCE ~ 1, family = gaussian(), nstates = 2, data=pce.trim)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs = posterior(hmmfit)

toPlot = data.frame(date = dates,MS_PCE = post_probs$S2,MS_SURVEY = stateEst[2,])

topBetas(t(betaEst),colnames(responses_count),top=10)

g = plotWithRecessions(toPlot,"Probability of Negative State")
plot(g)

#try state space model and compare to UMCSENT forecasting ability 

load(file="responses_count.RData")
V <- ncol(responses_count)
K <- 2
eta <- matrix(0.1,K,V)
id_restrict <- which(colnames(responses_count)=="55555555")
for (k in 1:(K-1)) { 
  eta[k,id_restrict] = 0.01
}

#tune <- 0.05
tune = 0.05
set.seed(1)
start <- Sys.time() 
v0=10
s0=0.3
#v0=10
#s0=0.3
set.seed(1)
estimate_DS <- discreteLDSModel(responses_count,eta,v0,s0,tune,K,1000,100,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)
plot(test[1,],type="l")
betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(responses_count))




#get data for forecasting exercise

umcsent = getSymbols('UMCSENT',src='FRED', auto.assign=F) 
umcsent.df = data.frame(date=time(umcsent), umcsent = coredata(umcsent) )
umcsent.df$umc_l1 = lag(umcsent.df$UMCSENT,1)
umcsent.trim  = subset(umcsent.df,date >=min(dates))
umcsent.trim = subset(umcsent.trim,date <=max(dates))

forecastData = data.frame(date=dates,y=pce.trim$PCE,pcel=pce.trim$PCE_l1,
                            umcs=umcsent.trim$umc_l1,prof1=lag(thetaEst[3,],1),prof2=lag(thetaEst[2,],1))

forecastData = forecastData[2:nrow(forecastData),]

training= 1:380
test = 381:478
forecastModel = lm(y~pcel+umcs+prof1+prof2,data=forecastData[training,])

yhat = predict(forecastModel,forecastData[test,])

mean((yhat - forecastData$y[test])^2)

baseLineModel = lm(y~pcel+umcs,data=forecastData[training,])
yhatBase = predict(baseLineModel,forecastData[test,])
mean((yhatBase - forecastData$y[test])^2)
thetaPlot = as.data.frame(t(thetaEst))
thetaPlot$date = dates
colnames(thetaPlot) = c("Profile_1","Profile_2","Profile_3","date")

g = plotWithRecessions(thetaPlot,"Profile Mixture Weight")
plot(g)

##also try MS model on regular index vs discrete 

hmm <- depmix(UMCSENT ~ 1, family = gaussian(), nstates = 2, data=umcsent.trim)
hmmfit <- fit(hmm, verbose = FALSE)
post_probs = posterior(hmmfit)
g = plotWithRecessions(data.frame(ICS_state=post_probs$S2,DISCMS_state=1-stateEst[2,],date=dates),"Profile Mixture Weight")
plot(g)

# ## Try it with BOE Data instead
# 
# library(dhlvm)
# library(quantmod)
# library(MSwM)
# library(dplyr)
# 
# load("boe_responses_limited.RData")
# load("dataPrepUtils.R")
# 
# set.seed(1)
# K=3
# V = ncol(boe_responses)
# Time = nrow(boe_responses)
# eta = matrix(0.1,nrow=K,ncol=V)
# alpha = matrix(1,nrow=K,ncol=K)
# 
# estimate = discreteMSFast(boe_responses,eta,alpha,K,1000,100,10)
# 
# betaEst = posteriorMean(estimate$beta,estimate$out)
# transEst = posteriorMean(estimate$P,estimate$out)
# stateEst = posteriorMean(estimate$S,estimate$out)
# 
# 
# cpi = getSymbols('GBRCPIALLQINMEI',src='FRED', auto.assign=F) 
# cpi.df = data.frame(date=time(cpi), 
#                     inflat = coredata(cpi))
# 
# cpi.df$inflat = 100*(cpi.df$GBRCPIALLQINMEI - lag(cpi.df$GBRCPIALLQINMEI,1))/lag(cpi.df$GBRCPIALLQINMEI,1)
# mod<-lm(cpi.df$inflat ~ 1) 
# mod.mswm=msmFit(mod, k=2, sw=c(T,T), p=0)
# plot(mod.mswm)
# 
# dates <- paste(rownames(responses_count),"01",sep="")
# dates <- as.Date(dates,"%Y%m%d")
