library(readstata13)
library(dhlvm)
library(ggplot2)


## Create Environment Index 
datapath= "~/Dropbox/evan/data/gpss/gpss_environment.dta"
data= read.dta13(datapath)

#~1000 people each year from 2000 to 2017 
#environmental sentiment index 
#6 to 5, 5 to 4, 5 to 4, 6 and 5 to 4, 7 to 6,  
env_qs = c("envt1","envt2","econ_v_envt","engy_v_envt","gw_when","gw_serious")
env_cols = append(c("yr"),env_qs)
env_raw = data[data$yr>2000,env_cols]

env_raw$permutation = apply(env_raw[,env_qs],MARGIN=1,paste,collapse="")
env_counts = t(table(env_raw$permutation,env_raw$yr))



tune = 0.05
set.seed(1)
eta = matrix(0.1,nrow=K,ncol=V)
v0=10
s0=0.3
#v0=10
#s0=0.3
estimate_DS <- discreteLDSModel(env_counts,eta,v0,s0,tune,K,1000,100,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)

betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(env_counts),10)
dates <- paste(rownames(env_counts),"0101",sep="")
dates <- as.Date(dates,"%Y%m%d")
ggplot(data.frame(d=dates,env_index=thetaEst[1,]),aes(x=d,y=env_index))+geom_line(alpha=0.3,color='red')

#Society becoming more progressive 
datapath= "~/Dropbox/evan/data/gpss/gpss_moral.dta"
data= read.dta13(datapath)
# need to encode 5 and 6 as 5 for first 3 and 4 and 5 as 4 for last 
mor_qs = c("moral_abortion","moral_death_penalty","moral_suicide","relimp")

mor_cols = append(c("yr"),mor_qs)
mor_raw = data[,mor_cols]

mor_raw$permutation = apply(mor_raw[,mor_qs],MARGIN=1,paste,collapse="")
mor_counts = t(table(mor_raw$permutation,mor_raw$yr))


tune = 0.05
set.seed(100)
eta = matrix(0.04,nrow=K,ncol=V)
v0=10
s0=0.3
#v0=10
#s0=0.3
estimate_DS <- discreteLDSModel(mor_counts,eta,v0,s0,tune,K,10000,1000,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)
betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(mor_counts),10)
dates <- paste(rownames(mor_counts),"0101",sep="")
dates <- as.Date(dates,"%Y%m%d")
ggplot(data.frame(dates=dates,mor_index=thetaEst[1,]),aes(x=dates,y=mor_index))+geom_line(alpha=0.3,color='red')


#Health index bottoms out in recession but indiivdual porfiles aren't intepretable (possibly due to ACA)
datapath= "~/Dropbox/evan/data/gpss/gpss_health.dta"
data= read.dta13(datapath)
#Need to recode 6 to 5 , 6 to 5, 4 to 3, 6 to 5, 6 to 5 
h_qs = c("phys_health_rating","mental_health_rating","us_hc_cost_sat","us_hc_quality","us_hc_coverage")

h_cols = append(c("yr"),h_qs)
h_raw = data[,h_cols]

h_raw$permutation = apply(h_raw[,h_qs],MARGIN=1,paste,collapse="")
h_counts = t(table(h_raw$permutation,h_raw$yr))

K=2
tune = 0.05
set.seed(100)
eta = matrix(0.02,nrow=K,ncol=V)
v0=10
s0=0.3
#v0=10
#s0=0.3
estimate_DS <- discreteLDSModel(h_counts,eta,v0,s0,tune,K,1000,100,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)
betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(h_counts),10)
dates <- paste(rownames(h_counts),"0101",sep="")
dates <- as.Date(dates,"%Y%m%d")
ggplot(data.frame(dates=dates,h_index=thetaEst[1,]),aes(x=dates,y=h_index))+geom_line(alpha=0.3,color='red')





#### THESE ONES DIDN'T MAKE THE CUT 
datapath= "~/Dropbox/evan/data/gpss/gpss_crime.dta"
data= read.dta13(datapath)
###Crime index didn't work 
#Have to drop 2013 
crim_qs = c("crime_area_yr_ago","crime_us_yr_ago","afraid_walk","gun_laws_strict")

crim_cols = append(c("yr"),crim_qs)
crim_raw = data[data$yr>2000&data$yr!=2012&data$yr!=2010,crim_cols]
crim_raw = crim_raw[!is.na(crim_raw$yr),]
crim_raw$permutation = apply(crim_raw[,crim_qs],MARGIN=1,paste,collapse="")
crim_counts = t(table(crim_raw$permutation,crim_raw$yr))

tune = 0.05
set.seed(100)
eta = matrix(0.1,nrow=K,ncol=V)
v0=10
s0=0.3
#v0=10
#s0=0.3
estimate_DS <- discreteLDSModel(crim_counts,eta,v0,s0,tune,K,1000,100,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)

betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(crim_counts),10)
dates <- paste(rownames(crim_counts),"0101",sep="")
dates <- as.Date(dates,"%Y%m%d")
ggplot(data.frame(dates=dates,crim_index=thetaEst[1,]),aes(x=dates,y=crim_index))+geom_line(alpha=0.3,color='red')





#Government index converges well, but it is not particularily interpretable 

datapath= "~/Dropbox/evan/data/gpss/gpss_govt.dta"
data= read.dta13(datapath)
govt_qs = c("sat_govern","gov_role_econ","gov_role_values","gov_reg_biz")

govt_cols = append(c("yr"),govt_qs)
govt_raw = data[,govt_cols]

govt_raw$permutation = apply(govt_raw[,govt_qs],MARGIN=1,paste,collapse="")
govt_counts = t(table(govt_raw$permutation,govt_raw$yr))


tune = 0.05
set.seed(100)
eta = matrix(0.01,nrow=K,ncol=V)
v0=10
s0=0.3
#v0=10
#s0=0.3
estimate_DS <- discreteLDSModel(govt_counts,eta,v0,s0,tune,K,1000,100,10)
thetaEst = posteriorMean(estimate_DS$theta,estimate_DS$out)
betaLDS = posteriorMean(estimate_DS$beta,estimate_DS$out)
topBetas(t(betaLDS),colnames(govt_counts),10)
dates <- paste(rownames(govt_counts),"0101",sep="")
dates <- as.Date(dates,"%Y%m%d")
ggplot(data.frame(dates=dates,govt_index=thetaEst[1,]),aes(x=dates,y=govt_index))+geom_line(alpha=0.3,color='red')


