library(dhlvm)
library(readstata13)
library(dplyr)
library(ggplot2) 
library(xtable) 

source("~/Documents/Github/hlvm-survey-paper/data_cleaning_utils.R")
setwd("~/Documents/Data/DHS_Burke/")

rawData = read.dta13("dhs_41417_indexed_weighted.dta")
head(rawData)

latin_code = c("BO","CO","DR","GY","HT","NI","PE","VN")

index.vars = colnames(rawData)[7:16]

data.subset <- rawData %>%
  filter(year >=2009) %>%
  filter(cname %in% latin_code) %>% 
  select(append(c("year","cname","index","hhwt"),index.vars))


polyt <- c("water_qual","toilet_qual","floor_qual")
data.fprit<- data.subset
data.fprit[,polyt] <- apply(data.fprit[,polyt],MARGIN=2,FUN=as.factor)
data.fprit <- model.matrix(~.-1,data=data.fprit[,index.vars])

fprit <- prcomp(data.fprit,center=TRUE,scale.=TRUE)
summary(fprit)
for (k in 1:K) { 
  data.subset$fprit = fprit$x[,k]
  #data.subset$fprit = (data.subset$fprit - min(data.subset$fprit))/(max(data.subset$fprit)-min(data.subset$fprit))
  indexes <- data.subset %>%
    group_by(cname) %>%
    summarize(fprit=mean(fprit))
  print(indexes)
} 

indexes <- data.subset %>%
  group_by(cname) %>%
  summarize(fprit=mean(fprit))

data.input <- clean_data(data.subset[,index.vars])
group.input <- as.numeric(as.factor(data.subset$cname))

setwd("/Users/evanmunro/Dropbox/evan/results/DHS/")

K = 4
G = length(unique(group.input))
alpha = matrix(1,nrow=G,ncol=K)
J=ncol(data.input)
L = apply(data.input,MARGIN=2,FUN=function(x) return(length(unique(x))))

eta= list() 
for(j in 1:J) { 
  eta[[j]] = matrix(0.1,nrow=K,ncol=L[j])
  for(k in 1:K) { 
    if ( k <= L[j]) {
      eta[[j]][k,k] = 1
    } 
  }
}
steps=1000
burn=100
skip=10
posterior <- hlcModel(data.input,group.input,eta,alpha,steps,burn,skip)
post.ev <- posteriorMeans(posterior)
plotBetas(post.ev$beta,questions=colnames(data.input))

pi = as.data.frame(post.ev$pi) 
rownames(pi) = unique(data.subset$cname)
colnames(pi) = c("pi_1","pi_2") 
fprit <- as.vector(indexes$fprit)
pi$fprit = fprit 
xtable(pi)