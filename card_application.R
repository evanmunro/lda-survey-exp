library(dhlvm)
library(data.table)
library(plyr)
source("~/Documents/Github/hlvm-survey-paper/data_cleaning_utils.R")
setwd("/Users/evanmunro/Dropbox/evan/results/Card/")
cardData = fread("~/Documents/Data/Card/nls.dat")
colname = fread("~/Documents/Data/Card/code_bk_cols.txt")
colnames(cardData) = colname$V1

#drop observations that are missing lwage76 observations
# N=3010 matches cards number of observations in his paper
#recreate Card Table 2 
cardData[cardData=="."]=NA
cardData = cardData[!is.na(cardData$LWAGE76),]
cardData$EXP76=cardData$AGE76-cardData$ED76-6
cardData$EXP762=cardData$EXP76^2/100
f = formula(LWAGE76 ~ ED76+BLACK+EXP76 +EXP762+SMSA76R+REG76R)
table2.1 = lm(f,data=cardData)
summary(table2.1)

#load and clean data for auxiliary data 
#drop those that weren't in cards original sample and recode for input 
expandData = fread("~/Documents/Data/Card/expandedData.csv")
expandData = as.data.frame(expandData[expandData$R0000100 %in% cardData$ID,])
expandVars = read.csv("~/Documents/Data/Card/expanded_shortlist.csv",header=F)$V1
aux.data <- clean_data(expandData[,as.vector(expandVars)])
aux.groups <- as.numeric(factor(paste(cardData$NODADED,cardData$NOMOMED,sep="")))

K = 3
G = length(unique(aux.groups))
alpha = matrix(1,nrow=G,ncol=K)
J=ncol(aux.data)
L = apply(aux.data,MARGIN=2,FUN=function(x) return(length(unique(x))))

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
posterior <- hlcModel(aux.data,aux.groups,eta,alpha,steps,burn,skip)
post.ev <- posteriorMeans(posterior)
aicm(posterior) 
#save(posterior,file="~/Dropbox/evan/results/Card/model_group_by_ped.RData")
load("~/Dropbox/evan/results/Card/model_group_by_ped.RData")
post.ev <- posteriorMeans(posterior)
plotBetas(post.ev$beta)


addCoefs=""
for (k in 1:(K-1)){ 
  name = paste("Z",k,sep="")
  cardData[,name] = post.ev$z_prob[,k]
  addCoefs = paste(addCoefs,"+",name,"*","ED76",sep="")
}


base0 = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R+ED76"

summary(lm(base0,data=cardData))

summary(lm(paste(base0,addCoefs,sep=""),data=cardData))

#IMpossible to beleive luck plays a role vs little influence for q20 

#base1 = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R+ED76+KWW+FAMED+LIBCRD14+MOMDAD14+SINMOM14+STEP14"

#summary(lm(base1,data=cardData))

#summary(lm(paste(base1,addCoefs,sep=""),data=cardData))

post.ev$pi

raoDistance <- function(x,k1,k2) {
  return(2*acos(sum(sqrt(x[k2,]*x[k1,]))))
}

divergence <- function(x,k1,k2) {
  return(sum(x[k1,x[k1,]!=0]*log(x[k1,x[k1,]!=0]/x[k2,x[k2,]!=0])))
}

raoCalc = unlist(lapply(post.ev$beta,FUN= raoDistance,1,2))
names(raoCalc) = paste("V",1:22,sep="")
#divgCalc = lapply(post.ev$beta, FUN= divergence)
#profile 1 has "most internal responses" 
tail(sort(raoCalc))
#tail(sort(divgCalc))


#-123992  for K=3 
#-127071 for K=2 
