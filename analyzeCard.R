library(data.table)
library(plyr)
setwd("~/Dropbox/imbens/proximity/")
cardData = fread("nls.dat")

colname = fread("code_bk_cols.txt")
colnames(cardData) = colname$V1
summary(cardData)

#drop observations that are missing lwage76 observations
# N=3010 matches cards number of observations in his paper 

cardData[cardData=="."]=NA
cardData = cardData[!is.na(cardData$LWAGE76),]

cardData$EXP76=cardData$AGE76-cardData$ED76-6
cardData$EXP762=cardData$EXP76^2/100

mean(as.numeric(cardData$LWAGE76))

f = formula(LWAGE76 ~ ED76+BLACK+EXP76 +EXP762+SMSA76R+REG76R)

table2.1 = lm(f,data=cardData)

#matches Table 2 exactly 
summary(table2.1)

library(mixedMem)

#Do I come up with ability profile or family background profile?
#combination seems appropriate? 

#let's use only variables from 1976 or 1966 

#recode parental education class 1-9 to 0-8 
cardData$FAMEDb = cardData$FAMED-1 
#recode NA as 2 for the relevant binary variables 
cardData$LIBCRD14b = cardData$LIBCRD14
cardData$LIBCRD14b[is.na(cardData$LIBCRD14)] = 2 

#bucket KWW score 
cardData$KWWb = cardData$KWW
cardData$KWWb[cardData$KWW %in% 1:15] = 0
cardData$KWWb[cardData$KWW %in% 16:25] = 1
cardData$KWWb[cardData$KWW %in% 26:35] = 2 
cardData$KWWb[cardData$KWW %in% 36:45] = 3
cardData$KWWb[cardData$KWW %in% 46:60] = 4
cardData$KWWb[is.na(cardData$KWW)] = 5 
cardData = as.data.frame(cardData)

## Now load additional auxiliary data and transform as necessary 

setwd("~/Dropbox/imbens/expandedData/")
expandData = fread("expandedData.csv")

#drop those that weren't in cards original sample
expandData = as.data.frame(expandData[expandData$R0000100 %in% cardData$ID,])
expandVars = colnames(expandData)[2:ncol(expandData)]

cleanColumn <- function(x) {
  x[x<0] = max(x)+1 
  if (min(x) > 0 ) x = x-1 
  return(x)  
  
}

expandData[,expandVars] = apply(expandData[,expandVars],MARGIN=2,FUN=cleanColumn)

expandData$R0005900[expandData$R0005900 %in% 0:6] =0
expandData$R0005900[expandData$R0005900 %in% 7:13] = 1 
expandData$R0005900[expandData$R0005900==14] = 2 

#181818 for K=4 3 % and 9 % 
set.seed(200)
K=4


cardVars = c("KWWb", "FAMEDb","LIBCRD14b","MOMDAD14","SINMOM14","STEP14")

mixedData = data.frame(cardData[,cardVars],expandData[,expandVars])
Total = nrow(mixedData)
J  = ncol(mixedData)
# only one replicate for each of vars 
Rj = rep(1,J)
#Nijr is number of ranking levels 
#All data i s multinomial so it should be array of all 1s 
Nijr = array(1,dim=c(Total,J,max(Rj)))




#apply nunique to each column 
Vj = apply(mixedData,MARGIN=2,function(x) return(length(unique(x))))
Vj[Vj==2] =1 
alpha = rep(0.2,K)

dist = rep("multinomial",length(Vj))
dist[Vj==1] = "bernoulli"

obs = array(0,dim=c(Total,J,max(Rj),max(Nijr)))
obs[, , 1, 1] = as.matrix(sapply(mixedData,as.numeric))


theta <- array(0, dim=c(J,K,max(Vj)))

for (j in 1:J) {
  if(Vj[j]==1)
    theta[j,,1] = rep(runif(1),K) 
  else 
    theta[j, ,1:Vj[j]] = gtools::rdirichlet(K,rep(1,Vj[j]))
  #theta[j,,1:Vj[j]] = 1/Vj[j]
}

initial = mixedMemModel(Total=Total,J=J,Rj=Rj,
                        Nijr=Nijr,K=K,Vj=Vj,alpha=alpha,
                        theta=theta,dist=dist,obs=obs)

computeELBO(initial)

out = mmVarFit(initial,printMod=25)
computeELBO(out)
computeBIC(out)

phis = as.data.frame(out$phi)
phis = t(t(phis)/colSums(t(phis)))
phis = cbind(phis,as.matrix(apply(phis,MARGIN=1,FUN=which.max))) 

colnames(phis)[length(colnames(phis))] = "GInd"

addCoefs= ""
for ( i in 0:(K-2)) {
  intcpt = paste("G",i,sep="")
  cardData[,intcpt] = phis[,paste("Group ",i,sep="")]
  addCoefs = paste(addCoefs,"+",intcpt,"*","ED76",sep="")
}

#cardData$G3Max = phis[,"GInd"]==4 

#f2 = formula(LWAGE76 ~G1 +G2 + ED76+BLACK+EXP76 +EXP762+SMSA76R+REG76R)
#myTable = lm(f2,data=cardData)

base = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R"


f3 = formula(paste(base,addCoefs,sep="")) 
cardData$LWAGE76 = as.numeric(cardData$LWAGE76)

varyCoef = lm(f3,data=cardData)
summary(varyCoef)


theta = out$theta
#5% group 0 and 8.5% return Group 1 

#K=2: 245159.9 
#K=3: 223248.7  ## this is messed up 
#K=4: 236792.3 
#K=5: 221399 
#K=6: 225642.7 
#K=7: 223889
#K=8: 224584.2

raoDistance <- function(x) {
  return(2*acos(sum(sqrt(x[1,]*x[4,])))) 
}

divergence <- function(x) { 
  return(sum(x[4,x[4,]!=0]*log(x[4,x[4,]!=0]/x[1,x[1,]!=0])))
}

raoCalc = apply(theta,MARGIN=1, FUN= raoDistance) 
divgCalc = apply(theta,MARGIN=1, FUN= divergence) 
tail(sort(raoCalc))
tail(sort(divgCalc))

#18 is high school experience: 1(like) to 4(don't like) Group 2: somewhat, Group 4: Missing/like
#20 is Financial AId? 1 (yes) 0 (no) Group 2 (Missing), Group 4(Yes)
#17 is High school subject disliked most 0:8 Group 2: Science, Group4: Missing/Social Science
#16 is High school subject liked most 0:8: Group 2: Math, Group 4: Missing/Math
#19 Type of College Received most recently (0:None -> 4 doctoral) Group 2: Missing, Group 4:highest weight on doctoral degree
#1 is KWW score : 3:4 for Group4 and for Group2, is 3,2,1, 37% prob on highest bucket, 0% on highest bucket for grp2

#returns are 0.070 
#drops returns on education 


# robustness check 
S=1000 
dist = rep(0,S)
sig = rep(0,S)
for (s in 1:S) { 
  phi_fake = gtools::rdirichlet(nrow(mixedData),rep(0.05,K))
  #phi_fake = matrix( rnorm(nrow(mixedData)*K,mean=0,sd=1), nrow(mixedData), K) 
addCoefs= ""
for ( i in 1:K) {
  intcpt = paste("G",i,sep="")
  slope = paste("GEd",i,sep="")
  cardData[,intcpt] = phi_fake[,i]
  cardData[,slope] = phi_fake[,i]*cardData$ED76
  addCoefs = paste(addCoefs,"+",intcpt,"+",slope,sep="")
}

#base = "LWAGE76~0+BLACK+EXP76 +EXP762+SMSA76R+REG76R"
alt = "LWAGE76~BLACK+EXP76+EXP762+SMSA76R + REG76R+G1*ED76 + G2*ED76 + G3*ED76"
f3 = formula(paste(base,addCoefs,sep="")) 
#robust = lm(f3,data=cardData)
robust2 = lm(alt,data=cardData)
summary(robust2)
het_ret = max(abs(tail(coef(robust2),3)))
sig[s] = min(tail(summary(robust2)$coef[,4],3))<0.05
dist[s] = het_ret
} 
print(sum(sig))/length(sig)
hist(dist)
