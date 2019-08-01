library(dhlvm)
source("~/Documents/Github/hlvm-survey-paper/data_cleaning_utils.R")
setwd("~/Dropbox/evan/results/Michigan/")
data_dir <- "~/Documents/Data/Michigan/raw_data_all.csv"
na.codes <- c(0,8,9,98,99)

data <- read.csv(data_dir)

#check which variables are not available in the first year: 
na_pct <- function(x) { 
  return(sum(is.na(x))/length(x)) 
}

na.pcts <- apply(first_mo,MARGIN=2,FUN=na_pct) 
n.opts <- apply(data,MARGIN=2,FUN=function(x) return(length(unique(x))))

full.sample.cols <- colnames(data)[na.pcts<0.5]
candidate.cols <-colnames(data)[na.pcts<0.5&n.opts <25]
dem.cols <- c("BIRTHM","REGION","SEX","MARRY","NUMKID","NUMADT","EDUC","ECLGRD","EHSGRD","EGRADE","INCQFM") 
ics.cols = c("ICS","ICC","ICE")

questions <- candidate.cols[!candidate.cols%in% c(dem.cols,ics.cols)]
print(questions)

data.idx <- data[,questions]
apply(data.idx,MARGIN=2,FUN=unique)
date_var <- "YYYYMM"
orig_idx <- "ICS" 

data.for.input <- clean_data(data.idx,na.codes)
groups.input <- as.numeric(factor(data$YYYYMM))

J=ncol(data.for.input)
L = apply(data.for.input,MARGIN=2,FUN=function(x) return(length(unique(x))))

K = 4

eta= list() 
for(j in 1:J) { 
  eta[[j]] = matrix(0.1,nrow=K,ncol=L[j])
  for(k in 1:K) { 
    if ( k <= L[j]) {
      eta[[j]][k,k] = 1
    } 
  }
}

v0=10
s0=1
steps = 1000
burn = 100
skip = 10
tune=0.01

load(file="~/Documents/Data/Michigan/Index_K_2.Rdata")
posterior = dhlcModel(data.for.input,groups.input,eta,v0,s0,tune,K,steps,burn,skip)
save(posterior,file="K_4_Index.Rdata")

dates <- paste(unique(data$YYYYMM),"01",sep="") 
dates <- as.Date(dates,"%Y%m%d")
post.ev <- posteriorMeans(posterior)
plotPis(post.ev$pi,dates) 
plotBetas(post.ev$beta)
plotWithDatesandR(data.frame(date=dates,toplot=post.ev$pi[,1]),lab="Probability Index")

#sentiment vs education and income 

na.obs<- is.na(data$EDUC) | is.na(data$INCOME)
educ.z.counts <- table(data$EDUC,post.ev$z_assign)
educ.z.freq <- educ.z.counts/apply(educ.z.counts,MARGIN=1,FUN=sum)
colnames(educ.z.freq)=c("Z_i=1","Z_i=2")

ggplot(df,aes(x=y,group=z,fill=z))+
  geom_histogram(position="dodge2",bins=10)+theme_bw()

sp500 <- read.csv("~/Documents/Data/Michigan/sp500_shiller.csv")
unrate <- read.csv("~/Documents/Data/Michigan/UNRATE.csv")
epu <- read.csv("~/Documents/Data/Michigan/epu.csv")
epu <- (epu - min(epu))/(max(epu)-min(epu))
epu <- epu$epu
pi2.short <- post.ev$pi[1:length(epu),2]
dates.short <- dates[1:length(epu)]
unrate <- unrate$UNRATE[1:length(epu)]
sp500 <- (sp500$SP500[2:length(sp500$SP500)] - sp500$SP500[1:(length(sp500$SP500)-1)])/sp500$SP500[1:(length(sp500$SP500)-1)]
sp500 <- sp500[1:length(epu)]
short.data = data.frame(dates=unique(data$YYYYMM[1:length(epu)]),epu=epu,sp500=sp500,unrate=unrate)
long.data = data.frame(dates=data$YYYYMM,z_prob = post.ev$z_prob[,1],educ=factor(data$EDUC))
long.data = long.data[!is.na(long.data$educ),]

educ_index<- aggregate(long.data,by=list(long.data$educ,long.data$dates),FUN=mean)
high_educ_index <- educ_index$z_prob[educ_index$Group.1==6]
low_educ_index <- educ_index$z_prob[educ_index$Group.1==1]
#low_educ_index <- aggregate(educ_index[educ_index$Group.1!=6,],
#                            by=list(educ_index[educ_index$Group.1!=6,
 #                                              "Group.2"]),FUN=mean)
reg.high.data <- data.frame(dates = dates.short,high_educ_index=high_educ_index[1:length(epu)],epu=epu,sp500=sp500,unrate=unrate)
reg.low.data <- data.frame(dates=dates.short,low_educ_index=low_educ_index[1:length(epu)],epu=epu,sp500=sp500,unrate=unrate)

#test = aggregate(reg.data,by=list("YYYYMM","educ"),FUN=mean)

summary(lm(high_educ_index~sp500+unrate+epu,data=reg.high.data))
summary(lm(low_educ_index~sp500+unrate+epu,data=reg.low.data))


#see how unemployment vs news  predicts profile probabilities 
#for educated vs uneducated, or high vs low income people 



#save some data for serena 

factored.data <- apply(data.for.input,MARGIN=2,FUN=factor)
dummy.vars <- model.matrix(~.,data.frame(factored.data))
mich_FP <- data.frame(dummy.vars[,2:ncol(dummy.vars)])
write.csv(mich_FP,"mich_FP.csv")

na.value <- apply(data.for.input,MARGIN=2,FUN=max)
which.rows.na <- apply(data.for.input,MARGIN=1,FUN=function(x){return(sum(x==na.value)==0)})

mich_PCA <- data.frame(data.for.input[which.rows.na,])
mich_PCA$Time <- data$YYYYMM[which.rows.na]



