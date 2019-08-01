setwd("~/Dropbox/evan/code/")
datapath = "~/Dropbox/evan/data/sent/csvfiles/"
library(plyr)
library(lda)

years = 1978:2017
#I could better automate the process of building the response vector
#by taking advantage of the XML encoding that UMICH
#publishes for the questionnaire - this will be necessary if/when we extend this process
#beyond 5 questions since the manual process below will get too cumbersome 
PAGO_a = c(1,3,5,8,9)
PEXP_a = c(1,3,5,8,9)
BUS12_a = c(1,2,3,4,5,8,9)
BUS5_a  = c(1,2,3,4,5,8,9)
DUR_a = c(1,3,5,8,9)

#Generate the possible permutations of survey answers
eg = expand.grid(PAGO_a,PEXP_a,BUS12_a,BUS5_a,DUR_a)
responses = apply(eg,1,paste,collapse = "")

responses_count = as.data.frame(as.integer(responses))
colnames(responses_count) = "x"
#Now summarize the data for each year in the format necessary for lda_uncollapsed

X = list() 
#and also for topicmodels R package 
index = 1 
count99 = 0 
count98  = 0 
count7 = 0 
for (year in years) { 
  filename = paste("MSC_export_",year,sep="")
  filename = paste(filename,".csv",sep="")
  raw_data = read.csv(paste(datapath,filename,sep=""),header=TRUE)
  relevant_c = c("YYYYMM","PAGO","PEXP","BUS12","BUS5","DUR")
  raw_data = raw_data[,relevant_c]
  #If raw data has missing value, replace with 9 which is encoding in later years for missing value
  raw_data[is.na(raw_data)] = 9
  
  #These are in 2017 data 
  count99 = count99 + sum(raw_data==99)
  count98 = count98 + sum(raw_data==98)
  count7 = count7 + sum(raw_data==7)
  raw_data[raw_data==99] = 9 
  raw_data[raw_data==98] = 8 
 
  #This is in 201707 DUR data (need to look into what 7 means or if it is a mistake)
  raw_data[raw_data==7] = 9 

  #Aggregate the data in list and response-count matrix format 
   for (month in sort(unique(raw_data$YYYYMM))) {
    monthdata = raw_data[(raw_data$YYYYMM==month),]
    monthresponses = apply(monthdata[,2:ncol(monthdata)],1,paste,collapse="")
    X[[index]] = monthresponses
    index = index + 1
    mr = as.integer(monthresponses)
    monthcounts = as.data.frame(count(mr))
    responses_count = merge(as.data.frame(responses_count),monthcounts,all=TRUE)
    colnames(responses_count)[ncol(responses_count)] = month
   }
  
  
}

#33555
#clean up the response count matrix 
responses_count = t(responses_count)
colnames(responses_count) = responses_count["x",]
responses = colnames(responses_count)
responses_count = responses_count[2:nrow(responses_count),]
responses_count[is.na(responses_count)] = 0 

#drop any response codes that have 0 respondents across the sample period
length(which(colSums(responses_count) == 0))
#more than half of potential response codes do not appear in the database 

responses = responses[which(colSums(responses_count)!=0)]
responses_count = responses_count[,which(colSums(responses_count) != 0)]

save(responses_count)
#X and responses can be used for my code lda_uncollapsed.R 
#responses_count can be used for R package topicmodels

#X_lda and vocab can be used for R package lda (did not end up using this)

X_lda = list() 
for (d in 1:nrow(responses_count)){
  #convert to 0 indexed matrix of counts for lda package
  X_lda[[d]] = matrix(as.integer(c(which(responses_count[d,]!=0)-1,responses_count[d,responses_count[d,]!=0])),2,sum(responses_count[d,]!=0),byrow=TRUE)  

}
vocab = colnames(responses_count)

#the below proof of concept uses the R package topicmodels

K = 4
SEED = 2017
library(topicmodels)
#drop 8s and 9s for first simple proof of concept - the results are more complicated when they are included since 8, for example, didn't seem to be an option in initial years of  the survey 

todrop = sapply(colnames(responses_count),grepl,pattern="9|8")
responses_count = responses_count[,!todrop]
responses = responses[!todrop]

for (t in 1:length(X)) {
  rs = X[[t]]
  todrop = sapply(rs,grepl,pattern="9|8")
  X[[t]] = rs[!todrop]
}

#estimate using my code 
#K=2
#eta = 0.1
#alpha = 50/K

#source("lda_uncollapsed_f.R")
#result = lda_uncollapsed_f(X,responses,K,eta,alpha)



#estimate model with default parameters and priors (0.1 for alpha and 50/k for eta, described  more fully in topicmodels documention) 

models = LDA(responses_count,k=K,control=list(seed=2017))

#get posterior thetas and betas 
theta = posterior(models)$topics
beta = posterior(models)$terms

#print top responses for each sentiment 
sort(beta[1,],decreasing=TRUE)[1:10]
sort(beta[2,],decreasing=TRUE)[1:10]

