library(stargazer)
library(kableExtra)
source("plot_utils.R")

#load Card data and drop individuals with missing rotter scale variables
load("data/card/card_data.RData")
card.groups <- paste(card.data$LIBCRD14, card.data$SINMOM14,sep="")
card.groups <- as.numeric(factor(card.groups))
card.data <- card.data[!card.data$NOROTTER, ]
card.aux <- c("rotter_A", "rotter_B", "rotter_C","rotter_D", "rotter_E", "rotter_F",
               "rotter_G", "rotter_H", "rotter_I", "rotter_J", "rotter_K",
               "subj_liked", "subj_dislik", "attitude_hs")

#Load Michigan Data
mich.aux <- c("PAGO", "PEXP","RINC","BAGO","BEXP","BUS12","BUS5","UNEMP","GOVT",
          "RATEX","PX1Q1","DUR","HOM","CAR")
mich.data <- read.csv("data/michigan/mich_raw.csv")

###########################################################################
## Figure 3 - LDA-DE Indices for Michigan Consumer Survey Data
###########################################################################
load(file="posteriors/mich_estimate.RData")
dates <- paste(unique(mich.data$YYYYMM),"01",sep="")
dates <- as.Date(dates,"%Y%m%d")
umcsent <- read.csv("data/michigan/UMCSENT.csv")
umcsent <- (umcsent$UMCSENT - min(umcsent$UMCSENT))/(max(umcsent$UMCSENT)- min(umcsent$UMCSENT))*max(post.ev$pi[,1])
data.plot1 <- data.frame(dates = dates, index_1=post.ev$pi[, 1], ics =umcsent )

unrate <- read.csv("data/michigan/UNRATE.csv")
epu <- read.csv("data/michigan/epu.csv")
epu <- epu$epu
pi3.short <- post.ev$pi[1:length(epu), 3]
pi4.short <- post.ev$pi[1:length(epu), 4]
dates.short <- dates[1:length(epu)]
unrate <- unrate$UNRATE[1:length(epu)]
unrate <- 0.6*(unrate - min(unrate))/(max(unrate)- min(unrate))
epu <- (epu - mean(epu))/sd(epu)*sd(pi4.short)+mean(pi4.short)
data.plot2 <- data.frame(dates=dates.short, index_4 = pi4.short, epu=epu)
data.plot3 <- data.frame(dates=dates.short, index_3 = pi3.short, unemp=unrate)

plotPis(data.plot1, T, path="exhibits/mich1_")
plotPis(data.plot2, T, path="exhibits/mich4_")
plotPis(data.plot3, T, path="exhibits/mich3_")


###########################################################################
## Figure 4 - Belief Types in Michigan Data
###########################################################################
plotBetas(post.ev$beta, path="exhibits/", questions=mich.aux)

###########################################################################
## Figure 5 - Belief Types in NLSYM Data
###########################################################################
load(file="posteriors/card_estimate.RData")
plotBetas(post.ev$beta, questions=card.aux, path="exhibits/")

###########################################################################
## Table 1 - Returns to Education Estimates
###########################################################################
K=3
base0 = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R+ED76"

addCoefs=""
for (k in 1:(K-1)){
  name = paste("Z",k,sep="")
  card.data[,name] = post.ev$z_prob[,k]
  addCoefs = paste(addCoefs,"+",name,"*","ED76",sep="")
}
base0 = "LWAGE76~BLACK+EXP76 +EXP762+SMSA76R+REG76R+ED76"

base = lm(base0, data=card.data)
het = lm(paste(base0, addCoefs, sep=""),data=card.data)
stargazer(base, het) %>% write("exhibits/table1.txt")
