library(dhlvm)
source("utils.R")

na.codes <- c(0,8,9,98,99)
vars <- c("PAGO", "PEXP","RINC","BAGO","BEXP","BUS12","BUS5","UNEMP","GOVT", "RATEX","PX1Q1","DUR","HOM","CAR")
data <- read.csv("data/michigan/mich_raw.csv")

data.input <- clean_data(data,na.codes)
group.input <- as.numeric(factor(data$YYYYMM))
data.input <- data.input[,vars]



load(file="forecast2.RData")
data.unemp <- data.input[,"UNEMP"]
unemp.t = matrix(0,nrow=4,ncol=length(steps))
i=1
for (step in steps) {
  unemp.yr = data.unemp[group.input==step]
  unemp.yr = as.vector(table(unemp.yr)/sum(table(unemp.yr)))
  unemp.t[1:3,i] = unemp.yr[1:3]
  i=i+1
}

data.plot = data.frame(  month= steps, 
                         true_data = unemp.t[3,], 
                         static_model = forecast_metrics[4,],
                         dynamic_model = forecast_metrics[9,])

data.long = reshape2::melt(data.plot,id.vars=c("month"))

library(ggplot2)
ggplot(data = data.long, mapping = aes(x = month, y = value, color = variable)) +geom_line() 

