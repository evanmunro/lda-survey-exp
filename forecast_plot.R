library(dhlvm)
source("utils.R")

na.codes <- c(0,8,9,98,99)
vars <- c("PAGO", "PEXP","RINC","BAGO","BEXP","BUS12","BUS5","UNEMP","GOVT", "RATEX","PX1Q1","DUR","HOM","CAR")
data <- read.csv("data/michigan/mich_raw.csv")

data.input <- clean_data(data,na.codes)
group.input <- as.numeric(factor(data$YYYYMM))
data.input <- data.input[,vars]



load(file="forecast_full.RData")
load(file="forecast1.RData")
forecast = cbind(forecast_metrics,sapply(forecast,unlist))

data.unemp <- data.input[,"UNEMP"]
unemp.t = matrix(0,nrow=4,ncol=length(steps))
i=1
steps=360:379
for (step in steps) {
  unemp.yr = data.unemp[group.input==step]
  unemp.yr = as.vector(table(unemp.yr)/sum(table(unemp.yr)))
  unemp.t[1:3,i] = unemp.yr[1:3]
  i=i+1
}

months = unique(data$YYYYMM)[steps]

months = paste0(months,"01")
months = as.Date(months,"%Y %m %d")

data.plot = data.frame(  month= months, 
                        true_data = unemp.t[1,], 
                         static_model = forecast[2,],
                         dynamic_model = forecast[7,])

data.long = reshape2::melt(data.plot,id.vars=c("month"))

library(ggplot2)
ggplot(data = data.long, mapping = aes(x = month, y = value, color = variable)) +geom_line() 

