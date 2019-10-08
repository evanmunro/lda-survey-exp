library(reshape2)
library(ggplot2)

#' Plot class-specific distributions over responses in bar chart
#'
#' @param betas J-length list of K x L_j matrices of class-specific distributions over responses
#' @param response_codes Optional J length list, each containing a vector of response codes for each question to plot
#' @param questions Optional J length vector of question labels to title each graph
#' @param path location to save plots
#'
#' @return Saves plots of beta for each question to path
plotBetas <- function(betas,response_codes=NULL,questions=NULL,path="") {
  library(ggplot2)
  J = length(betas)
  K = nrow(betas[[1]])
  if (is.null(questions)){
    questions = 1:J
  }
  for (j in 1:J) {
    beta_mat = t(betas[[j]])
    L_j = nrow(beta_mat)
    beta_df = data.frame(beta_mat)
    colnames(beta_df) = paste("K",1:K,sep="")
    if(!is.null(response_codes)) {
      x = response_codes[[j]]
    }
    else {
      if(L_j==2) {
        x = 0:1
      }
      else{
        x= 1:L_j
      }
    }
    if(!is.null(questions)) {
      title = paste("Q_",questions[j],sep="")
    }
    beta_df$response = factor(x)
    data_long = reshape2::melt(beta_df,id.vars=c("response"))

    colnames(data_long) = c("Response","Class","Probability")
    filename  = paste(path,title,".pdf",sep="")
    ggplot(data_long,aes(x=Response,y=Probability,fill=Class)) +
      geom_bar(stat='identity', position='dodge')
    ggsave(filename)
  }
}


plotPis <- function(data,withRecessions=F,path="") {
  data <- melt(data,id.vars=c("dates"))
  g <- ggplot(data)+geom_line(aes(x=dates,y=value,color=variable),alpha=0.9)
  if(withRecessions==T) {
    recessions.df = read.table(textConnection(
      "Peak, Trough
      1857-06-01, 1858-12-01
      1860-10-01, 1861-06-01
      1865-04-01, 1867-12-01
      1869-06-01, 1870-12-01
      1873-10-01, 1879-03-01
      1882-03-01, 1885-05-01
      1887-03-01, 1888-04-01
      1890-07-01, 1891-05-01
      1893-01-01, 1894-06-01
      1895-12-01, 1897-06-01
      1899-06-01, 1900-12-01
      1902-09-01, 1904-08-01
      1907-05-01, 1908-06-01
      1910-01-01, 1912-01-01
      1913-01-01, 1914-12-01
      1918-08-01, 1919-03-01
      1920-01-01, 1921-07-01
      1923-05-01, 1924-07-01
      1926-10-01, 1927-11-01
      1929-08-01, 1933-03-01
      1937-05-01, 1938-06-01
      1945-02-01, 1945-10-01
      1948-11-01, 1949-10-01
      1953-07-01, 1954-05-01
      1957-08-01, 1958-04-01
      1960-04-01, 1961-02-01
      1969-12-01, 1970-11-01
      1973-11-01, 1975-03-01
      1980-01-01, 1980-07-01
      1981-07-01, 1982-11-01
      1990-07-01, 1991-03-01
      2001-03-01, 2001-11-01
      2007-12-01, 2009-06-01"), sep=',',
      colClasses=c('Date', 'Date'), header=TRUE)
    recessions.trim = subset(recessions.df, Peak >= min(data$dates) )
    g<- g+ geom_rect(data=recessions.trim,
                     aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.4)
  }
  g +theme_bw()
  ggsave(paste(path,"pi_ev.pdf",sep=""),width=7,height=2)
}
