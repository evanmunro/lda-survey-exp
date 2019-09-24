
clean_data <- function(raw,na.values=NULL,na.code=10000) { 
  raw[is.na(raw)] = na.code 
  raw[raw<0] = na.code 
  which.na <- t(apply(raw,MARGIN=1,FUN=function(x){return(x%in%na.values)})) 
  raw[which.na] = na.code 
  data_clean = apply(raw,MARGIN=2,FUN=function(x) {return(as.numeric(factor(x)))} )
  return(data_clean) 
}
#' Title
#'
#' @param x 
#' @param y 
#' @param z 
#'
#' @return
#' @export
#'
#' @examples
plotScatter <- function(x,y,z)  { 
  df <- data.frame(x=x,y=y,z=factor(z)) 
  ggplot2::ggplot(df,ggplot2::aes(x=x,y=y,color=z))+ggplot2::geom_point()
  # ggplot2::ggsave("scatter.pdf")
}

#' Title
#'
#' @param pi.ev 
#' @param dates 
#' @param gt 
#' @param withRecessions 
#'
#' @return
#' @export
#'
#' @examples
plotPis <- function(data,withRecessions=F) { 
  data <- reshape2::melt(data,id.vars=c("dates"))
  g <- ggplot2::ggplot(data)+ggplot2::geom_line(aes(x=dates,y=value,color=variable)) 
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
                     aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2)
  }
  g +theme_bw() 
  ggplot2::ggsave("pi_ev.pdf",width=7,height=3)
}


plotWithDatesandR <- function(data,lab) {
  library(quantmod)
  library(ggplot2)
  library(reshape2)
  umcsent = getSymbols('UMCSENT',src='FRED', auto.assign=F) 
  umcsent.df = data.frame(date=time(umcsent), umcsent = coredata(umcsent) )
  
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
  
  recessions.trim = subset(recessions.df, Peak >= min(data$date) )
  sentiment.trim  = subset(umcsent.df,date >=min(data$date))
  sentiment.trim  = subset(sentiment.trim,date <=max(data$date))
  sentiment.trim$UMCSENT = (sentiment.trim$UMCSENT - min(sentiment.trim$UMCSENT))/(max(sentiment.trim$UMCSENT)- min(sentiment.trim$UMCSENT))
  g = ggplot(sentiment.trim) + geom_line(aes(x=date, y=UMCSENT,colour='red'),alpha=0.5) + theme_bw() +labs(UMCSENT="ICS")
  g = g + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2) 
  g = g+ geom_line(data=data, aes(x=date,y=toplot,colour='blue'))
  g = g+labs(y="sentiment")
  g = g+  scale_color_discrete(name = "", labels = c(lab, "Michigan ICS"))
  return(g)
}

plotWithR <- function(data) { 
  library(ggplot2)
  library(reshape2)
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
  
    recessions.trim = subset(recessions.df, Peak >= min(data$date) )
    sentiment.trim  = subset(umcsent.df,date >=min(data$date))
    sentiment.trim  = subset(sentiment.trim,date <=max(data$date))
    sentiment.trim$UMCSENT = (sentiment.trim$UMCSENT - min(sentiment.trim$UMCSENT))/(max(sentiment.trim$UMCSENT)- min(sentiment.trim$UMCSENT))
    g = ggplot(sentiment.trim) + geom_line(aes(x=date, y=UMCSENT,colour='red'),alpha=0.5) + theme_bw() +labs(UMCSENT="ICS")
    g = g + geom_rect(data=recessions.trim, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2) 
    g = g+ geom_line(data=data, aes(x=date,y=toplot,colour='blue'))
    g = g+labs(y="sentiment")
    g = g+  scale_color_discrete(name = "", labels = c(lab, "Michigan ICS"))
    return(g)
}