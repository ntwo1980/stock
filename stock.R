initEnvironment <- function()
{
  setwd("C:/Users/nn1003/Documents/R/stock")
  library("ggplot2")
  library("zoo")
  library("quantmod")
  library("dplyr")
}

loadData <- function(file)
{
  data = read.table(file, header= TRUE, sep=",", stringsAsFactors = FALSE, colClasses=c("factor", "NULL", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric" ), col.names=c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout", "IR"))
  
  return(data)
}

loadZooData <- function()
{
  data <- read.zoo("./8018131.csv", sep=",", head=TRUE, index.column = 1, format="%Y-%m-%d", colClasses=c("NULL","NULL","character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric"))
 
  return(data)
}

analyseStock <- function(x)
{
  roundPE <- c("RoundPE")
  #result <- as.data.frame(table(x$RoundPE))
  result <- aggregate(x[roundPE], by=list(PE=x$RoundPE), length)
  colnames(result) <- c("PE", "Count")
  total <- nrow(x)
  result <- cbind(result, per = result$Count/total * 100, cumper = (1 - cumsum(result$Count)/total) * 100)
  
  return(result)
}

analyseByMonth <- function(x)
{
  by.month.open <- aggregate(x["Open"], by=list(Month=x$Month), FUN = function(x) x[length(x)])
  by.month.close <- aggregate(x["Close"], by=list(Month=x$Month), FUN = function(x) x[1])
  by.month <- cbind(by.month.open, Close = by.month.close$Close)
  by.month <- cbind(by.month, m = as.integer(substring(by.month$Month, 6)))
  by.month <- cbind(by.month, Rise = (by.month$Close - by.month$Open) * 100 / by.month$Open )
  
  # subset(by.month, m == 7)
  return(arrange(by.month, as.yearmon(Month)))
  #return(by.month[order(as.yearmon(by.month$Month)),])
}

getMonthData <- function(x)
{
  by.month <- analyseByMonth(stocks)
  
  return(filter(by.month, m == x))
}

drawHLine <- function(x, percentile)
{
  # index <- which(a1$cumper <= per)[1]
  PE = quantile(x$PE, 1 - percentile, names = FALSE)
  # abline(h = as.integer(a1[index, 1]))
  abline(h = PE)
}

getTodayStat <- function(x)
{
  percentileFunc <- ecdf(x$PE)

  todayPE <- as.numeric(x$PE[1])
  cat("Today PE:", todayPE, "Percentile:", 1 - percentileFunc(todayPE) , "\n")
  cat("\n") 
  cat("percentile: \n")
  percentile <- c(0.1, 0.2, 0.4, 0.5, 0.6)
  cat(sprintf("%5.0f%%", 100 * (1 - percentile)), "\n")
  cat(quantile(x$PE, percentile, names = FALSE))
  
  return(todayPE)
}

initEnvironment()
stocks = loadData("801813.csv")

RoundPE <- round(stocks$PE)
Year <- as.integer( format(as.POSIXct(stocks$Date), "%Y"))
Month <- as.integer( format(as.POSIXct(stocks$Date), "%m"))
stocks <- cbind(stocks, RoundPE, Month=paste(Year, "-", Month, sep = ""))

stocks1 <- stocks[1:240,]
stocks2 <- stocks[1:480,]
stocks3 <- stocks[1:720,]
stocks5 <- stocks[1:1200,]

todayPE <- getTodayStat(stocks1)

# a1 <- analyseStock(stocks1)
# a2 <- analyseStock(stocks2)
# a3 <- analyseStock(stocks3)
# a5 <- analyseStock(stocks5)

# todayPE <- as.integer(stocks$PE[1])
# 
# cumper1 <- as.integer(a1[a1$PE==todayPE, c("cumper")])
# cumper2 <- as.integer(a2[a2$PE==todayPE, c("cumper")])
# cumper3 <- as.integer(a3[a3$PE==todayPE, c("cumper")])
# cumper5 <- as.integer(a5[a5$PE==todayPE, c("cumper")])

# cat("Today PE:", todayPE, "cumper1:", cumper1, "\b% cumper2:", cumper2, "\b% cumper3:", cumper3, "\b% cumper5:", cumper5, "\b%")

by.month <- analyseByMonth(stocks)
# by(by.month$Rise, by.month$m, function(x) length(x[x > 0]) / length(x))

plot(rev(stocks1$PE), type="l", xlab="Date", ylab="PE")
grid(NA, NULL)
drawHLine(stocks1, 0.1)
drawHLine(stocks1, 0.2)
drawHLine(stocks1, 0.3)
drawHLine(stocks1, 0.4)
drawHLine(stocks1, 0.5)
drawHLine(stocks1, 0.6)
drawHLine(stocks1, 0.7)
#drawHLine(60)
drawHLine(stocks1, 0.8)

qplot(factor(RoundPE), data=stocks1, geom = "bar", xlab = "PE")
qplot(as.factor(m), Rise, data=by.month, geom="boxplot", xlab = "Month")

plot(density(stocks1$PE), lwd = 3)
abline(v = todayPE)

# rollingCumper <- numeric()
# for(i in 1:1200)
# {
#   endIndex <- i+239
#   rollingStock <- stocks[i:endIndex,]
#   rollingPE <- as.integer(rollingStock$RoundPE[1])
#   rollingA1 <- analyseStock(rollingStock)
#   rollingCumper[i] <- rollingA1[rollingA1$PE == rollingPE,"cumper"]
# }
# 
# stocks5$rollingCumper <- rollingCumper