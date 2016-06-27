initEnvironment <- function()
{
  options(digits=2)
  library(ggplot2)
}

loadData <- function(file)
{
  data = stocks = read.table(file, header= TRUE, sep=",", stringsAsFactors = FALSE, colClasses=c("factor", "NULL", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric" ), col.names=c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout", "IR"))
  
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
  by.month.open <- aggregate(stocks["Open"], by=list(Month=stocks$Month), FUN = function(x) x[length(x)])
  by.month.close <- aggregate(stocks["Close"], by=list(Month=stocks$Month), FUN = function(x) x[1])
  by.month <- cbind(by.month.open, Close = by.month.close$Close)
  by.month <- cbind(by.month, m = substring(by.month$Month, 6))
  by.month <- cbind(by.month, Rise = (by.month$Close - by.month$Open) * 100 / by.month$Open )
  
  # subset(by.month, m == 7)
  return (by.month)
}

getMonthData <- function(x)
{
  by.month <- analyseByMonth(stocks)
  
  return(subset(by.month, m == x))
}

drawHLine <- function(per)
{
  index <- which(a1$cumper <= per)[1]
  abline(h = as.integer(a1[index, 1]))
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

a1 <- analyseStock(stocks1)
a2 <- analyseStock(stocks2)
a3 <- analyseStock(stocks3)
a5 <- analyseStock(stocks5)

todayPE <- as.integer(stocks$PE[1])

cumper1 <- as.integer(a1[a1$PE==todayPE, c("cumper")])
cumper2 <- as.integer(a2[a2$PE==todayPE, c("cumper")])
cumper3 <- as.integer(a3[a3$PE==todayPE, c("cumper")])
cumper5 <- as.integer(a5[a5$PE==todayPE, c("cumper")])

cat("Today PE:", todayPE, "cumper1:", cumper1, "\b% cumper2:", cumper2, "\b% cumper3:", cumper3, "\b% cumper5:", cumper5, "\b%")

by.month <- analyseByMonth(stocks)
# by(by.month$Rise, by.month$m, function(x) length(x[x > 0]) / length(x))

plot(rev(stocks2$PE), type="l")
drawHLine(40)
drawHLine(50)
#drawHLine(60)
drawHLine(90)

qplot(factor(RoundPE), data=stocks1, geom = "bar", xlab = "PE")

rollingCumper <- numeric()
for(i in 1:1200)
{
  endIndex <- i+239
  rollingStock <- stocks[i:endIndex,]
  rollingPE <- as.integer(rollingStock$RoundPE[1])
  rollingA1 <- analyseStock(rollingStock)
  rollingCumper[i] <- rollingA1[rollingA1$PE == rollingPE,"cumper"]
}

stocks5$rollingCumper <- rollingCumper