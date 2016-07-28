init.environment <- function()
{
  setwd("C:/Users/nn1003/Documents/R/stock")
  library("ggplot2")
  library("zoo")
  library("quantmod")
  library("dplyr")
  library("lubridate")
}

one.year.percenctile <- function(x)
{
  percentiles = c()
  
  for (i in 1:length(x)) {
    if(i < 240) {
      percentiles[i] <- NA
    } else {
      current <- as.numeric(x[i]) 
      start <- i+1-240
      end <- i
      percentile.func <- ecdf(as.numeric(x[start:end]))
      current.percentile <- round(percentile.func(current),2)
      percentiles[i] <- current.percentile
    }
  }
  
  return(percentiles)
}

two.week.breakdown <- function(x)
{
  breakdown = c()
  
  for (i in 1:length(x)) {
    if(i < 14) {
      breakdown[i] <- NA
    } else {
      week1start <- i+1-14
      week2start <- i+1-7
      week1min <- min(x[week1start:week2start])
      week2min <- min(x[week2start:i])
      
      if(week2min >= week1min)
        breakdown[i] = FALSE
      else
        breakdown[i] = TRUE
    }
  }
  
  return(breakdown)
}

two.week.breakup <- function(x)
{
  breakup = c()
  
  for (i in 1:length(x)) {
    if(i < 10) {
      breakup[i] <- NA
    } else {
      week1start <- i+1-10
      week2start <- i+1-5
      week1max <- max(x[week1start:week2start])
      week2max <- max(x[week2start:i])
      
      if(week2max <= week1max)
        breakup[i] = FALSE
      else
        breakup[i] = TRUE
    }
  }
  
  return(breakup)
}

a <- function(x)
{
  return (subset(x, select=c(Close,PE, PB, Returns, ClosePercentile, PEPercentile, VolumnPercentile, AmplitudePercentile, Breakdown, Breakup)))
}

load.data.df <- function(file)
{
  data <- read.table(file, header= TRUE, sep=",", stringsAsFactors = FALSE, colClasses=c("factor", "NULL", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric" ), col.names=c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout", "IR"))
  data <- subset(data, select=c(Date, Open, Close, High, Low, Volumn, PE, PB))
  returns <- -round(c(diff(data$Close), NA) / data$Close, 4) * 100
  reversed.Close = rev(data$Close)
  reversed.PE = rev(data$PE)
  reversed.Low = rev(data$Low)
  reversed.High = rev(data$High)
  reversed.Volumn = rev(data$Volumn)
  reversed.Amplitude = rev((data$High - data$Low) / data$Close)
  y <- year(data$Date)
  m <- month(data$Date)
  wd <- wday(data$Date) - 1 
  md <- mday(data$Date)
  ma10 <- SMA(reversed.Close, 10)
  ma20 <- SMA(reversed.Close, 20)
  ma30 <- SMA(reversed.Close, 30)
  close.percentile <- one.year.percenctile(reversed.Close)
  pe.percentile <- one.year.percenctile(reversed.PE)
  volumn.percentile <- one.year.percenctile(reversed.Volumn)
  amplitude.percentile <- one.year.percenctile(reversed.Amplitude)
  two.week.breakdown <- two.week.breakdown(reversed.Low)
  two.week.breakup <- two.week.breakup(reversed.High)
  data <- cbind(data, Returns = returns, Year = y, Month = m, Week = wd, Day = md, MA10 = rev(ma10), MA20 = rev(ma20), MA30 = rev(ma30))
  data <- cbind(data, PrevClose = lead(data$Close), PrevMA10 = lead(data$MA10), PrevMA20 = lead(data$MA20), PrevMA30 = lead(data$MA30))
  data <- cbind(data, ClosePercentile = rev(close.percentile), PEPercentile = rev(pe.percentile), VolumnPercentile = rev(volumn.percentile), AmplitudePercentile = rev(amplitude.percentile), Breakdown = rev(two.week.breakdown), Breakup = rev(two.week.breakup))
  rownames(data) <- data[[1]]
  data$Date <- NULL
  
  return(data)
}

plot_stocks <- function(data, title)
{
  mean.data <- round(mean(data, na.rm = TRUE), 3)
  sd.data <- round(sd(data, na.rm = TRUE), 3)
  
  hist(data, breaks = 100, prob = TRUE, xlab = "", main = title, cex.main = 0.8)
}

get.data.year <- function(data, year)
{
  days <- 240 * year
  return(data[1: days,])
}

init.environment()
stocks.whole.df <- load.data.df("801813.csv")
stocks.whole <- as.xts(stocks.whole.df[!is.na(stocks.whole.df$Returns),])
stocks1 <- as.xts(get.data.year(stocks.whole.df, 1))
stocks2 <- as.xts(get.data.year(stocks.whole.df, 2))
stocks3 <- as.xts(get.data.year(stocks.whole.df, 3))
stocks5 <- as.xts(get.data.year(stocks.whole.df, 5))
stocks = stocks1

monthly.returns <- monthlyReturn(stocks.whole$Close)
colnames(monthly.returns)[1] <- "Returns"
monthly.returns$Returns <- monthly.returns$Returns * 100
monthly.returns <- cbind(monthly.returns, Month = month(index(monthly.returns)))
qplot(as.factor(Month), Returns, data=monthly.returns, geom="boxplot", xlab = "Month")
qplot(as.factor(Week), Returns, data=stocks.whole, geom="boxplot", xlab = "Week")

todayClose <- last(stocks$Close)
todayClosePercentile <- last(stocks$ClosePercentile)
todayPE <- last(stocks$PE)
todayPEPercentile <- last(stocks$PEPercentile)

# par(mfrow = c(2, 2))
# current.PE = stocks$PE[1]
# mean.PE <- round(mean(stocks$PE), 2)
# sd.PE <- round(sd(stocks$PE), 2)
# hist(stocks$PE, breaks = 100, prob=TRUE, xlab = "PE", main = "",cex.main=0.9)
# abline(v = mean.PE, lwd =2)
# legend("topright", cex = 0.8, border = NULL, bty = "n", paste("current=", current.PE, "; mean=", mean.PE, "; sd=", sd.PE))
# par(mfrow = c(1, 1))

returns <- round(diff(stocks$Close) / stocks$Close[1:length(stocks$Close)], 6)

par(mfrow = c(5, 2))
boxplot(Returns ~ Month, data=monthly.returns, na.rm = TRUE, xlab = "Month")
grid()
boxplot(Returns ~ Week, data=stocks.whole, na.rm = TRUE, xlab = "Week")
grid()
plot(stocks$Close, type = "l", ylab = "Close", main = paste("current=", todayClose, " Percentile=", todayClosePercentile, sep=""))
abline(h = quantile(stocks$Close, c(1 - 0.8, 1-0.4, 1-0.2), names = FALSE))
plot(stocks$PE, type = "l", ylab = "PE", main = paste("current=", todayPE, " Percentile=", todayPEPercentile, sep=""))
abline(h = quantile(stocks$PE, c(1 - 0.8, 1-0.4, 1-0.2), names = FALSE))

plot(density(stocks$Close), lwd = 3)
abline(v = todayClose)
plot(density(stocks$PE), lwd = 3)
abline(v = todayPE)

monthly.returns.table <- table(sign(monthly.returns$Returns), monthly.returns$Month)
monthly.returns.table.prop <- prop.table(monthly.returns.table, 2)
plot(monthly.returns.table.prop[nrow(monthly.returns.table.prop),])
grid()

week.returns.table <- table(sign(stocks.whole$Returns), stocks.whole$Week)
week.returns.table.prop <- prop.table(week.returns.table, 2)
plot(week.returns.table.prop[nrow(week.returns.table.prop),])
grid()

mday.returns.table <- table(sign(stocks.whole$Returns), stocks.whole$Day)
mday.returns.table.prop <- prop.table(mday.returns.table, 2)
plot(mday.returns.table.prop[nrow(mday.returns.table.prop),])
grid()

plot(table(cut(stocks$PE, pretty(stocks$PE))))

par(mfrow = c(1, 1))

down.ma10 <- stocks.whole[which(stocks.whole$Close < stocks.whole$MA10 & stocks.whole$PrevClose >= stocks.whole$PrevMA10),]
up.ma10 <- stocks.whole[which(stocks.whole$Close >= stocks.whole$MA10 & stocks.whole$PrevClose < stocks.whole$PrevMA10),]
down.up.ma10 <- rbind(down.ma10, up.ma10)
down.up.ma10.day.diff <- diff(index(down.up.ma10))
down.up.ma10.close.diff <- diff(down.up.ma10$Close)
steps <- seq(2, length(down.up.ma10.day.diff), 2)
down.up.ma10.day.diff <- down.up.ma10.day.diff[steps]
down.up.ma10.close.diff <- down.up.ma10.close.diff[steps]
down.up.ma10.day.table <- prop.table(table(down.up.ma10.day.diff))
down.up.ma10.close.table <- prop.table(table(down.up.ma10.close.diff))

down.ma20 <- stocks.whole[which(stocks.whole$Close < stocks.whole$MA20 & stocks.whole$PrevClose >= stocks.whole$PrevMA20),]
up.ma20 <- stocks.whole[which(stocks.whole$Close >= stocks.whole$MA20 & stocks.whole$PrevClose < stocks.whole$PrevMA20),]
down.up.ma20 <- rbind(down.ma20, up.ma20)
down.up.ma20.day.diff <- diff(index(down.up.ma20))
down.up.ma20.close.diff <- diff(down.up.ma20$Close)
steps <- seq(2, length(down.up.ma20.day.diff), 2)
down.up.ma20.day.diff <- down.up.ma20.day.diff[steps]
down.up.ma20.close.diff <- down.up.ma20.close.diff[steps]
down.up.ma20.day.table <- prop.table(table(down.up.ma20.day.diff))
down.up.ma20.close.table <- prop.table(table(down.up.ma20.close.diff))

down.ma30 <- stocks.whole[which(stocks.whole$Close < stocks.whole$MA30 & stocks.whole$PrevClose >= stocks.whole$PrevMA30),]
up.ma30 <- stocks.whole[which(stocks.whole$Close >= stocks.whole$MA30 & stocks.whole$PrevClose < stocks.whole$PrevMA30),]
down.up.ma30 <- rbind(down.ma30, up.ma30)
down.up.ma30.day.diff <- diff(index(down.up.ma30))
down.up.ma30.close.diff <- diff(down.up.ma30$Close)
steps <- seq(2, length(down.up.ma30.day.diff), 2)
down.up.ma30.day.diff <- down.up.ma30.day.diff[steps]
down.up.ma30.close.diff <- down.up.ma30.close.diff[steps]
down.up.ma30.day.table <- prop.table(table(down.up.ma30.day.diff))
down.up.ma30.close.table <- prop.table(table(down.up.ma30.close.diff))
# mu<-mean(returns, na.rm=T)
# sigma<-sd(returns, na.rm=T)
# par(mfrow = c(1, 2))
# qqnorm(as.numeric(returns), main="SPY empirical returns qqplot", cex.main = 0.8)
# qqline(as.numeric(returns), lwd=2)
# grid()
# 
# normal_data<-rnorm(nrow(returns), mean = mu, sd=sigma)
# qqnorm(normal_data, main="Normal returns", cex.main = 0.8)
# qqline(normal_data, lwd =2)
# grid()
