init.environment <- function()
{
  setwd("C:/Users/nn1003/Documents/R/stock")
  library("ggplot2")
  library("zoo")
  library("quantmod")
  library("dplyr")
  library("lubridate")
  library("sqldf")
  library("Hmisc")
}

get.percentile <- function(x, percentile.length)
{
  percentiles = c()
  
  for (i in 1:length(x)) {
    if(i < percentile.length) {
      percentiles[i] <- NA
    } else {
      current <- x[i]
      start <- i + 1 - percentile.length
      end <- i
      percentile.func <- ecdf(x[start:end])
      current.percentile <- round(percentile.func(current), 2)
      percentiles[i] <- current.percentile
    }
  }
  
  return(percentiles)
}

core.data <- function(x)
{
  data = x[, c("Close", "PE", "Returns", "CP", "PEP", "PBP", "VP")]
  
  return(data)
}

load.stocks.data <- function(file) { 
  data <- read.table(file, header= TRUE, sep=",", stringsAsFactors = FALSE, 
          colClasses=c("factor", "NULL", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "NULL", "NULL", "NULL", "NULL", "NULL", "numeric" ), 
          col.names=c("Code", "Name", "Date", "Open", "High", "Low", "Close", "Volumn", "Amount", "Change", "Turnover", "PE", "PB", "Average", "AmountPercentage", "HQLTSZ", "AHQLTSZ", "Payout", "IR")) 
  data <- sqldf("select Date, Open, Close, High, Low, Volumn, PE, PB, IR from data order by Date")
  percentiles <- apply(data[, c("Close", "PE", "PB", "Volumn")], 2, get.percentile, percentile.length = 240 )
  
  data <- transform(data,
                    Returns = round((Close - lag(Close)) / lag(Close), 4) * 100,
                    CP = percentiles[, 1],
                    PEP = percentiles[, 2],
                    PBP = percentiles[, 3],
                    VP = percentiles[, 4]
                    )
  
  rownames(data) <- data[[1]]
  data$Date <- NULL
  
  return(data)
}

get.data.year <- function(data, year)
{
  end <- nrow(data)
  start <- end + 1 - year * 240
  
  return(data[start : end,])
}

init.environment()
stocks.whole <- load.stocks.data("801813.csv")
stocks <- core.data(get.data.year(stocks.whole, 1))