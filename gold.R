init.environment <- function()
{
  setwd("C:/Users/nn1003/Documents/R/stock")
  library("ggplot2")
  library("zoo")
  library("quantmod")
  library("dplyr")
  library("lubridate")
}

load.data.df <- function(file)
{
  data <- read.table(file, sep=",", colClasses=c("character", "numeric"), col.names=c("date", "price"))
  
  return(data)
}

get.data.year <- function(data, year)
{
  days <- 240 * year
  
  t <- data[1: days,]
  rownames(t) <- t[[1]]
  t$date <- NULL
  
  return(t)
}

init.environment()
gold.whole.df <- load.data.df("gold.csv")

gold1 <- as.xts(get.data.year(gold.whole.df, 1))
gold2 <- as.xts(get.data.year(gold.whole.df, 2))
gold3 <- as.xts(get.data.year(gold.whole.df, 3))
gold <- gold1
