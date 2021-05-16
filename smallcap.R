# Load packages
library(ggplot2)
library(zoo)
library(quantmod)
library(pracma)
library(tidyverse)
library(tseries)
# Set work directory
setwd("")

# Load data
data <- read.csv("S&P_TSX Canadian Small Cap Historical Data.csv")

# Data cleaning
data <- map_df(data, rev)
data$ï..Date <- seq(as.Date("1999-6-1"), as.Date("2021-4-1"), by = "months")
data$Price <- as.numeric(data$Price)


for(x in 1:nrow(data))
{
  if(grepl("M",data$Vol.[x]))
  {
    data$Vol.[x] <- as.numeric(gsub("M","",data$Vol.[x])) * 1000000
  }
  
  else if (grepl("B", data$Vol.[x]))
  {
    data$Vol.[x] <- as.numeric(gsub("B","",data$Vol.[x])) * 1000000000
  }
}
data$Vol. <- as.numeric(data$Vol.)
data$VolumeGrowth <- Delt(data$Vol., k = 12) * 100
data$StockReturns <- Delt(data$Price, k = 12) * 100
data$Loss <- ifelse(data$StockReturns < 0, "1", "0")

# Draw line plot of Monthly Stock Returns of S&P/TSX Composite
fig <- ggplot(data, aes(x = ï..Date, y = StockReturns, fill = Loss)) 
fig <- fig + geom_bar(stat = "identity") + theme_minimal() 
fig <- fig + scale_fill_manual(values = c("green", "red")) + theme(legend.position = "none")
fig <- fig + labs(title = "S&P/TSX SmallCap Index year-over-year Monthly Returns", subtitle = "1999 - 2021",
                  y = "Stock Market Returns", x= "Date")
fig
mean(data$StockReturns, na.rm = TRUE)
# Create dataframe of hurst exponent 
returns <- data %>% select(c("ï..Date", "StockReturns"))
returns <- read.zoo(subset(returns, !is.na(returns$StockReturns)))
hurstexp(returns)
new_data <- data.frame(rollapplyr(returns, 200, hurstexp))

new_data$Hrs <- as.numeric(new_data$Hrs)
new_data <- subset(new_data, select = -c(Hs, He, Hal, Ht))
new_data <- fortify(as.zoo(new_data))
# Draw More Plots
ggplot(new_data, aes(x = Index)) + geom_line(aes(y = Hrs), color = "cornflowerblue") +
  labs(title = "Estimating Market Efficiency in the S&P/TSX Smallcap Index", subtitle = "2017-2021", y = "Corrected R Over S Hurst Exponent", x = "Date")

# Market efficiency in small cap stocks is less subject to left tail risks. 

ggplot(new_data, aes(x = Hrs)) + geom_histogram(alpha = 0.4, fill = "#842bd7", bins = 40) + geom_density(alpha = .2, fill = "#ff6600" ) +
  labs(title = "Distribution of Hurst Exponents", subtitle = "The Hurst Exponents Approximate a Normal Distribution", x =  "Corrected R Over S Hurst Exponent", y= "Count")


# Find summary statistics
Mean <- mean(new_data$Hrs)
Max <- max(new_data$Hrs)
Min <- min(new_data$Hrs)
m <- data.frame(Mean, Max, Min)
m
new_data[which(new_data$Hrs == min(new_data$Hrs)), ]
new_data[which(new_data$Hrs == max(new_data$Hrs)), ]

# Visualizing relationship between volume of stocks and hurst exponent

new_data$VolumeGrowth <- data$VolumeGrowth[212:nrow(data)]
new_data$StockReturns <- data$StockReturns[212:nrow(data)]

ggplot(new_data, aes(x = VolumeGrowth, y = Hrs)) + geom_point(alpha = .6, color = "darkred") + geom_smooth(method = lm) +
  labs(title = "Hurst Exponent versus Market Volume", y = "Corrected R Over S Hurst Exponent", x = "Volume of Stocks Traded (Growth Rate)")

test <- new_data %>% filter(VolumeGrowth <= 75 & VolumeGrowth >= -75)

ggplot(test, aes(x = VolumeGrowth, y = Hrs)) + geom_point(alpha = .6, color = "darkred") + geom_smooth(method = lm) +
  labs(title = "Hurst Exponent versus Market Volume", y = "Corrected R Over S Hurst Exponent", x = "Volume of Stocks Traded (Growth Rate)")
# Testing for Unit Roots in new_data

adf.test(new_data$Hrs) # Unit Root
adf.test(new_data$VolumeGrowth) # No Unit Root
adf.test(new_data$StockReturns)

# Testing for Unit Roots in Test

adf.test(test$Hrs) # Unit Root

adf.test(test$StockReturns)

# Creating linear regression

# Again, there doesn't appear to be a significant relationship between
# the volume of stocks traded and market efficiency
fit <- lm(formula = diff(new_data$Hrs) ~ diff(new_data$VolumeGrowth)) # Not statistically significant
summary(fit)
fit2 <- lm(formula = diff(new_data$Hrs) ~ diff(new_data$VolumeGrowth) + diff(new_data$StockReturns))
summary(fit2)


fit <- lm(formula = diff(test$Hrs) ~ diff(test$VolumeGrowth)) # Not statistically significant
summary(fit)
fit2 <- lm(formula = diff(test$Hrs) ~ diff(test$VolumeGrowth) + diff(test$StockReturns))
summary(fit2)

