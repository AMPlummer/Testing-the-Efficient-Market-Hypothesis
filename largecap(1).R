# Load packages
library(ggplot2)
library(readxl)
library(quantmod)
library(pracma)
library(zoo)
library(tseries)
library(magrittr)
library(tidyverse)

# Set work directory
setwd("C:\\Users\\plua\\Desktop\\Academic Papers\\Efficiency\\LargeCap")

# Load data
data <- read.csv("largecap.csv")
# Data cleaning
data$Date <- as.Date(data$Date)
data$Adj.Close <- as.numeric(data$Adj.Close)
data$Volume <- as.numeric(data$Volume)
data$VolumeGrowth <- Delt(data$Volume, k = 12) * 100
data$StockReturns <- Delt(data$Adj.Close, k = 12) * 100
data$Loss <- ifelse(data$StockReturns < 0, "1", "0")

# Draw line plot of monthly Stock Returns of S&P/TSX Composite
fig <- ggplot(data, aes(x = Date, y = StockReturns, fill = Loss)) 
fig <- fig + geom_bar(stat = "identity") + theme_minimal() 
fig <- fig + scale_fill_manual(values = c("green", "red")) + theme(legend.position = "none")
fig <- fig + labs(title = "S&P/TSX Composite year-over-year Monthly Returns", subtitle = "1986 - 2021",
                  y = "Stock Market Returns")
fig
mean(data$StockReturns, na.rm = TRUE)
# Create dataframe of hurst exponent
returns <- data %>% select(c("Date", "StockReturns"))
returns <- read.zoo(subset(returns, !is.na(returns$StockReturns)))

hurstexp(returns)
new_data <- data.frame(rollapplyr(returns, 200, hurstexp))


new_data$Hrs <- as.numeric(new_data$Hrs)
new_data <- subset(new_data, select = -c(Hs, He, Hal, Ht))
new_data <- fortify(as.zoo(new_data))
# Draw More Plots
ggplot(new_data, aes(x = Index)) + geom_line(aes(y = Hrs), color = "cornflowerblue") +
  labs(title = "Estimating Market Efficiency in the S&P/TSX Composite", subtitle = "2002-2021", y = "Corrected R Over S Hurst Exponent", x = "Date")

ggplot(new_data, aes(x = Hrs)) + geom_histogram(alpha = 0.4, fill = "#842bd7", bins = 40) + geom_density(alpha = .2, fill = "#ff6600" ) +
  labs(title = "Distribution of Hurst Exponents", subtitle = "The Hurst Exponents Approximate a Bimodial Distribution", x =  "Corrected R Over S Hurst Exponent", y= "Count")

# Find summary statistics
Mean <- mean(new_data$Hrs)
Max <- max(new_data$Hrs)
Min <- min(new_data$Hrs)
m <- data.frame(Mean, Max, Min)
m


# Visualizing relationship between volume of stocks and hurst exponent

new_data$VolumeGrowth <- data$VolumeGrowth[212:nrow(data)]
new_data$StockReturns <- data$StockReturns[212:nrow(data)]

ggplot(new_data, aes(x = VolumeGrowth, y = Hrs)) + geom_point() + geom_smooth(method = lm) +
  labs(title = "Hurst Exponent Versus Market Volume", x = "Volume of Stocks Traded (Growth Rate)", y = "Corrected R Over S Hurst Exponent")


# Testing for Unit Roots

adf.test(new_data$Hrs) # Unit Root
adf.test(new_data$VolumeGrowth) # No Unit Root
adf.test(new_data$StockReturns)

# Creating linear regression

fit <- lm(formula = diff(new_data$Hrs) ~ diff(new_data$VolumeGrowth)) # Not statistically significant
summary(fit)
fit2 <- lm(formula = diff(new_data$Hrs) ~ diff(new_data$VolumeGrowth) + diff(new_data$StockReturns))
summary(fit2)

# No statistically significant relationship between market efficiency and volume
# Possible that we are excluding important variables, such as:
# (1) Information 
# (2) Risk

# Another explanation is that we didn't include lags of these variables. We should
# expect that market efficiency in the previous month is correltaed with market
# efficiency in the subsequent month

# acf(new_data$Hrs, lag.max = 36)

test <- new_data %>% subset(VolumeGrowth <= 400)
ggplot(test, aes(x = VolumeGrowth, y = Hrs)) + geom_point() + geom_smooth(method = lm)

# Creating linear regression

fit <- lm(formula = diff(test$Hrs) ~ diff(test$VolumeGrowth)) # Not statistically significant
summary(fit)
fit2 <- lm(formula = diff(test$Hrs) ~ diff(test$VolumeGrowth) + diff(test$StockReturns))
summary(fit2)
