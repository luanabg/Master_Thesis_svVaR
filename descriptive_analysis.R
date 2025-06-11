# analysis of log returns: histogram and qq plot

source("load_data.R")

# USD
hist(log_returns_df[,2], breaks = 100)
# CHF
hist(log_returns_df[,3], breaks = 100)

# USD
qqnorm(log_returns_df[,2], main = "Q-Q Plot: EUR/USD")
qqline(log_returns_df[,2], col = "red")

# CHF
qqnorm(log_returns_df[,3], main = "Q-Q Plot: EUR/CHF")
qqline(log_returns_df[,3], col = "red")

library(mistr)
GNG_model_USD <- GNG_fit(log_returns_df[,2], start = c(break1 = -0.02, break2 = 0.02, mean = 0,
                                        sd = 0.016, shape1 = 0.16, shape2 = 0.11))
GNG_model_USD
plot(GNG_model_USD)

GNG_model_CHF <- GNG_fit(log_returns_df[,3], start = c(break1 = -0.02, break2 = 0.02, mean = 0,
                                                       sd = 0.016, shape1 = 0.16, shape2 = 0.11))
GNG_model_CHF
plot(GNG_model_CHF)


