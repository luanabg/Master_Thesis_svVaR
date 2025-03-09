# load the data

library(quantmod)
env <- new.env()
getSymbols("EURUSD=X", 
           env = env, 
           from = "2015-01-01", 
           to = "2025-01-01", 
           source = "yahoo")
getSymbols("EURCHF=X", 
           env = env, 
           from = "2015-01-01", 
           to = "2025-01-01", 
           source = "yahoo")

eur_usd <- env$`EURUSD=X`
eur_chf <- env$`EURCHF=X`

# we have four and three days with NA's:
length(eur_usd[is.na(eur_usd$`EURUSD=X.Close`)]$`EURUSD=X.Close`)
length(eur_chf[is.na(eur_chf$`EURCHF=X.Close`)]$`EURCHF=X.Close`)

# since it's only four/three days we can safely omit them
eur_usd <- na.omit(Ad(eur_usd))
eur_chf <- na.omit(Ad(eur_chf))

# first example with 100 days
prices_usd <- eur_usd[1:100]
ret_usd <- rep(prices_usd[1], 99) - prices_usd[2:100]

prices_chf <- eur_chf[1:100]
ret_chf <- rep(prices_chf[1], 99) - prices_chf[2:100]

returns <- merge(ret_usd, ret_chf)

# Simple scatter plot
plot(returns[,1], returns[,2], 
     main = "Scatter Plot of AAPL vs MSFT Returns", 
     xlab = "AAPL Returns", ylab = "MSFT Returns", 
     col = "blue", pch = 16)

library(ggplot2)

# Convert xts to data.frame for ggplot2
returns_df <- data.frame(date = index(returns), 
                         EURUSD = coredata(returns[,1]), 
                         EURCHF = coredata(returns[,2]))

# Plot
ggplot(returns_df, aes(x = EURUSD.X.Adjusted, y = EURCHF.X.Adjusted)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "EURUSD vs EURCHF Returns", 
       x = "EURUSD Returns", 
       y = "EURCHF Returns") +
  theme_minimal()

# Find the point with the smallest x (AAPL return) and smallest y (MSFT return)
min_x <- min(returns[,1])  # Smallest AAPL return
min_y <- min(returns[,2])  # Smallest MSFT return

# Find the row that matches both conditions
bottom_left_point <- returns[returns[,1] == min_x & returns[,2] == min_y, ]

# Print the smallest point
print(bottom_left_point)

# the smallest point is at (0.0009214878      0.0006899834), 2015-01-02


