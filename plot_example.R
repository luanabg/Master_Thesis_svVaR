# plot example

prices_usd <- eur_usd[101:200]
ret_usd <- rep(prices_usd[1], 99) - prices_usd[2:100]

prices_chf <- eur_chf[101:200]
ret_chf <- rep(prices_chf[1], 99) - prices_chf[2:100]

returns <- merge(ret_usd, ret_chf)

returns_df <- data.frame(date = index(returns), 
                         EURUSD = coredata(returns[,1]), 
                         EURCHF = coredata(returns[,2]))

ggplot(returns_df, aes(x = EURUSD.X.Adjusted, y = EURCHF.X.Adjusted)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "green", alpha = 0.2) +
  geom_point(aes(color = (EURUSD.X.Adjusted > 0 & EURCHF.X.Adjusted > 0)), alpha = 0.6) +
  guides(color = "none") +
  theme_minimal() +
  # Define colors: TRUE (inside green area) = blue, FALSE (outside) = red
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
  labs(title = "EURUSD vs EURCHF Returns", 
       x = "EURUSD Returns", 
       y = "EURCHF Returns") 


# now we see that clearly a lot of points are outside of K!!




# another thing

library(ggplot2)

ggplot(returns_df, aes(x = EURUSD.X.Adjusted, y = EURCHF.X.Adjusted)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "EURUSD vs EURCHF Returns", 
       x = "EURUSD Returns", 
       y = "EURCHF Returns") +
  theme_minimal()
