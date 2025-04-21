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
prices_usd <- eur_usd[1:101]
ret_usd <- rep(prices_usd[1], 99) - prices_usd[2:100]

prices_chf <- eur_chf[1:101]
ret_chf <- rep(prices_chf[1], 99) - prices_chf[2:100]

returns <- merge(ret_usd, ret_chf)

returns_df <- data.frame(date = index(returns), 
                         EURUSD = coredata(returns[,1]), 
                         EURCHF = coredata(returns[,2]))

library(ggplot2)

ggplot(returns_df, aes(x = EURUSD.X.Adjusted, y = EURCHF.X.Adjusted)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "EURUSD vs EURCHF Returns", 
       x = "EURUSD Returns", 
       y = "EURCHF Returns") +
  theme_minimal()


# first example with this window: we have t = 1, ..., 100
library(MASS)

set.seed(123)

# empirical mean
mu <- c(mean(returns_df$EURCHF.X.Adjusted), mean(returns_df$EURUSD.X.Adjusted))
# empirical var cov matrix
Sigma <- cov(returns_df[,2:3])

# draw 10000 xs from multivariate normal
xs <- mvrnorm(n = 10000, mu = mu, Sigma = Sigma)

# save returns in a list of vectors
y_list <- split(returns_df[, 2:3], seq(nrow(returns_df)))
y_list <- lapply(y_list, function(x) as.numeric(x))




plot(xs, main = "Example for y_2", xlim = c(-0.01, 0.3))
abline(v = -y_list[[1]][1], col = "red")
abline(a = -y_list[[1]][2], b = 0, col = "red")

# and we see that there are some points outside the thresholds

# the set D (???) probabilities of x + y_t >= 0
D_list <- lapply(in_set, function(x) (10000-x)/10000)

# check which ones are outside the set D (???)
which(D_list >= 0.05)

# and we see that all xs are in D (???)



# xs are 10000 drawn from pi, y is the return vector of the respective window, 
# alpha confidence level
VaR_scoring <- function(xs, y, alpha = 0.05){
  
  # step 1: check if x_i in (-y_t + K) <=> x_i >= -y_t
  # list for y_2 to y_100 indicating how many xs >= -y_t
  in_set <- lapply(y_list, function(y) {
    sum(apply(xs, 1, function(x_row) {
      all(x_row >= -y)
    }))
  })
  
  # the set D probabilities of x + y_t < 0
  D_list <- lapply(in_set, function(x) ifelse(1-x/10000 >= alpha, 10000-x, 0) )
  
}


