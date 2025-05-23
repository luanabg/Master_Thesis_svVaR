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

# first example with 100 days in the past, 1 day is day of purchase, 102 day is day of VaR
prices_usd <- eur_usd[1:202]
ret_usd <- rep(prices_usd[1], 201) - prices_usd[2:202]

prices_chf <- eur_chf[1:202]
ret_chf <- rep(prices_chf[1], 201) - prices_chf[2:202]

returns <- cbind(ret_usd, ret_chf)

# returns of the first 101 days (100 days past data, one day to be analysed)
returns_df <- data.frame(date = index(returns), 
                         EURUSD = coredata(returns[,1]), 
                         EURCHF = coredata(returns[,2]))

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
# y_list <- split(returns_df[, 2:3], seq(nrow(returns_df)))
# y_list <- lapply(y_list, function(x) as.numeric(x))
# 
# plot(xs, main = "Example for y_2", xlim = c(-0.01, 0.3))
# abline(v = -y_list[[1]][1], col = "red")
# abline(a = -y_list[[1]][2], b = 0, col = "red")

y <- as.matrix(returns_df[,2:3])

# and we see that there are some points outside the thresholds

###############################
# First attempt VaR function
###############################

# xs are 10000 drawn from pi, y is the return vector of the respective day, 
# alpha confidence level
VaR_scoring_single <- function(xs, y, alpha = 0.05){
  #y <- as.matrix(y)
  
  # step 1: check if x_i in (-y_t + K) <=> x_i >= -y_t
  # list for y_2 to y_100 indicating how many xs >= -y_t
  in_yK <- apply(xs, 1, function(x) {
    if (all(x >= -y[-1,])) 1 else 0
  })
  
  # step 2: check if x_i belongs to D, i.e., if the change x_i to y_t cannot make y_t acceptable
  # <=> y_t + x_i < 0 (probability based on the passt 100 y)
  in_D <- apply(xs, 1, function(x) {
    sum_x_y <- y[1:100,] + x
    
    not_positive <- apply(y, 1, function(y) {
      if (any(x + y < 0)) 1 else 0
    })
    
    prob <- length(which(not_positive == 1))/100
    if (prob <= alpha) 1 else 0
  })
  
  # step 3: compute sets
  yK_without_D <- which(in_yK == 1 & in_D == 0)
  D_without_yK <- which(in_yK == 0 & in_D == 1)
  
  
  # step 4: compute and return scoring
  return(alpha*(1/length(xs[,1]))*length(yK_without_D) + (1-alpha)*(1/length(xs[,1]))*length(D_without_yK))
}

VaR_scoring_multiple <- function(xs, y, window_len = 101, alpha = 0.01) {
  # throw error in case window size cannot be reached
  if(nrow(y) <= window_len) {
    stop("Vector y needs to have more rows than window_len.")
  }
  
  len <- nrow(y)-window_len
  scorings <- vector(length = len)
  
  for (i in 1:(len+1)) {
    y_window <- y[i:(i+window_len-1),]
    print(i)
    scorings[i] <- VaR_scoring_single(xs = xs, y = y_window, alpha = alpha)
  }
  
  return(scorings)
}

test <- VaR_scoring_multiple(xs, y)
test


####### TEST REMOVE LATER

VaR_scoring_multiple_test <- function(xs, y, window_len = 100, alpha = 0.05) {
  len <- nrow(y)-window_len
  scorings <- vector(length = len)
  
  for (i in 1:(len+1)) {
    y_window <- y[i:(i+window_len-1),]
    print(y_window)
    #scorings[i] <- VaR_scoring_single(xs = xs, y = y_window, alpha = alpha)
  }
  
  #return(scorings)
}

y_test <- matrix(c(1,2,3,4,5,6,3,4,5,6,7,8), ncol = 2)
VaR_scoring_multiple_test(0, y_test, window_len = 3)
