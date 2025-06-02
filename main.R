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

# returns (normal not log!)
ret_usd <- diff(eur_usd)[-1]

ret_chf <- diff(eur_chf)[-1]

returns <- cbind(ret_usd, ret_chf)

# returns of the first 101 days (100 days past data, one day to be analysed)
returns_df <- data.frame(date = index(returns), 
                         EURUSD = coredata(returns[,1]), 
                         EURCHF = coredata(returns[,2]))

# first example with this window: we have t = 1, ..., 100
library(MASS)

set.seed(123)

anyNA(returns_df$EURCHF.X.Adjusted)
anyNA(returns_df$EURUSD.X.Adjusted)
which(is.na(returns_df$EURUSD.X.Adjusted))

# there is one NA in the USD data row number 1142
returns_df <- returns_df[-(which(is.na(returns_df$EURUSD.X.Adjusted))),]

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

# check for completeness
anyNA(xs)
anyNA(y)

###############################
# First attempt VaR function
###############################

# xs are 10000 drawn from pi, y is the return vector of the respective day, 
# alpha confidence level, dist_type shows whether empirical or theoretical y are used
VaR_scoring_single <- function(xs, y, alpha = 0.05, dist_type = "empirical"){
  # preprocess y to be either empirical or theoretical
  if (dist_type == "theoretical") {
    # for nor mvtnorm TODO adapt possibly to new distribution
    N <- nrow(y)
    y <- mvrnorm(n = N, mu = mu, Sigma = Sigma)
  } else if (dist_type != "empirical" & dist_type != "theoretical") {
    stop("Invalid argument dist_type, can be either 'empirical' or 'theoretical'.")
  }
  
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

VaR_scoring_multiple <- function(xs, y, window_len = 101, alpha = 0.01, dist_type = "empirical") {
  # throw error in case window size cannot be reached
  if(nrow(y) <= window_len) {
    stop("Vector y needs to have more rows than window_len.")
  }
  
  len <- nrow(y)-window_len
  scorings <- vector(length = len)
  
  for (i in 1:(len+1)) {
    y_window <- y[i:(i+window_len-1),]
    print(i)
    scorings[i] <- VaR_scoring_single(xs = xs, y = y_window, alpha = alpha, dist_type = dist_type)
  }
  
  return(scorings)
}


##################### RESULTS ##################### 

empirical_scorings <- VaR_scoring_multiple(xs, y)

theoretical_scorings <- VaR_scoring_multiple(xs, y, dist_type = "theoretical")

t.test(theoretical_scorings, test, alternative = "less", paired = TRUE)
