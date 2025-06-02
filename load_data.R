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

library(MASS)

set.seed(123)

# only for direct use
# anyNA(returns_df$EURCHF.X.Adjusted)
# anyNA(returns_df$EURUSD.X.Adjusted)
# which(is.na(returns_df$EURUSD.X.Adjusted))

# there is one NA in the USD data row number 1142
returns_df <- returns_df[-(which(is.na(returns_df$EURUSD.X.Adjusted))),]

# empirical mean
mu <- c(mean(returns_df$EURCHF.X.Adjusted), mean(returns_df$EURUSD.X.Adjusted))
# empirical var cov matrix
Sigma <- cov(returns_df[,2:3])

# draw 10000 xs from multivariate normal
xs <- mvrnorm(n = 10000, mu = mu, Sigma = Sigma)

y <- as.matrix(returns_df[,2:3])

# check for completeness
is_complete <- function() {
  !anyNA(xs)
  !anyNA(y)
}
