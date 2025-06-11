# load the data

library(quantmod)
env <- new.env()

start <- "2015-01-01"
end <- "2025-01-01"

getSymbols("EURUSD=X", 
           env = env, 
           from = start, 
           to = end, 
           source = "yahoo")
getSymbols("EURCHF=X", 
           env = env, 
           from = start, 
           to = end, 
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
log_ret_usd <- diff(log(eur_usd))[-1]

log_ret_chf <- diff(log(eur_chf))[-1]

log_returns <- cbind(log_ret_usd, log_ret_chf)

# returns of the first 101 days (100 days past data, one day to be analysed)
log_returns_df <- data.frame(date = index(log_returns), 
                         EURUSD = coredata(log_returns[,1]), 
                         EURCHF = coredata(log_returns[,2]))

library(MASS)

set.seed(123)

# only for direct use
# anyNA(log_returns_df$EURCHF.X.Adjusted)
# anyNA(log_returns_df$EURUSD.X.Adjusted)
# which(is.na(log_returns_df$EURUSD.X.Adjusted))

# there is one NA in the USD data row number 1142
log_returns_df <- log_returns_df[-(which(is.na(log_returns_df$EURUSD.X.Adjusted))),]

# empirical mean
mu <- c(mean(log_returns_df$EURCHF.X.Adjusted), mean(log_returns_df$EURUSD.X.Adjusted))
# empirical var cov matrix
Sigma <- cov(log_returns_df[,2:3])

# draw 10000 xs from multivariate normal
xs <- mvrnorm(n = 10000, mu = mu, Sigma = Sigma)

y <- as.matrix(log_returns_df[,2:3])

# check for completeness
is_complete <- function() {
  !anyNA(xs)
  !anyNA(y)
}
