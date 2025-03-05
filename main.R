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
eur_usd <- na.omit(eur_usd)
eur_chf <- na.omit(eur_chf)
