library(mvtnorm)
library(copula)
library(mistr)

source("load_data.R")
source("cdf_helpers.R")

# check completeness
is_complete()

###############################
# First attempt VaR function
###############################

# xs are 10000 drawn from pi, y is the return vector of the respective day, 
# alpha confidence level, dist_type shows whether empirical or theoretical y are used
VaR_scoring_single <- function(xs, y, window_len = 101, alpha = 0.05, dist_type = "empirical"){
  
  # step 1: check if x_i in (-y_t + K) <=> x_i >= -y_t
  # list for y_2 to y_100 indicating how many xs >= -y_t
  in_yK <- apply(xs, 1, function(x) {
    if (all(x >= -y[-1,])) 1 else 0
  })
  
  # step 2: check if x_i belongs to D, i.e., if the change x_i to y_t cannot make y_t acceptable
  # <=> y_t + x_i < 0 (probability based on the past 100 y)
  in_D <- apply(xs, 1, function(x) {
    
    if (dist_type == "normal") {
      mu_t <- colMeans(y[1:window_len,])
      sigma_t <- cov(y[1:(window_len-1),])
      
      # compute exact normal probability with empirical parameters
      # this is the normal probability of P(Y_1 + x_1 < 0, Y_2 + x_2 < 0), so 'not positive'
      prob <- pmvnorm(lower = rep(-Inf, 2), upper = -x, mean = mu_t, sigma = sigma_t)
    
    } else if (dist_type == "gpd-normal"){
      d <- ncol(y)
      
      u <- numeric(d)
      
      for (i in 1:d) {
        gpd_fit <- GNG_fit(log_returns_df[,(i+1)], start = c(break1 = -0.02, break2 = 0.02, mean = 0,
                                                         sd = 0.016, shape1 = 0.16, shape2 = 0.11))
        
        # we pass -x[i] here for P(y < -x) and again 'not positive'
        u[i] <- univariate_mixed_cdf(-x[i], gpd_fit)
      }
      
      cor_matrix <- cor(y[1:window_len, ])
      cop <- normalCopula(param = cor_matrix[lower.tri(cor_matrix)], dim = d, dispstr = "un")
      
      prob <- pCopula(u, cop)
      
    } else if (dist_type == "empirical") {
      not_positive <- apply(y, 1, function(y) {
        if (any(x + y < 0)) 1 else 0
      })
      
      prob <- length(which(not_positive == 1))/(window_len-1)
    } else { # troubleshooting
      stop("Invalid argument dist_type, can be either 'empirical', 'normal' or 'gpd-normal'.")
    }
    
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
  if(nrow(y) < (window_len+1)) {
    stop("Vector y needs to have more rows than window_len.")
  }
  
  len <- nrow(y)-window_len
  scorings <- vector(length = len)
  
  for (i in 1:(len+1)) {
    y_window <- y[i:(i+window_len-1),]
    print(i)
    scorings[i] <- VaR_scoring_single(xs = xs, y = y_window, window_len = window_len, alpha = alpha, dist_type = dist_type)
  }
  
  
  return(scorings)
}


##################### RESULTS ##################### 

# use 250 days for the window using "the past year" to have enough tail 
# observations to fit the gpd
start_time <- Sys.time()
empirical_scorings <- VaR_scoring_multiple(xs[1:300,], y[1:500,], window_len = 251)

normal_scorings <- VaR_scoring_multiple(xs[1:300,], y[1:500,], window_len = 251, dist_type = "normal")

gpd_normal_scorings <- VaR_scoring_multiple(xs[1:300,], y[1:500,], window_len = 251, dist_type = "gpd-normal")
end_time <- Sys.time()

# here, "less" means better!
t.test(empirical_scorings, normal_scorings, alternative = "less", paired = TRUE)
t.test(empirical_scorings, gpd_normal_scorings, alternative = "less", paired = TRUE)

matplot(log_returns_df$date[251:500], cbind(empirical_scorings, normal_scorings), type = "l", lty = 1, 
        col = c("red", "blue"), xlab = "Date", 
        ylab = "Scorings", main = "Scoring values: Empirical vs. Normal")
legend("topright", legend = c("Empirical", "Normal"), 
       col = c("red", "blue"), 
       lty = 1)

matplot(log_returns_df$date[251:500], cbind(empirical_scorings, gpd_normal_scorings), type = "l", lty = 1, 
        col = c("red", "blue"), xlab = "Date", 
        ylab = "Scorings", main = "Scoring values: Empirical vs. GPD-Normal")
legend("topright", legend = c("Empirical", "GPD-Normal"), 
       col = c("red", "blue"), 
       lty = 1)
