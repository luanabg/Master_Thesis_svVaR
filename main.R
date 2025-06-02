source("load_data.R")

# check completeness
is_complete()

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
