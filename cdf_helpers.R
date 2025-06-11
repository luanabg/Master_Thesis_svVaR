library(copula)
library(evir)

univariate_hybrid_cdf <- function(x, mean, sd, lower, upper, scale_u, shape_u, scale_l, shape_l) {
  # error handling
  # vital parameters
  if (is.na(x) || is.na(mean) || is.na(sd) || sd <= 0) {
    stop("NA or negative sd encountered in CDF inputs")
  }
  
  # fall back to normal if gpd fails‚
  if (any(is.na(c(scale_u, shape_u, scale_l, shape_l)))) {
    print("A")
    return(pnorm(x, mean = mean, sd = sd))
  }
  
  if (x > upper) {
    # '1 - ' because the default is the lower tail (?)
    if ( (shape_u >= 0 && x >= upper) ||  (shape_u < 0 && upper <= x && x <= (upper-sd/shape_u)) ) {
      return(1 - pgpd(q = x, mu = upper, beta = scale_u, xi = shape_u))
    } else {
      return(pnorm(x, mean = mean, sd = sd))
    }
    
  } else if (x < lower) {
    if ( (shape_u >= 0 && x >= upper) ||  (shape_u < 0 && upper <= x && x <= (upper-sd/shape_u)) ) {
      return(pgpd(q = x, mu = lower, beta = scale_l, xi = shape_l))
    } else{
      return(pnorm(x, mean = mean, sd = sd))
    }
  } else {
    return(pnorm(x, mean = mean, sd = sd))
  }
}

# here, ys will be passed as the returns of the last 100 days
# 'threshold' should be in the form of 'alpha', so 5% tail size means 
# 'threshold = 0.05'
get_gpd_params <- function(ys, threshold) {
  upper <- quantile(ys, 1-threshold)
  lower <- quantile(ys, threshold)
  upper_fit <- gpd(ys, threshold = upper)
  lower_fit <- gpd(-ys, threshold = -lower)
  
  return(list(upper_scale = upper_fit$par.ests["beta"],
              upper_shape = upper_fit$par.ests["xi"],
              lower_scale = lower_fit$par.ests["beta"],
              lower_shape = lower_fit$par.ests["xi"],
              upper = upper, lower = lower
              ))
}
