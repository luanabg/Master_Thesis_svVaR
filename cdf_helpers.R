library(mistr)

extract_params <- function(fitted_model) {
  break1 <- fitted_model$params$breakpoints[1]
  break2 <- fitted_model$params$breakpoints[2]
  mean <- fitted_model$params$coef["mean"]
  sd <- fitted_model$params$coef["sd"]
  shape1 <- fitted_model$params$coef["shape1"]
  shape2 <- fitted_model$params$coef["shape2"]
  loc1 <- fitted_model$params$coef["loc1"]
  loc2 <- fitted_model$params$coef["loc2"]
  scale1 <- fitted_model$params$coef["scale1"]
  scale2 <- fitted_model$params$coef["scale2"]
  weight1 <- fitted_model$params$weights[1]
  weight2 <- fitted_model$params$weights[2]
  weight3 <- fitted_model$params$weights[3]
  
  return(list(break1 = break1, break2 = break2, mean = mean, sd = sd, 
              shape1 = shape1, shape2 = shape2, loc1 = loc1, loc2 = loc2, 
              scale1 = scale1, scale2 = scale2, weight1 = weight1, 
              weight2 = weight2, weight3 = weight3))
}

univariate_mixed_cdf <- function(u, fitted_model) {
  # extract parameters from fitted GNG model
  params <- extract_params(fitted_model)
  
  if (u <= params$break1) {
    # lower tail
    return(pGPD(u, loc = params$loc1, scale = params$scale1, 
                shape = params$shape1, lower.tail = TRUE))
  } else if (u < params$break2) {
    # center
    return(pnorm(u, mean = params$mean, sd = params$sd))
  } else {
    # upper tail
    return(pGPD(u, loc = params$loc2, scale = params$scale2, 
                shape = params$shape2, lower.tail = FALSE))  
  }
}

############## DEPRECATED ############################

# univariate_hybrid_cdf <- function(x, mean, sd, lower, upper, scale_u, shape_u, scale_l, shape_l) {
#   # error handling
#   # vital parameters
#   if (is.na(x) || is.na(mean) || is.na(sd) || sd <= 0) {
#     stop("NA or negative sd encountered in CDF inputs")
#   }
#   
#   # fall back to normal if gpd fails‚
#   if (any(is.na(c(scale_u, shape_u, scale_l, shape_l)))) {
#     print("A")
#     return(pnorm(x, mean = mean, sd = sd))
#   }
#   
#   if (x > upper) {
#     # '1 - ' because the default is the lower tail (?)
#     if ( (shape_u >= 0 && x >= upper) ||  (shape_u < 0 && upper <= x && x <= (upper-sd/shape_u)) ) {
#       return(1 - pgpd(q = x, mu = upper, beta = scale_u, xi = shape_u))
#     } else {
#       return(pnorm(x, mean = mean, sd = sd))
#     }
#     
#   } else if (x < lower) {
#     if ( (shape_u >= 0 && x >= upper) ||  (shape_u < 0 && upper <= x && x <= (upper-sd/shape_u)) ) {
#       return(pgpd(q = x, mu = lower, beta = scale_l, xi = shape_l))
#     } else{
#       return(pnorm(x, mean = mean, sd = sd))
#     }
#   } else {
#     return(pnorm(x, mean = mean, sd = sd))
#   }
# }
# 
# # here, ys will be passed as the returns of the last 100 days
# # 'threshold' should be in the form of 'alpha', so 5% tail size means 
# # 'threshold = 0.05'
# get_gpd_params <- function(ys, threshold) {
#   upper <- quantile(ys, 1-threshold)
#   lower <- quantile(ys, threshold)
#   upper_fit <- gpd(ys, threshold = upper)
#   print(summary(-ys))
#   print(-lower)
#   lower_fit <- gpd(-ys, threshold = -lower)
#   
#   return(list(upper_scale = upper_fit$par.ests["beta"],
#               upper_shape = upper_fit$par.ests["xi"],
#               lower_scale = lower_fit$par.ests["beta"],
#               lower_shape = lower_fit$par.ests["xi"],
#               upper = upper, lower = lower
#               ))
# }
