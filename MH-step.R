
## p_gamma(jump distribution)
p_gamma = function(gamma, B_gamma, R, tau_gamma, M_gamma){
  sum = 0
  for(i in 1:nrow(B_gamma)){
    s = t(gamma)%*%B_gamma[i, ]
    else{
    t = (R[i]/s)^2
    sum = sum + 2*log(s) + t
  }
  exp(-.5*sum - (1/(2*tau_gamma))*t(gamma)%*%M_gamma%*%gamma)
  }
}

##generate the covariance matrix for MH step
Sigma_gamma = function(B_gamma, R, tau_gamma, M_gamma){
  h = solve((gen_M(B_gamma, R))/2 + M_gamma/tau_gamma)
  return(h)
}

##
gen_M = function(x, R){
  sum = matrix(0, nrow = ncol(x), ncol = ncol(x))
  for(i in 1:nrow(x)){
    a = x[i, ] %*% t(x[i, ]) * (R[i]^2)
    sum = sum + a
  }
  return(sum)
}


## the MH step function
library(mvtnorm)
MHstep = function(gamma, B, R, tau_gamma, M_gamma, delta_gamma){
  sigma = Sigma_gamma(B, R, tau_gamma, M_gamma)
  gamma_can = t(rmvnorm(1, gamma, sigma*delta_gamma))
  h = p_gamma(gamma, B, R, tau_gamma, M_gamma)
  accept_rate = min(1, p_gamma(gamma_can, B, R, tau_gamma, M_gamma)/h)
  if(is.na(accept_rate) == TRUE){gamma_return = rep(0, ncol(B))}
  else{
    u = runif(1)
    if(u <= accept_rate){
      gamma_return = gamma_can
    }
    else{
      gamma_return = gamma
    }
  }
  return(gamma_return)
}