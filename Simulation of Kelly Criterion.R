# Construst the function of simulation
Kelly_sim = function(F, n, w , p, odds){
  # first construct the kelly function to simulate the gambling process
  # w means intial wealth, p means probablity of success
  # f means the fraction, odds is the net odds, n is the # of round you play
  Kelly = function(w, p, f, odds, n){
    Wealth = rep(0, n)
    u = runif(n)
    for(i in 1:n){
      w1 = w * f
      if(u[i] < p){
        Wealth[i] = w + w1 * odds
        w = w + w1 * odds
      } else {
        Wealth[i] = w - w1
        w = w -w1
      }
    }
    return(Wealth)
  }
  # second, we construct a function to realize the gambling process within 
  # different fractions
  library(ggplot2)
  library(tidyr)
  # S is a matrix to store the outcome
  S = matrix(0, ncol = length(F), nrow = n)
  # F is vector contains different fractions 
  for(i in F){
  loc = which(F == i)
  S[, loc] = Kelly(w = w, p = p, f = i, odds, n = n)
  }
  colnames(S) = c(F[-length(F)], 'Kelly')
  df = as.data.frame(S)
  gg = gather(df, key = Proportion, value = Wealth)
  Round = rep(1:n, length(F))
  gg = cbind(gg, Round)
  g = ggplot(gg, aes(x = Round, y = Wealth, colour = Proportion))
  g + geom_line(size = 1.2)
  }
# now we can simulate
set.seed(1234)
F = c(0.1, 0.4, 0.7, 0.9, (0.5 * (2 + 1) - 1) / 2)
Kelly_sim(F = F, n = 100, w = 1, p = 0.5, odds = 2)