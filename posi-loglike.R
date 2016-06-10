#poisson loglike function
poissonlik = function(sita, x){
  n = length(x)
  LogL = -n * sita + sum(x) * log(sita) - sum(log(factorial(x))) 
  return(LogL)
}

#generate observed data
set.seed(123)
x1 = rpois(100,5) 
x2 = rpois(1000,5)
x3 = rpois(5000,5)

#generate sequence of lambda
lambda = seq(from = 1, to = 9, length = 1000) 
#generate the value of possion loglike function
y1 = poissonlik(sita = lambda, x = x1)
y2 = poissonlik(sita = lambda, x = x2)
y3 = poissonlik(sita = lambda, x = x3)

#generate the dataset for graph
y = c(y1, y2, y3)
nf = length(lambda)
n = c(rep(100,nf), rep(1000,nf), rep(5000,nf))
p = data.frame(x = rep(lambda, 3), y = y, n = n)   

#use ggplot2 to plot
library(ggplot2)
g = ggplot(p, aes(x, y, colour = factor(n)))
g + geom_line(size = 1.2) + geom_vline(xintercept = 5.0, size = 1.2)
