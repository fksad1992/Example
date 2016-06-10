x = rexp(1000, 1)
plot(ecdf(x), do.point = FALSE)
v = seq(0, 6, len = 1000)
lines(v, pexp(v), col = 'steelblue', lwd = 2)
t = x + rnorm(1000, 0, .1)
lines(ecdf(t), col = 'red', lwd = 1, do.point = FALSE)
convolve(pnorm(sort(x), sd = 10000), pexp((knots(ecdf(x)))), type = 'open')

## simulation of beta-binomial distribution
bb = vector()
varhat = vector()
for(j in 1:100){
  for(i in 1:5000){
    p = rbeta(1, 1, 2)
    bb[i] = rbinom(1, 100, p)
  }
  varhat[j] = var(bb)
}
summary(varhat)
var(bb)
plot(ecdf(bb), do.point = FALSE, verticals = TRUE)
vartrue = 200*103/9/4
plot(varhat)
abline(a = vartrue, b = 0, col = 'steelblue', lwd = 2)

plot(ecdf(convolve(bb, rev(rnorm(5000)), type = 'o')))
