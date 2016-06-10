# a Discrete case
## p(a) = .5, p(b) = miu, p(c) = 2*miu, p(d) = .5 - 3*miu
## now we nor #a+#b = h, #c, #d
## when miu is known, #a = .5*h/(.5+miu), #b =  miu*h/(.5+miu)
## first we set the initial value of miu and #b from uniform distribution
h = 20
c = 10
d = 10
miu = runif(1,0,1/6)
b = round(runif(1,1,20))
iter = 1
nonstop = TRUE
while (nonstop) {
  # E-step, compute b when miu is given
  b = c(b, miu[iter]*h/(0.5+miu[iter]))
  # M-step, compute miu using the b got from the previous step
  miu = c(miu, (b[iter+1] + c)/(6*(b[iter+1]+c+d)))
  # record the num of iteration
  iter = iter + 1
  # break when the gap is relatively small
  nonstop = (miu[iter]-miu[iter-1] > 10^(-8))
}
print(cbind(miu,b))

# a mixture guassian distribution case
## for simplification, we consider two guassian distribution with same sigam
## the proportation of each cluster is .4 versus .6
## let miu1 = -5 and miu2 = 3
## generate the mixture sample
set.seed(1000)
Y = data.frame(p = runif(5000), x = rep(0, 5000))
Y[which(Y$p > 0.6), 2] = rnorm(nrow(Y[Y$p > 0.6, ]), -2, 2)
Y[which(Y$p <= 0.6), 2] = rnorm(nrow(Y[Y$p <= 0.6, ]), 3, 1)
Y[which(Y$p > 0.6), 3] = 0
Y[which(Y$p <= 0.6), 3] = 1
colnames(Y)[3] = 'z'
plot(Y$x, col = factor(Y$z), type = 'p')
hist(Y$x, freq = FALSE)
lines(density(Y$x), col = 'red', lwd = 2)
## EM Algorithm
m = 2
n = nrow(Y)
miu = runif(m)
sigma = runif(m)
alpha = c(0.2, 0.8)
prob = matrix(rep(0, n*m), ncol = m)
for (step in 1:1000) {
  # E-step
  for (j in 1:m) {
    prob[, j] = sapply(Y$x, dnorm, miu[j], sigma[j]) # compute the conditional prob
  }
  sumprob = rowSums(prob)
  prob = prob/sumprob # normalized prob
  oldmiu = miu
  oldsigma = sigma
  oldalpha = alpha
  
  # M-step
  for (j in 1:m) {
    p1 = sum(prob[, j])
    p2 = sum(prob[, j]*Y$x)
    miu[j] = p2/p1
    alpha[j] = p1/n
    p3 = sum(prob[, j]*(Y$x - miu[j]) ^ 2)
    sigma[j] = sqrt(p3/p1)
  }
  
  # break condition
  epsilon = 10 ^ -10
  if (sum(abs(miu - oldmiu)) < epsilon &
      sum(abs(sigma - oldsigma)) < epsilon &
      sum(abs(alpha - oldalpha)) < epsilon) break
  cat('step', step, 'miu', miu, 'sigma', sigma, 'alpha', alpha, '\n')
}

