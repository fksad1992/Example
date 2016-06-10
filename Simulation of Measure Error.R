Attenuation = function(s, n){
x = matrix(rnorm(s * n, 0, 1), nrow = s)
epsilon1 = matrix(rnorm(s * n, 0, 1), nrow = s)
epsilon2 = matrix(rnorm(s * n, 0, 1), nrow = s)
y = 1 + 2 * x + epsilon1 
z = x + epsilon2
b_hat = rep(0, n)
for (i in 1:n){
  b_hat[i] = coef(lm(y[,i] ~ z[,i]))[2]
} 
summary(b_hat)
}
Attenuation(50, 10000)
