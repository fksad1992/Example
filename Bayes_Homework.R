######################################################
##                 Bayes Homework                   ##
######################################################
## Author: Qian Yixin
## Date: 2016.05
## Version: 1.0.0
## Built: R 3.2.5; Windows7

# Exercise One
## Bayesian Logistic Regression based on MCMC
## Important Dataset
library(mcmc)
data(logit)
## Ordinal Logit Model
Ordinal_model = glm(y ~ x1+x2+x3+x4, family = 'binomial', data = logit, x = TRUE)
summary(Ordinal_model)
Ordinal_model$x


x <- Ordinal_model$x
y <- Ordinal_model$y
beta = Ordinal_model$coef
logpost <- function(beta, x, y) {
    z <- as.numeric(x %*% beta)
    logp <-  -log1p(exp(-z))
    logq <- -log1p(exp(z))
    logl <- sum(logp[y == 1]) + sum(logq[y == 0])
    return(logl - sum(beta^2)/8)
}
set.seed(42)
MHsample = metrop(logpost, initial = rep(0, 5), nbatch = 5000, x = x, y = y, scale = .4)
summary(MHsample)
MHsample$accept
plot(ts(MHsample$batch))



############
logitlogl=function(beta,y,X){
    if (is.matrix(beta) == FALSE) beta=as.matrix(t(beta))
    n = dim(beta)[1]
    pll = rep(0, n)
    for (i in 1:n){
        lF1 = plogis(X %*% beta[i,], log = T)
        lF2 = plogis(-X %*% beta[i,], log = T)
        pll[i] = sum(y*lF1 + (1 - y)*lF2)
    }
    pll
}
logitlogl(beta, y, x)
logitlogpost = function(beta, y, X){
    return(logitlogl(beta, y, X) - sum(beta^2)/8)
}
library(mgcv)
MHlogit=function(initial, niter, y, X, scale){
    p = dim(X)[2]
    beta = matrix(0, niter, p)
    beta[1, ] = initial
    accept = 0
    for (i in 2:niter){
        tildebeta = rmvn(1, beta[i-1, ], (scale^2)*diag(1, p))
        llr = logitlogpost(tildebeta, y, X) - logitlogpost(beta[i-1, ], y, X)
        if (runif(1) <= exp(llr)) {
            beta[i, ] = tildebeta
            accept = accept + 1
        }
        else beta[i, ] = beta[i-1, ]
    }
    return(list(beta = beta, accept = accept/niter))
}
set.seed(42)
ti = MHlogit(rep(0, 5), 5000, y, x, .4)
plot(ts(ti$beta))
ti$accept
MHsample$accept

MH1 = apply(ti$beta[2000:5000, ], 2, mean)
MH2 = apply(MHsample$batch[2000:5000, ], 2, mean)

MHsd1 = apply(ti$beta[2000:5000, ], 2, sd)
MHsd2 = apply(MHsample$batch[2000:5000, ], 2, sd)
beta_interval = function(mean, sd){
    quant = c(.01, .05, .25, .5, .75, .95, .99)
    q = matrix(NA, nrow = length(mean), ncol = length(quant))
    for(i in 1:length(mean)) q[i, ] = qnorm(quant, mean[i], sd[i])
    return(q)
}
beta_interval(MH1, MHsd1)
beta_interval(MH2, MHsd2)
Ordinal_model$coef
outm = cbind(Ordinal_model$coef, beta_interval(MH1, MHsd1))
colnames(outm) = c('MLE', '1%', '5%', '25%', '50%', '75%', '95%', '99%')
outm
##############
# Exercise 2 
## Simulation of garch model

sim_garch <- function(N=1e3, h0 = 1, mu = 0, omega=0.1,
                      alpha1 = 0.3, beta1  = 0.6){
    ret <- et <- ht <- vector()
    ht[1] <- h0
    for(j in 1:N){
        et[j]   <- rnorm(1, 0, sqrt(ht[j]))
        ret[j]  <- mu + et[j]
        if(j < N) {
            ht[j+1] <- omega + alpha1*(et[j]^2) + beta1*ht[j]
        }
    }
        return(ret)
}
x <- sim_garch(N=1e4)
x = x[-(1:1000)]
summary(x)
ts.plot(x)
lines()
library(fGarch)
model = garchFit( ~ garch(1, 1), data = x)
summary(model)
## Simulation of SV model
sim_sv <- function(N=1e3, mu = 0, rho0=0.1, rho1 = 0.3){
    ret <- et <- vector()
    et[1] <- .1
    ht = rnorm(N)
    eta = rnorm(N)
    for(i in 2:N){
        et[i] = exp(rho0 + rho1*(log(et[i-1])-rho0) + eta[i])
        ret[i]  <- mu + ht[i]*sqrt(et[i])
    }
    return(ret)
}
y = sim_sv(1e4)
y = y[-(1:1000)]
ts.plot(y)
library(stochvol)
draws = svsample(y, 3000, 2000, priormu = c(0, 1),
                 priorphi = c(2, 1), priorsigma = 0.5)
summary(draws, showlatent = F)
