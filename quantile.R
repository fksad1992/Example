### the function to realize the bootstrap process
boot_VaR = function(x, alpha){
    n  = length(x)
    boot_VaR = vector()
    for(i in 1:500){
        boot_sample = sample(x, n, replace = TRUE)
        boot_VaR[i] = quantile(boot_sample, alpha)
    }
    return(mean(boot_VaR))
}
### parametric method to calculate the quantile based on normal distribution
normal_VaR = function(x, alpha){
    return(mean(x)-pnorm(alpha)*sd(x))
}
### compare these two methods
boot_test = function(x2, n, trial){
    boot_VaR_Test = matrix(0, nrow = trial, ncol = 2)
    for(i in 1:trial){
        small_x = sample(x2, n, replace = FALSE)
        boot_VaR_Test[i, ] = c(boot_VaR(small_x, .05), normal_VaR(small_x, .05))
    }
    hist(boot_VaR_Test[, 1])
    return(apply(boot_VaR_Test, 2, mean))
}
boot_test(x2, 24, 100)

### The function to compute the sample quantile
quant = function(x, p){
    m = 1-p
    n = length(x)
    j = floor(n*p - p + 1)
    g = n*p - p + 1 -j
    quant = (1-g)*q[j] + g*q[j+1]
    return(quant)
}


