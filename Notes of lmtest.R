##################################################################
#                       Notes of lmtest                          #
##################################################################
library(lmtest)
library(ggplot2)
data(jocci)
View(jocci)
# draw the time-series plot to show whether the var is stable
plot(jocci[,2], ylab = 'jocci dy1')
# draw the scatter plot of AR(6)'s residuals to find the heteroskedasicity
ar6 = lm(ar6.model, data = jocci)
plot(residuals(ar6), ylab = 'AR(6) residuals')
#################################
#    Test of autocorrelation    #
#################################
# 1. DW test
# we test the dy(log returns of stocks)'s autocorrelation
dwtest(dy ~ 1, data = jocci)
# Due to the output we find significant seriel-correlation
# 2. LM test
# first we construct the AR(6) formula
ar6.model = dy ~ dy1 + dy2 + dy3 + dy4 + dy5 + dy6
# then we plug the formula into the BG test (a kind of LM test)
bgtest(ar6.model, data = jocci)
# we find there is no correlation because we add lagged vars
# Attention! DW test should not be applied here!

#################################
#  Test of heteroskedasicity    #
#################################
# 1. BP test
# we find the variance of residuals increase by time through the 2nd plot
# BP test will fit a linear model for the residuals as the 2nd argument
var.model = ~ I(dy1 ^ 2) + I(dy2 ^ 2) + I(dy3 ^ 2) + I(dy4 ^ 2) +I(dy5 ^ 2) + I(dy6 ^ 2)
bptest(ar6.model, var.model, data = jocci)
# the outcome p-value reject the null of homoskedasicity
# in addition the gqtest & hmctest also reject the null, while none of them can show
# the trendancy of the change of variance

####################################
#    Test of Model Specification   #
####################################
data(Mandible)
View(Mandible)
mandible = log(Mandible)
plot(y = mandible$length, x = mandible$age)
abline(lm(length ~ age, data = mandible), col = 'red', lwd = 2)
t = predict(lm(length ~ age + I(age ^ 2), data = mandible))
lines(x = mandible$age, y = t, col = 'blue', lwd = 2)
g = ggplot(mandible, aes(x = age, y = length))
g + geom_point() + geom_smooth(method = 'lm', se = F, size = 1.2) + geom_line(aes(y = t), colour = 'orange', size = 1.2)
# Through the plot we think the quadratic model may be more appropriate
# However, we still need constuct the linear model and test the model missepcification
# Harvey-Collier test 
harvtest(length ~ age, order.by = ~ age, data = mandible)
harvtest(length ~ age + I(age ^ 2), order.by = ~ age, data = mandible)
# order.by means the observation will be ordered by the size of age
# the significant result show the relationship of age is non-lineary
# Rainbow test
raintest(length ~ age, order.by = ~ age, data = mandible)
raintest(length ~ age + I(age ^ 2), order.by = ~ age, data = mandible)
# the result also reject the null
# we can also use resettest to test whether we should add the 2nd and 3rd powers of the fitted values
resettest(length ~ age, data = mandible)
resettest(length ~ age, power = 2, type = 'regressor', data = mandible)
resettest(length ~ age + I(age ^ 2), data = mandible)
# the null of resettest is higher powers have significant influence
# the last test means we should not add higher powers of age
plot(y = residuals(lm(length ~ age + I(age ^ 2), data = mandible)), x = mandible$age)
res = data.frame(age = rep(mandible$age, 2), resid = c(residuals(lm(length ~ age, data = mandible)),
                 res2 = residuals(lm(length ~ age + I(age ^ 2), data = mandible))), 
                 model = rep(c('model1', 'model2'), each = length(mandible$age)))
resplot = ggplot(res, aes(x = age, y = resid))
resplot + geom_point(size = 4, alpha = 0.5) + geom_smooth(size = 1.2, se = F, col = 'orange') + facet_wrap( ~ model)
# The 2 plots of residuals of each model also tell us the quadratic model is better fitted