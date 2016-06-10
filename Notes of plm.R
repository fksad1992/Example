#######################################################
#                                                     #
#                 Introduction of PLM                 #
#                                                     #
#######################################################
library(plm)
data('EmplUK', package = 'plm')
data('Produc', package = 'plm')
data('Grunfeld', package = 'plm')
data('Wages', package = 'plm')
data('Hedonic', package = 'plm')
# use pdata.frame to transform a dataset into a fancy format
E = pdata.frame(EmplUK, index = c('firm', 'year'),
                drop.index = TRUE, row.names = TRUE)
# Pdata.frame has specific methods, like summary and as.matrix
summary(E$emp)
head(as.matrix(E$emp))
# some transformations for panel series
head(lag(E$emp, 0:2))
head(lag(E$emp, 2), 10)
head(diff(E$emp), 10)
head(Within(E$emp))
head(between(E$emp), 4)
head(Between(E$emp), 10)

###########################
#      Basic Models       #
###########################

# 1 Fixed Effect Model
formula = expression(inv ~ value+capital)
grun.fe = plm(eval(formula), data = Grunfeld, 
              model = 'within')
summary(grun.fe)
# the coef of each individual in the FE model(i.e. extract the FE)
fixef(grun.fe, typoe = 'dmean')
summary(fixef(grun.fe, type = 'dmean'))
# only extract one fixed effect
grun.twfe = plm(eval(formula), data = Grunfeld,
                model = 'within', effect = 'twoways')
fixef(grun.twfe, effect = 'time')

# 2 Random Effect Models
grun.amen = plm(eval(formula), data = Grunfeld, 
              model = 'random', effect = 'twoways',
              random.method = 'amemiya')  # (swar,walhus,nerlove,kinla)               
summary(grun.amen)
# use ercomp to see the var of error components
ercomp(grun.amen)

# 3 Unbalanced Panels(i.e. we cannot use random twoways method)
Hed = plm(mv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+blacks+lstat,
          data = Hedonic, model = 'random',index = "townid")
summary(Hed)

# 4 Instumental Variable Estimators
# use the sign | at the end of formula to specify IV
# the coef of IV will be allocated to the orgin regressors
# in the following case, log(taxpc) & log(mix) are the IV of log(prbarr) & log(polpc)
# however the IV do not show up in the result
data("Crime", package = "plm")
cr = plm(log(crmrte) ~ log(prbarr) + log(polpc) + log(prbconv) +
            log(prbpris) + log(avgsen) + log(density) + log(wcon) + 
            log(wtuc) + log(wtrd) + log(wfir) + log(wser) + log(wmfg) + 
            log(wfed) + log(wsta) + log(wloc) + log(pctymle) + log(pctmin) + 
            region + smsa + factor(year) | . - log(prbarr) -log(polpc) + 
            log(taxpc) + log(mix), data = Crime,
          model = "random")
summary(cr)

# 5 Variable Coef Model
grun.varw = pvcm(inv~value+capital,data=Grunfeld,model="within")
grun.varr = pvcm(inv~value+capital,data=Grunfeld,model="random")
summary(grun.varw)
summary(grun.varr)

#####################################
#             Tests                 #
#####################################

# 1 Test of Poolability
# to test whether we can treat the panel as cross-sectional data
# the arugments are 2 models(i.e. it is a model comparison)  
znp <- pvcm(inv~value+capital,data=Grunfeld,model="within")  # variable coef
zplm <- plm(inv~value+capital,data=Grunfeld,model='within')  # invariant coef
pooltest(zplm,znp)
# you can also use in this way
pooltest(inv~value+capital,data=Grunfeld,model="within")

# 2 Test of Individual and Time Effects
# the null model must be a pooling model
g <- plm(inv ~ value + capital,data=Grunfeld,model="pooling")
plmtest(g,effect="time",type="honda")
plmtest(g,effect="individual",type="honda")
plmtest(g,effect="twoways",type="ghm")
# you can also use in this way
plmtest(inv~value+capital,data=Grunfeld,effect="twoways",type="ghm")
# pFtest is another way to realize the test
# just like pooltest, it is a model comparison
gw <- plm(inv ~ value + capital,data=Grunfeld,effect="twoways",model="within")
gp <- plm(inv ~ value + capital,data=Grunfeld,model="pooling")
pFtest(gw,gp)
# you can also use in this way
pFtest(inv~value+capital,data=Grunfeld,effect="twoways")

# 3 Hausman Test
# This test helps us to modify the kind of effect(i.e. FE or RE)
gw <- plm(inv~value+capital,data=Grunfeld,model="within")
gr <- plm(inv~value+capital,data=Grunfeld,model="random")
phtest(gw, gr)
# When the null is not rejected, we should use random effect model
# Because RE model is more efficient
# However, if the null is rejected the RE estimators would be inconsisitent