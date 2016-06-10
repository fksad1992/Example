library(fPortfolio)                                          
library(BLCOP)                                                                                           
View(monthlyReturns)                                             
# We have a view that the average return of 2 IT stocks will  
# be 0.06 higher than that of MS
pickMatrix <- matrix(c(1/2, -1, 1/2, rep(0, 3)), nrow = 1, ncol = 6 )
views <- BLViews(P = pickMatrix, q = 0.06, confidences =  100,  # confidences means the inverse of sigma 
                 assetNames = colnames(monthlyReturns))
views
# First kind of prior
priorMeans <- rep(0, 6)
priorVarcov <- cov.mve(monthlyReturns)$cov  # use cov.mve to get a robust estimation
priorVarcov
# Compute the postier mean as well as cov
marketPosterior <- posteriorEst(views = views, sigma = priorVarcov, 
 mu = priorMeans, tau = .5)  # tau is choosen to be .5
marketPosterior
# Add another view
# The average return of MS, C, JPM, BAC will be 0.15
finViews <- matrix(ncol = 4, nrow = 1, dimnames = list(NULL, c("C","JPM","BAC","MS")))
finViews[,1:4] <- rep(1/4,4)
views <- addBLViews(finViews, 0.15, 90, views)
views
# Compute the postier value with the above 2 views
# This time we use CAPM to generate prior
marketPosterior <- BLPosterior(returns = as.matrix(monthlyReturns), views, tau = 1/2, 
	marketIndex = as.matrix(sp500Returns),riskFree = as.matrix(US13wTB))
marketPosterior                                                                      
# Calculate optimal potrfolios under prior and posterior distributinos
optPorts1 <- optimalPortfolios.fPort(marketPosterior, optimizer = "minriskPortfolio")
optPorts1
# Add a constraints that each asset should be allocate at least 10% weight
optPorts2 <- optimalPortfolios.fPort(marketPosterior, 
		constraints = "minW[1:6]=0.1", optimizer = "minriskPortfolio")
optPorts2
# Add a constraints that only long positions are avaliable
optPorts3 <- optimalPortfolios.fPort(marketPosterior, 
    constraints = "Longonly", optimizer = "minriskPortfolio")
optPorts3                                                                                                   


