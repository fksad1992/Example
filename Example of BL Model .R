library(ggplot2)
# Input the basic information of MSCI World Index
assetnames = c('UK', 'US', 'JP', 'FR', 'DE', 'CA', 'CH', 'AU')
pi = c(0.0179, 0.0188, 0.0409, 0.0214, 0.0238, 0.0160, 0.0174, 0.0122)
omiga = c(0.09, 0.34, 0.43, 0.03, 0.04, 0.03, 0.02, 0.02)
Sigma = matrix(c(0.0246, 0.0081, 0.0054, 0.0204, 0.0196, 0.0085, 0.0161, 0.0055,
                 0.0081, 0.0228, 0.0019, 0.0102, 0.0121, 0.0141, 0.0072, 0.0017,
                 0.0054, 0.0019, 0.0332, 0.0064, 0.0070, 0.0034, 0.0060, 0.0079,
                 0.0204, 0.0102, 0.0064, 0.0350, 0.0284, 0.0110, 0.0212, 0.0060,
                 0.0196, 0.0121, 0.0070, 0.0284, 0.0438, 0.0125, 0.0234, 0.0075,
                 0.0085, 0.0141, 0.0034, 0.0110, 0.0125, 0.0234, 0.0075, 0.0049,
                 0.0161, 0.0072, 0.0060, 0.0212, 0.0234, 0.0075, 0.0276, 0.0052,
                 0.0055, 0.0017, 0.0079, 0.0060, 0.0075, 0.0049, 0.0052, 0.0246),
               nrow = 8, ncol = 8, byrow = TRUE)
# Input the objects of views
PickMatrix = matrix(c(0, -1, 0, 0, 0, 0, 1, 0,
                      0, 0, 1, 0, 0, 0, 0, 0),
            nrow = 2, ncol = 8, byrow = TRUE)
PickMatrix
PickAbs = matrix(PickMatrix[2, ], nrow = 1, ncol = 8)
PickRel = matrix(PickMatrix[1, ], nrow = 1, ncol = 8)
q = c(.05, .1)
A = 2.5
tau = .5
Omiga_High = matrix(c(.0001, 0, 0, .0004), nrow = 2, byrow = TRUE)
Omiga_Low = matrix(c(.04, 0, 0, .0004), nrow = 2, byrow = TRUE)

# Generate the BL weights step-by-step
# First, generate the Views-implied expected returns
BLRET <- function(Pick, q, Omiga, pi, Sigma, tau, names){
  V = solve(t(Pick) %*% solve(Omiga) %*% Pick + solve(tau * Sigma))
  M = V %*% (solve(tau * Sigma) %*% pi + t(Pick) %*% solve(Omiga) %*% q) 
  rownames(M) = names
  colnames(M) = 'Expect Returns'
  return(t(M))
}
R_abs = BLRET(Pick = PickAbs, q = q[2], pi = pi, Sigma = Sigma, tau = tau,
               Omiga = Omiga_High[2, 2], names = assetnames)
R_rel_high = BLRET(Pick = PickRel, q = q[1], pi = pi, Sigma = Sigma, tau = tau,
                         Omiga = Omiga_High[1, 1], names = assetnames)
R_rel_low = BLRET(Pick = PickRel, q = q[1], pi = pi, Sigma = Sigma, tau = tau,
                        Omiga = Omiga_Low[1, 1], names = assetnames)
R_both = BLRET(Pick = PickMatrix, q = q, pi = pi, Sigma = Sigma, tau = tau,
                     Omiga = Omiga_Low, names = assetnames)
RET = rbind(pi, R_abs, R_rel_high, R_rel_low, R_both)
rownames(RET) = c('CAPM', 'ABs', 'Rel_high', 'Rel_low', 'Both')
RET

# Secondly, we construct the function to get the BL weights
BLModel <- function(Pick, q, Omiga, pi, Sigma, tau, names){
V = solve(t(Pick) %*% solve(Omiga) %*% Pick + solve(tau * Sigma))
M = V %*% (solve(tau * Sigma) %*% pi + t(Pick) %*% solve(Omiga) %*% q) 
omiga = solve((Sigma + V)) %*% M
omiga = omiga / sum(omiga)
rownames(omiga) = names
colnames(omiga) = 'BLomiga'
return(omiga)
}
omiga_rel_high = BLModel(Pick = PickRel, q = q[1], pi = pi, Sigma = Sigma, tau = tau,
                         Omiga = Omiga_High[1, 1], names = assetnames)
omiga_rel_low = BLModel(Pick = PickRel, q = q[1], pi = pi, Sigma = Sigma, tau = tau,
                         Omiga = Omiga_Low[1, 1], names = assetnames)
omiga_abs = BLModel(Pick = PickAbs, q = q[2], pi = pi, Sigma = Sigma, tau = tau,
                         Omiga = Omiga_High[2, 2], names = assetnames)
omiga_both = BLModel(Pick = PickMatrix, q = q, pi = pi, Sigma = Sigma, tau = tau,
                        Omiga = Omiga_Low, names = assetnames)
# Draw the barcharts of different occasions
gg = data.frame(names = rep(assetnames,8), 
                omiga = c(omiga, omiga_abs, omiga, omiga_both, omiga, omiga_rel_high, omiga, omiga_rel_low),
                weight = c(rep('mkt', 8), rep('abs', 8), rep('mkt', 8), rep('both', 8), rep('mkt', 8), rep('high', 8), rep('mkt', 8), rep('low', 8)),
                facet = rep(c('abs','both','high','low'), each = 16))
q = ggplot(gg, aes(x = names, y = omiga, fill = weight))
q + geom_bar(position = 'dodge', stat = 'identity', alpha = 0.75) + facet_grid(. ~ facet, scale = 'free') 



