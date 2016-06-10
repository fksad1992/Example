b2 <- bayesx(RTS[, 1] ~ #sx(lag(RTS[, 2], 2):lag(RTS[, 2], 2) + 
                             sx(lag(RTS[, 2], 2):lag(RTS[, 2]), 
                                bs = 'psplinerw2'), method = "MCMC", 
             iter = 12000, burnin = 2000, data = RTS)
summary(b2)
plot(b2, residuals = TRUE)
