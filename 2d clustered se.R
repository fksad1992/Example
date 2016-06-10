## function for two-dimension clustered s.e.
## dat for data.frame, fm for fitted model 
mcl <- function(dat,fm, cluster1, cluster2){
  attach(dat, warn.conflicts = F)
  library(sandwich);library(lmtest)
  cluster12 = paste(cluster1,cluster2, sep="")
  M1  <- length(unique(cluster1))
  M2  <- length(unique(cluster2))   
  M12 <- length(unique(cluster12))
  N   <- length(cluster1)          
  K   <- fm$rank             
  dfc1  <- (M1/(M1-1))*((N-1)/(N-K))  
  dfc2  <- (M2/(M2-1))*((N-1)/(N-K))  
  dfc12 <- (M12/(M12-1))*((N-1)/(N-K))  
  u1j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster1,  sum)) 
  u2j   <- apply(estfun(fm), 2, function(x) tapply(x, cluster2,  sum)) 
  u12j  <- apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum)) 
  vc1   <-  dfc1*sandwich(fm, meat=crossprod(u1j)/N )
  vc2   <-  dfc2*sandwich(fm, meat=crossprod(u2j)/N )
  vc12  <- dfc12*sandwich(fm, meat=crossprod(u12j)/N)
  vcovMCL <- vc1 + vc2 - vc12
  coeftest(fm, vcovMCL)
}

