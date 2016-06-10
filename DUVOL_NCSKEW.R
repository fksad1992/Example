## the larger the risker(crash-risk only)
DUVOL = function(ts){
  ts = as.numeric(ts)
  ts = log(1 + ts/100)
  t1 = ts[ts>mean(ts)]
  t2 = ts[ts<=mean(ts)]
  D = log(sd(t2) / sd(t1))
  return(D)
}
NCSKEW = function(ts){
    library(psych)
    ts = as.numeric(ts)
    ts = log(1 + ts/100)
    n = skew(ts, type = 2)
    return(-n)
}