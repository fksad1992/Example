SimpleRet = function(x){
  d = vector()
  for(i in 1:length(x)){
    if(i==1){
      d[i] = NA
    }
    else{
      d[i] = ((x[i]/x[i-1])-1)*100
    }
  }
  return(d)
}


Complete = function(x, y){
  z = x
  z[is.na(x)] = y[is.na(y)]
  return(z)
}

Information_ratio = function(x, y, data){
  d1 = cumprod(1+data$x)-1
  d2 = cumprod(1+data$y)-1
  z = (d1-d2)/(sd(data$x-data$y)*sqrt(length(data$x)/(length(data$x)-1)))
  return(z)
}
D = rnorm(10)
pmin(0, t(D))
