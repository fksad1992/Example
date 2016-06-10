## use the previous value to interplot
na.repeat = function(x){
  d = x
  for(i in 1:length(x)){
    if(is.na(x[i])){
      d[i] = d[i-1]
    }
    else{d[i] = x[i]}
  }
  return(d)
}
## compute the simple return
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
## compute the information ratio
Information_ratio = function(x, y, data){
  d1 = cumprod(1+data$x)-1
  d2 = cumprod(1+data$y)-1
  z = (d1-d2)/(sd(data$x-data$y)*sqrt(length(data$x)/(length(data$x)-1)))
  return(z)
}
## compute the monthly return by weekly data
Month = function(x){
  xts(x$Cclose, order.by = x$Date) %>%
    to.monthly() -> df
  df[, 4] %>% as.vector() %>% SimpleRet() ->re
  res = data.frame(Date = time(df), Mret = re)
  res = res[complete.cases(res), ]
  return(res)
}
Month(R1)
## transfer the monthly return to bianry factor
binary = function(x){
  x[x>0]=1
  x[x<0]=0
  x = factor(x)
  return(x)
}
## find the maximun runs
findruns = function(x){
  n = length(x)
  runs  = vector(length = n)
  num = vector(length = n)
  for(j in 1:n){
    count = 0
    for(i in 1:(n-j+1)){
      if(all(x[i:(i+j-1)]==1)){
        count = count + 1
        runs[count]  = i
      }
    }
    if(count > 0 ){
      num[j] = j
    } 
  }
  Max = which.max(num)
  return(Max)
}

### upside month divide by down side month
Udmonth = function(x){
  table(x)[2]/table(x)[1]
}
### ting pan
suspension = function(x, k){
n = length(x)
run = 0
for(i in 1:(n-k+1)){
  if(all(x[i:(i+k-1)]==0)){
    run = run +1
  }
}
return(rep(run!=0, n))
}


