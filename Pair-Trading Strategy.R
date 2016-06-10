# pair-trading
## get data
rm(list=ls());gc();#清空数据及缓存
#定义待分析的股票代码
df_StockCode <- data.frame(
  Name = c("浦发银行","民生银行","中信证券","中国联通")
  ,Code = c("600000","600016","600030","600050")
) 
library("quantmod")#加载R包
df_StockCode$Code <- paste(df_StockCode$Code,"SS",sep=".")#修改证券代码格式
list_50etf <- list()
list_50etf.a <- list()
ListingDate.Last <- as.Date("1900-01-01")
for(i in 1:length(df_StockCode$Code)){
  print(paste("load",df_StockCode$Name[i],"除权数据"))
  tmp1 <- 1;tmp2 <- 0
  while(tmp1 > 0 & tmp2 < 10){
    getSymbols(df_StockCode$Code[i],scr='yahoo'
               ,from="1900-01-01",to='2015-06-05')
    #读取对应股票数据
    STOCK.DATA <- get(df_StockCode$Code[i])
    tmp1 <- length(which(is.na(as.Date(.indexDate(STOCK.DATA)))))
    tmp2 <- tmp2 + 1
    # 检验读取数据是否完整，即验证日期序列是否存在NA
  }
  if(tmp2 == 10){
    print(paste(df_StockCode$Name[i],"除权数据","读取出错"))
    break
  }
  print(paste(df_StockCode$Name[i],"除权数据","读取完成"))
  list_50etf[[i]] <- STOCK.DATA
  ListingDate.Last <- max(ListingDate.Last,as.Date(.indexDate(STOCK.DATA))[1])
  print(paste("load",df_StockCode$Name[i],"前复权数据"))
  #统计最晚上市的股票上市日期，方便时间序列日期一致
  STOCK.DATA.a <- adjustOHLC(STOCK.DATA,symbol.name=df_StockCode$Code[i])
  #读取前复权后的股票数据
  print(paste(df_StockCode$Name[i],"前复权数据","读取完成"))
  list_50etf.a[[i]] <- STOCK.DATA.a
  rm(list=c("tmp1","tmp2","STOCK.DATA","STOCK.DATA.a",df_StockCode$Code[i]))
  print(paste(i,"/",length(df_StockCode$Code),"end"))
}
names(list_50etf) <- df_StockCode$Code
names(list_50etf.a) <- df_StockCode$Code
## data cleaning
list_Stock <- list_50etf.a #提取初始前复权数据
for(i in 1:length(list_Stock)){
  list_Stock[[i]] <- list_Stock[[i]][paste(ListingDate.Last
                                           ,"2015-06-05",sep="/")]
}#取上市最晚的方正证券日期2011-08-15到2015-06-05日的每只股票前复权数据

## stocks selection

###### 可配对交易股票检验(begin)######
library("quantmod")
library("fUnitRoots")
library("fGarch")
# - Find pairs  ---------
# Ur means unit root test. 
# Here uses Engle Granger Two Step Method to test if co-integration.
No.ij <- t(combn(1:length(list_Stock),2))
St <- round(dim(list_Stock[[1]])[1]*3/4)
pairs.test.res <- t(mapply(No.ij[, 1], No.ij[, 2], FUN=function(i,j){
  
  #Pre Data
  stk.i <- list_Stock[[i]][1:(St-1),4]
  stk.j <- list_Stock[[j]][1:(St-1),4]
  
  cor.pair <- cor(stk.i, stk.j)   
  reg.pair <- lm(stk.i~ stk.j)
  coefs <- coef(reg.pair)
  error <- as.numeric(reg.pair$residuals)
  ur <- ur.df(error,type="none",lags=5,selectlags="AIC")
  # Notice that ur.df() returns a s4 class:urca, 
  # we should use @ instead of $
  ur.stat <- ur@teststat 
  ur.signif <- ur@cval
  
  signif.level <- ""
  if ( ur.stat < ur.signif[1] ) { signif.level <- "***" 
  } else if ( ur.stat < ur.signif[2] ) { signif.level <- "**"    
  } else if ( ur.stat < ur.signif[3] ) { signif.level <- "*"    
  } else { signif.level <- "" }
  
  Flag <- 0
  if( ur.stat < ur.signif[1] && cor.pair > 0.85 ) { Flag <- 1 }
  
  ret <- c(i, j, names(stk.i), names(stk.j), cor.pair, 
           coefs[1], coefs[2], ur.stat, signif.level, Flag)
  return(ret)
  
}))

pairs.test.res0 <- data.frame(pairs.test.res)
names(pairs.test.res0) <- c("No.i","No.j","Nme.i","Nme.j","Corr",
                            "Alpha", "Beta", 
                            "Ur.stat", "Signif.level","Flag") 
head(pairs.test.res0)
head( pairs.test.res0[(pairs.test.res0$Flag == 1 
                       & as.numeric(as.character(pairs.test.res0$Corr))>0.9), ] )
### 可配对交易股票检验(end) ###
pairs.test.res0[(pairs.test.res0$Flag == 1 
                 & as.numeric(as.character(pairs.test.res0$Corr))
                 ==max(as.numeric(as.character(pairs.test.res0$Corr)))), ]

### 配对套利盈利 ### 
pairs.test.res <- pairs.test.res0[(pairs.test.res0$Flag == 1 
                                   & as.numeric(as.character(pairs.test.res0$Corr))
                                   ==max(as.numeric(as.character(pairs.test.res0$Corr)))), ]


No.i <- as.numeric(as.character(pairs.test.res[1,1]))
No.j <- as.numeric(as.character(pairs.test.res[1,2]))
prc.A <- list_Stock[[No.i]][,4]
prc.B <- list_Stock[[No.j]][,4]
xts::plot.xts(merge(prc.A,prc.B), screens=1
              , main="Prices of SH001 and SZ300", lwd=2, 
              auto.legend=TRUE, legend.loc='topright') 
St <- round(length(prc.B)/4*3) 

prc.trn.A <- prc.A[1:St] 
prc.trn.B <- prc.B[1:St] 
cor(prc.trn.A, prc.trn.B) 
# 相关系数0.9491952显示二者强相关 

reg.res <- lm(prc.trn.A~ prc.trn.B) 
summary(reg.res) 
# 线性回归系数高显著，说明存在线性关系很平稳 
alpha <- coef( reg.res )[1] 
beta <- coef( reg.res )[2] 
spread <- prc.A - beta*prc.B - alpha 
xts::plot.xts (spread, lwd=2, col='darkblue', main="Spread Series") 
ur.res <- ur.df( coredata(resid(reg.res)), type='none') 
summary(ur.res) 
# Value of test-statistic is: -4.6053 表示两个指数的线性组合是平稳的时间序列 

sigma <- sd(spread) 
phi.1 <- 0.5 
phi.2 <- 1.2 
theta <- 2.5 


plot(coredata(spread), type='l', col='lightgreen', lwd=2, main='Spread and State Zone') 
abline( h=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=2, lty=2) 
abline( h=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=2, lty=4) 
abline( h=c(-theta*sigma, theta*sigma), col='gray30', lwd=2, lty=3) 

hist(spread, breaks=100, col='lightgreen', border=FALSE) 
abline(v=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=2, lty=2) 
abline(v=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=3, lty=4) 
abline(v=c(-theta*sigma, theta*sigma), col='gray30', lwd=3, lty=3) 

state <- cut(spread, breaks=c(-Inf, -theta*sigma, -phi.2*sigma, -phi.1*sigma, phi.1*sigma, phi.2*sigma, theta*sigma, Inf)) 
state <- as.numeric(state) - 4 
xts::plot.xts( merge(spread, state), col=c('darkblue', 'darkgreen'), lwd=2, main='Spread and State Line') 

End <- length(spread) 
End 
# 1 means open, 2 means close 

End <- length(spread) 

signal <- c( rep(0, St), 0) 

for( t in (St+1):End ){ 
  
  signal[t] <- 0 
  if( state[t-1] == -2 & state[t] > -2 ) signal[t] <- -1 
  if( state[t-1] <= -1 & state[t] > -1 ) signal[t] <- -2 
  if( state[t-1] >= -2 & state[t] == -3 ) signal[t] <- -2 
  if( state[t-1] == 2 & state[t] < 2 ) signal[t] <- 1 
  if( state[t-1] >= 1 & state[t] < 1 ) signal[t] <- 2 
  if( state[t-1] <= 2 & state[t] == 3 ) signal[t] <- 2 
  
} 

trd.state <- c( rep(0, St), 0) 

for( t in (St+1):End ){ 
  
  trd.state[t] <- trd.state[t-1] 
  
  if( trd.state[t-1] == 0 & signal[t] == 1 ){ 
    trd.state[t] <- 1 
  } 
  
  if( trd.state[t-1] == 0 & signal[t] == -1 ){ 
    trd.state[t] <- -1 
  } 
  
  if( trd.state[t-1] != 0 & abs(signal[t]) == 2 ){ 
    trd.state[t] <- 0 
  } 
  
} 


plot(coredata(spread), type='l', col='lightgreen', lwd=2, 
     main='Spread and State Zone') 
lines(trd.state, col='red', lty=1, lwd=2) 
abline( h=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=1, lty=2) 
abline( h=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=1, lty=4) 
abline( h=c(-theta*sigma, theta*sigma), col='gray30', lwd=1, lty=4) 
trd.state <- xts(trd.state, order.by=as.Date(.indexDate(prc.A)))
xts::plot.xts( merge(prc.A, beta*(prc.B), spread, trd.state), screens=1, 
               auto.legend=TRUE, legend.loc='topright', 
               main="Prices, Spread, and Trading Line") 
trd.state <- coredata(trd.state)
cash <- c( rep(0, St), 0 ) 
share.B <- c( rep(0, St), 0) 
share.A <- c( rep(0, St), 0) 
value <- c( rep(0, St), 0) 

for( t in (St+1):End ) { 
  
  if( trd.state[t-1]==0 & trd.state[t] == 0 ){ 
    share.B[t] <- 0 
    share.A[t] <- 0 
    cash[t] <- cash[t-1] 
  } 
  
  if( trd.state[t-1] == 0 & trd.state[t] != 0 ){ 
    share.A[t] <- trd.state[t] 
    share.B[t] <- beta*trd.state[t] 
    cash[t] <- cash[t-1] + coredata(prc.A[t])*share.A[t] + coredata(prc.B[t])*share.B[t]  
  } 
  
  if( trd.state[t-1] != 0 & trd.state[t] !=0 ){ 
    share.B[t] <- share.B[t-1] 
    share.A[t] <- share.A[t-1] 
    cash[t] <- cash[t-1] 
  } 
  
  if( trd.state[t-1] !=0 & trd.state[t] == 0 ){ 
    share.B[t] <- 0 
    share.A[t] <- 0 
    cash[t] <- cash[t-1] - coredata(prc.A[t])*share.A[t] + coredata(prc.B[t])*share.B[t] 
  } 
  
  value[t] <- cash[t] - coredata(prc.A[t])*share.A[t] + coredata(prc.B[t])*share.B[t] 
  
} 

sim.res.xts <- merge(spread, signal, trd.state, cash, value) 
xts::plot.xts(sim.res.xts, screens=1) 
xts::plot.xts(sim.res.xts[, "value"], col='darkred', lty=1, lwd=2, main="Value")
### 配对套利策略实例(end) ###




