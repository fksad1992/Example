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
## Copula套利策略实例
library(xts)
library(copula)
No.i <- as.numeric(as.character(pairs.test.res[1,1]))
No.j <- as.numeric(as.character(pairs.test.res[1,2]))
df <- merge(list_Stock[[No.i]][,4],list_Stock[[No.j]][,4])
ag <- coredata(na.omit(df)[,1])
au <- coredata(na.omit(df)[,2])

plot(scale(ag), type='l', col='darkgreen', main="Standadized Prices")
lines(scale(au), col='darkred')

logReturn <- function(prc) { log( prc[-1]/prc[-length(prc)] ) }

ret.ag <- logReturn(ag)
ret.au <- logReturn(au)

plot(ret.ag, ret.au, pch=20, col='darkgreen', main="Scatterplot of the Returns")

fGumbel<-function(u,v,a) { -((-log(u))^(1/a)+(-log(v))^(1/a))^a }
Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a)) }
p1.Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a))*(-log(u))^(-1+1/a)*(((-log(u))^(1/a)+(-log(v))^(1/a))^(-1+a))/u } 
p2.Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a))*(-log(v))^(-1+1/a)*(((-log(u))^(1/a)+(-log(v))^(1/a))^(-1+a))/v }

Misp.Idx<-function(r1, r2){ 
  
  frac1 <- ecdf(r1)    
  frac2 <- ecdf(r2)
  
  size<-length(r1)
  
  xpar.1 <- c()
  xpar.2 <- c()
  
  for(i in 1:size)         
  {                             
    xpar.1[i] <- frac1(r1[i])                        
    xpar.2[i] <- frac2(r2[i])
  }
  
  u0 <- pobs( cbind(xpar.1, xpar.2) )
  gumbel.cop<- gumbelCopula(3,dim=2)           
  fit.ml <- fitCopula(gumbel.cop, u0)      
  
  alpha <- fit.ml@estimate          
  
  a <- alpha
  u <- frac1(r1[size])
  v <- frac2(r2[size])
  
  mis1 <- eval(p1.Gumbel(u,v,a))
  mis2 <- eval(p2.Gumbel(u,v,a))
  
  return( c(mis1, mis2) )
}

# test: 
# Misp.Idx(ret.ag[1:300], ret.au[1:300])

misp.idx.t <- c()
misp.idx.1 <- c()
misp.idx.2 <- c()

trd.prc.1 <- c()
trd.prc.2 <- c()

position <- c()
position.t <- 0

profit <- c()
m0 <- 100000

d1 <- 0.27
d2 <- 0.2
d3 <- 1.7
d4 <- 0.9
d5 <- 0.1
d6 <- 0.5

p1 <- ag
p2 <- au

p10 <- 0
p20 <- 0

k <- 200

for(i in (k+1):(length(p1)-1)){
  
  p.10 <- p1[i+1]
  p.20 <- p2[i+1]
  
  r1 <- logReturn(p1[(i-k):i])
  r2 <- logReturn(p2[(i-k):i])
  
  misp.idx.t <- Misp.Idx(r1,r2)     
  misp.idx.1 <- c(misp.idx.1, misp.idx.t[1])
  misp.idx.2 <- c(misp.idx.2, misp.idx.t[2])
  
  if( is.nan(misp.idx.t[1]) || is.nan(misp.idx.t[2]) ){ 
    position.t  <- position.t
    position <- c(position, position.t)
  }
  else{
    if( position.t == 0 ){
      if( misp.idx.t[1] > d1 & misp.idx.t[2] < d2 ){     
        trd.prc.1 <- c(trd.prc.1, p.10) 
        trd.prc.2 <- c(trd.prc.2, p.20) 
        vol.1 <-  m0*p.20/(p.10+p.20)
        vol.2 <-  -m0*p.10/(p.10+p.20)
        position.t  <-  1
        position <- c(position, position.t) 
      }
      else {
        if( misp.idx.t[1] < d3 && misp.idx.t[2] > d4 ){
          trd.prc.1 <- c(trd.prc.1, p.10) 
          trd.prc.2 <- c(trd.prc.2, p.20) 
          vol.1 <-  -m0*p.20/(p.10+p.20)
          vol.2 <-   m0*p.10/(p.10+p.20)
          position.t  <-  -1
          position <- c(position, position.t)
        }
        else{ 
          position.t <- position.t
          position <- c(position, position.t)
        }
      }
    }
    else{
      if((misp.idx.t[1] < d5 && position.t == 1) || (misp.idx.t[2] > d6 && position.t == -1)){ 
        profit.t <- position.t*( vol.1*(trd.prc.1[length(trd.prc.1)]-p.10) + vol.2*(p.20-trd.prc.2[length(trd.prc.2)]) )
        profit <- c(profit, profit.t)
        position.t <- 0
        position <- c(position, position.t)
      }
      else { # do nothing
        position.t  <- position.t
        position <- c(position, position.t) 
      } 
    }
  }
} 

win <- sum(profit > 0)
lose <- sum(profit <= 0)
win.ratio <- win/(win+lose)
win.ratio

plot(cumsum(profit), type='l')
plot(profit, type='l')

hist(profit, 50)
mean(profit)

## Copula套利策略实例(end) ##


