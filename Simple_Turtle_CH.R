# 海龟交易策略
# 加载相应的包
library(quantmod)
library(TTR)
library(blotter)
.blotter <- new.env()
.instrument <- new.env()
Sys.setenv(TZ="UTC")
# 清空内存
try(rm("account.turtles","portfolio.turtles",pos=.blotter),silent=TRUE)
try(rm("portfolio","account","N","symbol","symbols","ClosePrice","CurrentDate","equity","Units","maxUnits","size","Stop","equity","TxnPrice","initDate","initEq","Posn","verbose"),silent=TRUE)
# 设定初值
initDate="2015-01-01"
initEq=1000000000
print("Initializing portfolio and account structure")
# 构造一个包含三支股票的小型投资组合
symbols = c('000651.SZ')#c("600479.SS", "600422.SS", "600781.SS")
currency("RMB")
for(symbol in symbols){
  stock(symbol, currency="RMB",multiplier=1)
}
# 创建函数存储中间值
updateStrat <- function(Portfolio, Symbol, TxnDate, PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)
{ #添加交易事务相关数据到STRATEGY时间序列
  #输入的参数有：
  #TxnDate:以ISO 8106格式的交易日期，例如：‘2008-09-01”
  #PosUintsQty:总交易数量（股数）
  #StopPrice:交易完成价格
  #TxnPrice:最后交易价格
  #TxnN:为最后的结算N
  #输出
  #没有输出。在本地命名空间修改STRATEGY
  #保存交易事务与计算结果，返回投资组合
  NewTxn <- xts(t(c(PosUnitsQty, UnitSize, StopPrice, TxnPrice, TxnN)), order.by=as.POSIXct(TxnDate),
                dimnames=list(NULL, c('Pos.Units', 'Unit.Size', 'Stop.Price', 'Txn.Price', 'Txn.N')))
  # .getPortfolio 函数会返回可供读写的投资组合环境
  # NOTE: 安全起见，可用getPortfolio函数得到一个只读的环境副本，将投资组合的环境拷贝至一个列表
  Portfolio <- .getPortfolio(Portfolio)
  # 这张表存储了与策略相关的交易信息
  Portfolio$symbols[[Symbol]]$strat <- rbind(Portfolio$symbols[[Symbol]]$strat, NewTxn)
}
getSymbols(symbols, index.class="POSIXct", from=initDate, src="yahoo")
for(symbol in symbols){
  t = get(symbol)
  for(j in 1:ncol(t)){
    t[, j] = na.spline(t[, j])
    assign(symbol, t)
  }
}
for(symbol in symbols){
  adsymbol = adjustOHLC(get(symbol),symbol.name = symbol)
  assign(symbol, adsymbol)
}
# getSymbols函数默认使用“Date”来索引，这里我们将其改为用POSIXct索引 
# 创建一个投资组合对象和账户对象
portfolio = "turtles" 
initPortf(name=portfolio,symbols, initDate=initDate)
account = "turtles"
initAcct(name=account,portfolios="turtles", initDate=initDate, initEq=initEq)
# 构造一个指标
print("Setting up indicators")
for(symbol in symbols){
  # 以20日突破为指标的短线策略
  # 20天突破就是指价格超越了过去20天的最高点
  # 但如果上次突破是可以带来一次赢利的交易，那么系统1当前入市信号将被忽略
  x=get(symbol)
  # 入市
  x$Max20 <- runMax(x[,grep('High',colnames(x))],20)
  # 离场
  x$Min10 <- runMin(x[,grep('Low',colnames(x))], 10)
  # 仓位规模参数 c('High','Low','Close')
  x$N <- ATR(x[,c(2,3,4)], n=20, maType=EMA, wilder=TRUE)[,'atr']
  assign(symbol,x)
}
# 投资组合参数
size = 0.01
maxUnits = 4
Units=0
verbose=TRUE
# 创建交易
count = 0
for( i in 22:NROW(x) ) { # 假设所有日期相同
  CurrentDate=time(x)[i]
  # 显示当前日期
  equity = getEndEq(account, CurrentDate)
  for(symbol in symbols){
    x=get(symbol)
    ClosePrice = as.numeric(Cl(x[i,]))
    Posn = getPosQty(Portfolio=portfolio, Symbol=symbol, Date=CurrentDate)
    s = tail(getPortfolio(portfolio)$symbols[[symbol]]$strat,1)
    Units = as.numeric(s[,'Pos.Units'])
    TxnPrice = as.numeric(s[,'Txn.Price'])
    N = as.numeric(s[,'Txn.N'])
    Stop = as.numeric(s[,'Stop.Price'])
    UnitSize = as.numeric(trunc((size * equity)/(x[i-1,'N']*ClosePrice)))
    # 加仓 (假设以收盘价买入)
    if( Posn == 0 ) { 
      # 初始长头寸
      if( as.numeric(Hi(x[i-1,])) > as.numeric(x[i-2,'Max20']) ) 
        { 
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio=portfolio, Symbol=symbol, TxnDate = CurrentDate, PosUnitsQty = 1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
        count = count + 1
      } 
    } 
      # 减仓和止损
      if( ( Posn > 0 && ( as.numeric(Lo(x[i-1,]))  <  as.numeric(x[i-2,'Min10']) || Lo(x[i-1,])  < Stop ) ) )
           {
        addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = -Posn , TxnFees=0, verbose=verbose)
        N = as.numeric(x[i-1,'N'])
        updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = 0, UnitSize = UnitSize, StopPrice = NA, TxnPrice = ClosePrice, TxnN = N)
        count = count+1
      }
        # 增加到长头寸（开多仓）
        if( Posn > 0  && Units < maxUnits && Hi(x[i-1,]) > ( TxnPrice + N * 0.5 ) )
          {
          addTxn(Portfolio=portfolio, Symbol=symbol, TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty = UnitSize , TxnFees=0, verbose=verbose)
          N = as.numeric(x[i-1,'N'])
          updateStrat(Portfolio = portfolio, Symbol = symbol, TxnDate = CurrentDate, PosUnitsQty = Units+1, UnitSize = UnitSize, StopPrice = (ClosePrice-2*N), TxnPrice = ClosePrice, TxnN = N)
          count = count +1
        } 
    # 维持先前仓位
  } # 结束每个股票的循环
  # 所有交易跟新完毕，开始记录结果
  updatePortf(Portfolio = portfolio)
  updateAcct(account, Dates=CurrentDate)
  updateEndEq(account, Dates=CurrentDate)
} # 结束日期更新
# 最终结果
count
cat('Return: ',(getEndEq(Account=account, Date=CurrentDate)-initEq)/initEq,'\n')
if (require(quantmod)) {
  for(symbol in symbols){
    dev.new()
    chart.Posn(Portfolio='turtles',Symbol=symbol)
  }
}
if(require(PerformanceAnalytics)){
  return = Delt(getAccount(account)$summary$End.Eq)
  dev.new()
  charts.PerformanceSummary(as.zoo(return),main="Turtle Demo Performance")   
  dev.new()
  charts.PerformanceSummary(PortfReturns('turtles'),main='Turtle Demo Instrument Return on Equity',geometric=FALSE)
}
getEndEq(account,Sys.time())


date = time(get('000001.SS'))
total = vector()
for(i in 1:length(date)){
  total[i] = getEndEq(account, Date = date[i])
}

close = Cl(get('000001.SS'))
SH_Ret = vector()
for(i in 2:length(close)){
  t = as.numeric(close)
  SH_Ret[i] = log(t[i]) - log(t[i-1])
}
SH_Ret[1] = 0
plot.ts(total/10^9-1)
lines(cumsum(SH_Ret), col = 'red')
