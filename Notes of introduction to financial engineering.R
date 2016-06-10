## Forecast the exchange rate
eurusd = get.hist.quote(instrument = 'EUR/USD', provider = 'oanda', 
                        start = '2013-12-31', end = '2014-12-31')
plot(eurusd)
surusd %>% 
  as.numeric() %>%
    log() %>%
      diff() %>%
        exp() -> r
r = r - 1
qqnorm(r)
model = garchFit(~ arma(1, 0) + garch(1, 1), con.dist = 'std', data = r,
                 prediction.interval = T)
qqnorm(model@residuals)
predict(model, n.ahead = 20, plot = T)
## CAPM Model
sp500 = get.hist.quote(instrument = '^gspc', start = '2004-12-31', 
                       end = '2014-12-31', quote = 'AdjClose')
ms = get.hist.quote(instrument = 'msft', start = '2004-12-31', 
                       end = '2014-12-31', quote = 'AdjClose')
plot(sp500)
plot(ms)
sp500 = exp(diff(log(as.numeric(sp500)))) - 1 
ms = exp(diff(log(as.numeric(ms)))) - 1
fit = lm(ms ~ sp500)
summary(fit)
qqnorm(fit$residuals)
