# 生成一些仿真数据
set.seed(20160227)  
x <- seq(0, 50, 1)
y <- ((runif(1, 10, 20)*x)/(runif(1, 0, 10)+x)) + rnorm(51, 0, 1)
# 对于一些简单的模型，nls函数可以自动找到合适的参数初值
m <- nls(y ~ a*x/(b+x))
# 计算模型的拟合优度
cor(y, predict(m))
[1] 0.9496598
# 将结果可视化
plot(x, y) 
lines(x, predict(m), lty = 2, col = "red", lwd = 3)


# 生成仿真数据，并且此次对于参数没有先验信息
y <- runif(1, 5, 15)*exp(-runif(1, 0.01, 0.05)*x)+rnorm(51, 0, 0.5)
# 可视化数据并选择一些参数初值
plot(x, y)
# 通过这个散点图确定参数a, b的初值
a_start <- 8 # 参数a是x = 0时y的取值
b_start<- 2*log(2)/a_start # b 是衰减速率
# 拟合模型
m <- nls(y ~ a*exp(-b*x), start = list(a = a_start, b = b_start))
# 计算拟合优度
cor(y, predict(m))
[1] 0.9811831
# 将结果可视化
lines(x, predict(m), col = "red", lty = 2, lwd = 3)


library(deSolve)
# 利用逻辑斯蒂模型生成人口增长的仿真数据，并用nls估计参数
log_growth <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dN <- R*N*(1-N/K)
    return(list(c(dN)))
  })
}
# 逻辑斯蒂增长的参数
pars  <- c(R = 0.2, K = 1000)
# 设定初值
N_ini  <- c(N = 1)
# 常微分方程的时间阶段（下标t）
times <- seq(0, 50, by = 1)
# 常微分方程
out   <- ode(N_ini, times, log_growth, pars)
# 添加一些随机波动
N_obs <- out[, 2]+rnorm(51, 0, 50)
# 个体数值不能小于1
N_obs <- ifelse(N_obs<1, 1, N_obs)
# 画图
plot(times, N_obs)
SS <- getInitial(N_obs ~ SSlogis(times, alpha, xmid, scale), data = data.frame(N_obs = N_obs, times = times))
# 改变参数形式
K_start <- SS["alpha"]
R_start <- 1/SS["scale"]
N0_start <- SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)
# 构建模型的公式
log_formula <- formula(N_obs ~ K*N0*exp(R*times)/(K + N0*(exp(R*times) - 1)))
# 拟合模型
m <- nls(log_formula, start = list(K = K_start, R = R_start, N0 = N0_start))
# 估计参数
summary(m)

# 计算拟合优度
cor(N_obs,predict(m))



# 结果可视化
lines(times, predict(m), col = "red", lty = 2, lwd = 3)
