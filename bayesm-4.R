library(bayesm)
set.seed(1234)
n=1000
x<-cbind(rep(1,n),runif(n))#生成X矩阵
x
beta<-c(1,2)#确定参数实际值
y<-x%*%beta+rnorm(n)#得到服从正态分布的Y数据
A<-diag(c(.05,.05))
betabar<-c(0,0)#设置先验参数
betadraw<-matrix(double(500),ncol=2)#选择600个数0做成一个2列，250行的矩阵
betadraw
for(rep in 1:250){betadraw[rep,]=breg(y,x,betabar,A)}#用breg函数计算参数的后验分布
betadraw
a0<-mean(betadraw[,1])
a1<-mean(betadraw[,2])
mat<-apply(betadraw,2,quantile,probs=c(0.01,.5,.1,.9,.95,.99))#计算不同概率水平下的分位数
mat
mat1<-rbind(beta,mat)#列出与OLS估计值的对比
rownames(mat1)[1]<-"beta"
mat1
beta0<-sort(betadraw[,1])#对参数进行排序，从小到大
beta1<-sort(betadraw[,2])
p1<-dnorm(beta0,mean(betadraw[,1]),1,log=F)#刻画后验正态分布
p2<-dnorm(beta1,mean(betadraw[,2]),1,log=F)
#use ggplot2 to print graph
library(ggplot2)
I = c(rep('beta0',250), rep('beta1',250))
beta = c(beta0, beta1)
df = data.frame(beta = beta, I = I, p = c(p1, p2))
p = ggplot(df, aes(x = beta)) + facet_grid(. ~ I, scale = 'free', space = 'free')
p + geom_histogram(aes(y = ..density..)) 
t = ggplot(df, aes(x = beta, y = p)) + facet_grid(. ~ I, scale = 'free', space = 'free')
t + geom_line(size = 1.2, colour = I('steelblue'))
