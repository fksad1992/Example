###复合中点规则求积分p115
unnormpost<-function(pi,pr){
        like<-pi^7*(1-pi)^43
        like*pr
}
like = rep(0,100)
        endpoints<-c(0,0.1,0.2,0.3,0.4)
        prior<-c(2.5,5.0,2.0,0.5)
        h<-0.001
        integral<-0
        for(i in 1:4){
                mypis<-seq(endpoints[i]+h/2,endpoints[i+1]-h/2,by=h)
                heights<-unnormpost(mypis,prior[i])
                like[i]<-heights/prior[i]
                integral<-integral+sum(heights*h)         
        }
integral

plot(mypis,prior*like/integral,type="l",ylab="正规化后验")

