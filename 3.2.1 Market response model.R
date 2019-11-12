library(ggplot2)

q21<-read.csv(file="~/Desktop/YEAR 4/YEAR 4 SEM 1/STAT3613/project/q2.csv",header=T)

#sales <- c(19,19,19,20,19,17,14,8,7,5,5)
#price <- c(20,25,30,35,40,45,50,55,60,65,70)
price <- q21[,2]
demand <- q21[,3]

ggplot(mapping=aes(x=price,y=demand))+
  geom_line()+
  geom_point()+
  labs(x="price.",y="demand")

#data= specifies a data frame containing the variables
fit0<-nls(f=demand~a*price^-b,data=q21,start=list(a=100,b=1.5))
summary(fit0)

r2<-1-(sum(residuals(fit0)^2)/sum((fit0$m$lhs()-mean(fit0$m$lhs()))^2))
r2

#power series model 1
fit1<-nls(f=demand~a+b*price+c*price^2,data=q21,start=list(a=200,b=-100,c=20))
summary(fit1)

r2<-1-(sum(residuals(fit1)^2)/sum((fit1$m$lhs()-mean(fit1$m$lhs()))^2))
r2

#power series model 2
fit2<-nls(f=demand~a+c*price^2,data=q21,start=list(a=200,c=20))
summary(fit2)

r2<-1-(sum(residuals(fit2)^2)/sum((fit2$m$lhs()-mean(fit2$m$lhs()))^2))
r2

#logistic model
d <- min(demand)
a <- max(demand) - d
c <- 4*(19-17)/(40-45)/a
b <- -50*c
fit3 <- nls(f=demand~a/(1+exp(-b-c*price))+d,data=q21,start=list(a=a,b=b,c=c,d=d))
summary(fit3)

r2<-1-(sum(residuals(fit3)^2)/sum((fit3$m$lhs()-mean(fit3$m$lhs()))^2))
r2

#profit function
profit<-function(price,fit){
  cost<-15
  nd<-data.frame(price=price)
  demand<-predict(fit,newdata=nd)
  profit<-demand*(price-cost)
  return(profit)
}

ggplot(data=q21,mapping=aes(x=price,y=profit(price,fit3)))+
  geom_line()+
  geom_point() +
  labs(y="profit")

#optimization
op<-optimize(f=profit,fit=fit3,interval=c(20,70),maximum =TRUE)
op

sprice <- 46
profit(sprice,fit3)




