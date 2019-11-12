library(ca)

attribute<-read.csv("C:/Users/thinkpad/Desktop/STAT 3613/project/attribute.csv",header=T,row.names = 1)

attribute

fit<-ca(obj=attribute)

summary(fit)

cacoord(fit,type="principal",dim=1:2)

par(mar=c(4,4,1,4))
plot(fit)
