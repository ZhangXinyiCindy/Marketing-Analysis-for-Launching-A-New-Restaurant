library(car)

restaurant<-read.csv("C:/Users/thinkpad/Desktop/STAT 3613/project/restaurant.csv",header=T)

restaurant$Foodstyle<-factor(restaurant$Foodstyle,labels=c("asian","western"))
restaurant$Price<-factor(restaurant$Price,labels=c("$45","$75"))
restaurant$Waitingtime<-factor(restaurant$Waitingtime,labels=c("10min","20min"))

restaurant

contrasts(restaurant$Foodstyle)
contrasts(restaurant$Price)
contrasts(restaurant$Waitingtime)

mod.form <- "cbind(c,nc) ~ Foodstyle+Price+Waitingtime"
logit <- glm(mod.form, family="binomial", data=restaurant)

#type III anova test for each factor
Anova(logit,type="III")

summary(logit)
