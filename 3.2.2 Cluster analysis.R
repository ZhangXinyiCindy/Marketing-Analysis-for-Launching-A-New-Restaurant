library(psych)
library(reshape2)
library(ggplot2)
q22<-read.csv(file="~/Desktop/YEAR 4/YEAR 4 SEM 1/STAT3613/project/q22.csv",header=T)
#(a)
#Standardization
st <- scale(data.frame(q22[,2:6]))
#Squared Euclidean distance
dist <- dist(st ,method="euclidean")^2
#Ward's method
fit <- hclust(dist, method="ward.D")
history <- cbind(fit$merge,fit$height)
#Plot
ggplot(mapping=aes(x=1:length(fit$height),y=fit$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")
#Dendrogram
plot(fit,labels=q22$id,hang=-1,sub="",xlab="",main="Dendrogram (Ward's method)")
axis(side = 2, at = seq(0, 100, 20))

#(b)
#clustering
cluster <- cutree(fit,k=3)
id <- q22$id
sol <- data.frame(cluster,st,id)
sol[order(sol$cluster),c(1,7)]
#cluster means
tb <- aggregate(x=sol[,2:6], by=list(cluster=sol$cluster),FUN=mean)
print(tb,digits=2)

tbm <- melt(tb,id.vars='cluster')
tbm$cluster <- factor(tbm$cluster)
ggplot(tbm,
       aes(x = variable, y = value, group = cluster, colour = cluster)) +
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")

#cluster size
table(sol$cluster)

#gender
tbp<-table(q22[,"gender"],sol$cluster)
prop.table(tbp,margin=2)

#faculty
tbp<-table(q22[,"faculty"],sol$cluster)
prop.table(tbp,margin=2)

#status
tbp<-table(q22[,"status"],sol$cluster)
prop.table(tbp,margin=2)

#age
tb<-aggregate(x=q22[,"age"], by=list(cluster=sol$cluster),FUN=mean)
print(tb,digits=2)

#expense
tb<-aggregate(x=q22[,"expense"], by=list(cluster=sol$cluster),FUN=mean)
print(tb,digits=2)


