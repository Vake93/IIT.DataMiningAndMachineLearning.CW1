library(NbClust)
library(fpc)
library(MASS)
library(flexclust)

whiteWine<-read.csv("D:/Projects/WhiteWine.csv")
names(whiteWine)
str(whiteWine)
summary(whiteWine)
data.train <- scale(whiteWine[1:11])
summary(data.train)

set.seed(1024)
data.nc <- NbClust(data=data.train, distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
table(data.nc$Best.n[1,])
barplot(table(data.nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria")
wss <- 0
for (i in 1:10){
  wss[i] <-
    sum(kmeans(data.train, centers=i)$withinss)
}
plot(1:10,
     wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

set.seed(2048)
fit.km1 <- kmeans(data.train, 2)
fit.km1$centers
plotcluster(data.train, fit.km1$cluster)
parcoord(data.train, fit.km$cluster)
confuseTable.km1 <- table(whiteWine$quality, fit.km1$cluster)
confuseTable.km1
randIndex(confuseTable.km1)

set.seed(4096)
fit.km2 <- kmeans(data.train, 7)
fit.km2
plotcluster(data.train, fit.km2$cluster)
parcoord(data.train, fit.km2$cluster)
confuseTable.km2 <- table(whiteWine$quality, fit.km2$cluster)
confuseTable.km2
randIndex(confuseTable.km2)
