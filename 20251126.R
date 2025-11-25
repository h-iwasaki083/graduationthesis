library(readxl)
df <- read_excel("data/exceldata_170_1119.xlsx")
View(df)

X <- scale(df[, c("Game_SP","SNS_Chat","SNS_View","SNS_Post",
                  "Watch_Media","Reading","Study_SP","Research","Shop_SP")])

library(ggplot2)

wss <- sapply(1:10, function(k){
  kmeans(X, centers = k, nstart = 20)$tot.withinss
})

plot(1:10, wss, type="b", pch=19, xlab="k", ylab="WSS")


library(cluster)

sil <- sapply(2:10, function(k){
  km <- kmeans(X, centers = k, nstart = 20)
  ss <- silhouette(km$cluster, dist(X))
  mean(ss[, 3])
})

plot(2:10, sil, type="b", pch=19, xlab="k", ylab="Silhouette")

set.seed(123)
km2 <- kmeans(X, centers = 2, nstart = 20)
table(km2$cluster)

# aggregate(df[,1:9], by=list(cluster=km2$cluster), mean)



