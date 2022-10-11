rm(list=ls())
#地形生境划分
# 加载包
library(dplyr)       # 数据预处理
library(ggplot2)     # 数据可视化
library(cluster)     # 实现聚类算法
library(factoextra)  # 聚类结果可视化
library(dendextend)

## 加载数据
mydata <- read.csv("elevtion.csv")

## 选择要聚类的列，E，slope，convexity，aspect
mydata1 <- mydata[,c(3,4,5,6)]

## 计算欧几里得距离
d <- dist(mydata1, method = "euclidean")

## ward聚类
hc <- hclust(d, method = 'ward.D')

plot(hc)

plot(hc, hang = -1, cex = 0.8,main = "Dendrogram of hlcust(agglomerative)")

cut1 <- cutree(hc, k = 6)

mydata$habitats <- cut1

head(mydata)

write.csv(mydata,"habitat.csv",sep =",", row.names =T)