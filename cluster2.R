rm(list=ls())
library(dendextend)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)
library(gdata)
library(dplyr)
library(reshape2)

get_full_data<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
}

df = get_full_data("Data/full_dataset.csv")
df = df[(df$variable=="X2017"),]

df2 = df %>% select(4:15)

d<-dist(df2)

# Elbow method
fviz_nbclust(df2, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(df2, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

## Impute by mean
for(i in 1:ncol(df2)){
  df2[is.na(df2[,i]), i] <- mean(df2[,i], na.rm = TRUE)
}

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df2, hcut, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

library("NbClust")
nb <- NbClust(df2, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

fit_ward<-hclust(d,method="ward.D")
plot(fit_ward)
rect.hclust(fit_ward, k=3, border="red")
groups_ward <- cutree(fit_ward, k=3)

df$groups_ward<-groups_ward

table(df$geo.time,df$groups_ward)

plot(fit_ward, labels=df$geo.time)
rect.hclust(fit_ward, k=3, border="red")
