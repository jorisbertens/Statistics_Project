rm(list=ls())
library(dendextend)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)


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


## KNN imputation
knn.impute(df2, k = 4, cat.var = 1:ncol(df2), to.impute = 1:nrow(df2),
           using = 1:nrow(df2))
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

df$geo.time<-groups_ward

table(df$geo.time,df$groups_ward)
