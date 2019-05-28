rm(list=ls())
library(dendextend)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)
library(gdata)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library("dendextend")
library(cluster)
library(ape)

get_full_data<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
}

df = get_full_data("Data/pca_df1.csv")

df2 = df %>% select(-1,-2,-33)
df2 = df %>% select(-1,-7,-8)

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

df3 = column_to_rownames(df, var = "County")
df3 = df3 %>% select(-1,-7)
dst <- dist(df3)

fit_ward<-hclust(dst,method="ward.D")
plot(fit_ward)
rect.hclust(fit_ward, k=3, border="red")
groups_ward <- cutree(fit_ward, k=3)

df$groups_ward<-groups_ward

table(df$groups_ward,df$groups_ward)


plot(fit_ward, labels=df$County)
rect.hclust(fit_ward, k=3, border="red")

cols <- brewer.pal(3, "Dark2")

plot(as.phylo(fit_ward), type = "fan", tip.color=cols[groups_ward],label.offset = 1, cex = 0.9, font =1)


