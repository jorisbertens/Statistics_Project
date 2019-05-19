rm(list=ls())

df = get_full_data("Data/full_dataset.csv")

dim(df)
nrow(df)
ncol(df)

df = df %>% select(3:15)

One <- matrix(rep(1,nrow(df2)), nrow=nrow(df2))
One <- matrix(1, nrow=nrow(df2))
head(One)

n <- nrow(df2)

mean<-t(One) %*% as.matrix(df2)/n

df_join17 = df[(df$variable=="X2017"),]
df_join17 = df_join17 %>% select(2:13)

library(FactoMineR)
library(factoextra)

plot(df_join17, xlim = c(0,1000))

beer<-data.frame(price=c(7.89,4.79,7.65,6.39,4.50,6.25),Quality=c(10,4,9,7,3,6))
row.names(beer)<-c("Bud Ice Draft","Milwalkee's Best","Coor's Light","Miller Genuine Draft","Schilitz","Michelob")

df_join17.std=as.data.frame(scale(df_join17))
plot(beer.std, xlim=c(-2,1.5))
text(beer.std,labels=row.names(beer.std), pos=2)

d.eucl<-dist(df_join17.std)
d.eucl

fit_complete<-hclust(d.eucl, method="complete")
plot(fit_complete)
rect.hclust(fit_complete, k=5, border="red")

groups_complete <- cutree(fit_complete, k=3)

fit_centroid<-hclust(d.eucl, method="centroid")
plot(fit_centroid)
rect.hclust(fit_centroid, k=, border="red")

groups_centroid <- cutree(fit_centroid, k=5)

#install.packages("dendextend")
library(dendextend)

# Create two dendrograms
dend_complete <- as.dendrogram (fit_complete)
dend_centroid <- as.dendrogram (fit_centroid)
# Create a list to hold dendrograms
dend_list <- dendlist(dend_complete, dend_centroid)

tanglegram(dend_complete, dend_centroid)

entanglement(dend_complete, dend_centroid)

cor.dendlist(dend_list, method = "cophenetic")

cor.dendlist(dend_list, method = "baker")

cor_cophenetic(dend_complete, dend_centroid)

cor_bakers_gamma(dend_complete, dend_centroid)

fit_single<-hclust(d.eucl, method="single")
plot(fit_single)

fit_average<-hclust(d.eucl, method="average")
plot(fit_average)

# Create multiple dendrograms by chaining
dend_complete <- df_join17.std %>% dist %>% hclust("complete") %>% as.dendrogram
dend_centroid <- df_join17.std %>% dist %>% hclust("centroid") %>% as.dendrogram
dend_single <- df_join17.std %>% dist %>% hclust("single") %>% as.dendrogram
dend_average <- df_join17.std %>% dist %>% hclust("average") %>% as.dendrogram

# Compute correlation matrix
dend_list <- dendlist("Complete" = dend_complete, "Single" = dend_single,
                      "Average" = dend_average, "Centroid" = dend_centroid)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

library(corrplot)

corrplot(cors, "pie", "lower")
