rm(list=ls())

# use these two packages
library(FactoMineR)
library(factoextra)

# I basically followed this guide to do everything for the PCA
http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

get_full_data<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
}

df = get_full_data("Data/full_dataset.csv")
df = df[(df$variable=="X2017"),]

# get rid of unnecessary stuff
df2 = df %>% select(4:15)

# get rid of the target variable for to prepare the PCA of the feature variables
df_features = df2[-8]

PCA(df_features, scale.unit = TRUE, ncp = 5, graph = TRUE)

res.pca = PCA(df_features, graph = FALSE)

print(res.pca)

eig.val = get_eigenvalue(res.pca)
eig.val


fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

var = get_pca_var(res.pca)

head(var$coord)
head(var$cos2)
head(var$contrib)
head(var$coord, 4)

fviz_pca_var(res.pca, col.var = "black")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_cos2(res.pca, choice = "var", axes = 1:4)

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

head(var$contrib, 4)

library("corrplot")
corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)

fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)
