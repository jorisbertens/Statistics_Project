rm(list=ls())
library(tidyr)
library(naniar)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(mice)
library(tidyverse)
library(zoo)
library(FactoMineR)
library(factoextra)
library(corrgram)
library(mlbench)
library(FSelector)
library(caret)
library(olsrr)


# Load the Data
get_data<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
}

df1 = get_data("Data/EconomicFreedomIndex.csv", sep=";")
df2 = get_data("Data/HumanFreedomIndex.csv")
df2 = df2[(df2$Year=="2016"),]

# Join the Data
join_data<-function(df1,df2){
  df_new <- merge(df1,df2,by.x = c("Country.Name"),by.y = c("Countries"))
  return(df_new)
}

df_join = join_data(df1,df2)

# delete first columns
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]


# delete columns that include "Chang"
df_try = df_join[, -grep("Chang", colnames(df_join))]

df_join[] <- lapply(df_join, gsub, pattern =".", replacement = "", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern =",", replacement = ".", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern ="$", replacement = "", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern ="[-].*", replacement = "", fixed = TRUE)

df_join <- df_join[, -c(37:40)]
df_join <- df_join[, -27]

df_join[df_join == "-"] <- NA
df_join[df_join == ""] <- NA
df_join[df_join == "N/"] <- NA
df_join$Business.regulations <- gsub("[-].*","", df_join$Business.regulations)
df_join$ECONOMIC.FREEDOM..Rank.<- gsub("[-].*","", df_join$ECONOMIC.FREEDOM..Rank.)
df_join$Extra.payments.bribes.favoritism<- gsub("[-].*","", df_join$Extra.payments.bribes.favoritism)

instanceconvert <- colnames(df_join[2:177])

df_join[,instanceconvert] <-
  lapply(df_join[,instanceconvert,drop=FALSE],as.numeric)

sapply(df_join, class)
#write.csv(df_join, file = "Data/df_join.csv")

df_join2 <- df_join

# manually selected columns
cols = c(1, 4 ,6:11, 18, 19, 25, 48, 56, 57, 58, 66, 71, 77, 89, 95, 107, 109, 119, 128, 162, 157, 165, 169, 88, 55, 35, 23)
df_try = df_try[,cols]

# get information about the data
di <- describe(df_try)
di
sapply(df_try, typeof)
sapply(df_try, class)

df_try <- data.frame(lapply(df_try, as.character), stringsAsFactors=FALSE)

df_try[] <- lapply(df_try, gsub, pattern =".", replacement = "", fixed = TRUE)
df_try[] <- lapply(df_try, gsub, pattern =",", replacement = ".", fixed = TRUE)
df_try[] <- lapply(df_try, gsub, pattern ="$", replacement = "", fixed = TRUE)

df_try$Business.regulations <- gsub("[-].*","", df_try$Business.regulations)

df_try$Fiscal.Freedom <- as.numeric(df_try$Fiscal.Freedom)
df_try$Business.Freedom <- as.numeric(df_try$Business.Freedom)
df_try$Labor.Freedom <- as.numeric(df_try$Labor.Freedom)
df_try$Monetary.Freedom <- as.numeric(df_try$Monetary.Freedom)
df_try$Trade.Freedom <- as.numeric(df_try$Trade.Freedom)
df_try$Investment.Freedom <- as.numeric(df_try$Investment.Freedom)
df_try$Financial.Freedom <- as.numeric(df_try$Financial.Freedom)
df_try$Population..Millions. <- as.numeric(df_try$Population..Millions.)
df_try$GDP..Billions..PPP. <- as.numeric(df_try$GDP..Billions..PPP.)
df_try$FDI.Inflow..Millions. <- as.numeric(df_try$FDI.Inflow..Millions.)
df_try$Conflicts <- as.numeric(df_try$Conflicts)
df_try$Security...Safety <- as.numeric(df_try$Security...Safety)
df_try$Domestic.Movement <- as.numeric(df_try$Domestic.Movement)
df_try$Foreign.Movement <- as.numeric(df_try$Foreign.Movement)
df_try$Religion <- as.numeric(df_try$Religion)
df_try$Establishing.and.Operating.Political.Parties <- as.numeric(df_try$Establishing.and.Operating.Political.Parties)
df_try$Operating.Educational <- as.numeric(df_try$Operating.Educational)
df_try$Political.Pressures.and.Controls.on.Media.Content <- as.numeric(df_try$Political.Pressures.and.Controls.on.Media.Content)
df_try$Parental.Authority..In.Marriage <- as.numeric(df_try$Parental.Authority..In.Marriage)
df_try$Transfers.and.subsidies <- as.numeric(df_try$Transfers.and.subsidies)
df_try$Government.enterprises.and.investment <- as.numeric(df_try$Government.enterprises.and.investment)
df_try$Protection.of.property.rights <- as.numeric(df_try$Protection.of.property.rights)
df_try$Money.growth <- as.numeric(df_try$Money.growth)
df_try$Labor.market.regulations <- as.numeric(df_try$Labor.market.regulations)
df_try$Hiring.and.firing.regulations <- as.numeric(df_try$Hiring.and.firing.regulations)
df_try$Starting.a..business <- as.numeric(df_try$Starting.a..business)
df_try$Business.regulations <- as.numeric(df_try$Business.regulations)
df_try$Laws.and.Regulations.that.Influence.Media.Content <- as.numeric(df_try$Laws.and.Regulations.that.Influence.Media.Content)
df_try$Women.s.Security...Safety <- as.numeric(df_try$Women.s.Security...Safety)
df_try$Criminal.Justice <- as.numeric(df_try$Criminal.Justice)
df_try$Unemployment.... <- as.numeric(df_try$Unemployment....)

sapply(df_try, class)


# change - to NA
df_try[df_try == "-"] <- NA


# Missing values
sum(is.na(df_try))

# Missing values per column
map(df_try, ~sum(is.na(.)))

# Method 1 of NA imputation = predicted value from LR to impute
imp <- mice(df_try, method = "norm.predict", m = 1)
data_imp1 <- complete(imp)

corrgram(data_imp1[-1],lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
data_imp1 = data_imp1[-7]
data_imp1 = data_imp1[-9]
data_imp1 = data_imp1[-8]
data_imp1 = data_imp1[-12]
data_imp1 = data_imp1[-12]
data_imp1 = data_imp1[-26]
#write.csv(data_imp1, file = "Data/clean_dataset.csv")

# Method 2 of NA imputation = imputing by the mean
ok <- sapply(df_try, is.numeric)
data_imp2 = df_try
data_imp2[ok] <- lapply(data_imp2[ok], na.aggregate)

# Method 3 of NA imputation = by median?
ok <- sapply(df_try, is.numeric)
data_imp3 = df_try
data_imp3[ok] <- lapply(data_imp3[ok], na.)

# Feature Selection based using Caret

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Unemployment....~., data=data_imp1[-1], method="lm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

fit_glm = glm(Unemployment....~., data = data_imp1[-1])
varImp(fit_glm)

important_features = data_imp1 %>% select(Unemployment....,Trade.Freedom,Laws.and.Regulations.that.Influence.Media.Content,Labor.Freedom,Financial.Freedom,Establishing.and.Operating.Political.Parties,Fiscal.Freedom,Religion,Hiring.and.firing.regulations,Business.regulations,Labor.market.regulations,Domestic.Movement,Criminal.Justice,Business.Freedom,Women.s.Security...Safety,Foreign.Movement)

# Feature selection based on correlation
#df_scale <- as.data.frame( scale(data_imp2[-1]))

#cor_tds <- cor(df_scale, df_scale, method = "pearson")
df_scale <- as.data.frame( scale(data_imp1 ))

cor_tds <- cor(data_imp1[-1], data_imp1[-1], method = "pearson")
cor_df <- data.frame(cor=cor_tds[1:24,25], varn = names(cor_tds[1:24,25]))
cor_df <- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
plot(cor_df$cor_abs)

list_varn <- cor_df %>% filter(cor_abs>0.1)
filter_df <- data.frame(data_imp1) %>% select(Unemployment...., one_of(as.character(list_varn$varn)))
head(filter_df)
print(col_names)

corrgram(filter_df,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")

summary(lm(data = filter_df, Unemployment.... ~ .))

# PCA

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_scale <- as.data.frame( scale(filter_df[-1] ))
df_norm <- as.data.frame(lapply(filter[-1], normalize))

# either go with feature importance done by Alex or using the Correlation
#df_pca = important_features[-1]
#df_pca = filter_df[-1]
#df_pca = df_scale
df_pca = data_imp1[-1]
#df_pca = df_pca[-31]

sapply(df_pca, class)
# get rid of the target variable for to prepare the PCA of the feature variables
#df_features = df_pca[-31]

prcomp(df_pca, scale.unit = TRUE, ncp = 5, graph = TRUE)

res.pca = prcomp(df_pca, scale. = TRUE , graph = FALSE)

plot(res.pca, type="l")

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

fviz_cos2(res.pca, choice = "var", axes = 1)

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

summary(res.pca)
spca = summary(res.pca)
plot(spca$importance[3,], type="l")

pca_df <- data.frame(res.pca$x)
pca_df <- pca_df %>% select(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12)

pca_df$Unemployment = filter_df$Unemployment....
pca_df$County = data_imp1$Country.Name

#write.csv(pca_df, file = "Data/pca.csv")

### LM
#data <- as.data.frame( scale(data_imp1[-1]))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(data_imp1, method="number", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

corrgram(data_imp1[-1],lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
data_imp1[data_imp1$Unemployment.... == 0] <- 0.01
model <- lm(Unemployment.... ~ Fiscal.Freedom+
              Business.Freedom
            +
              Labor.Freedom
            +
              Monetary.Freedom
            +
              Trade.Freedom
            +
              Investment.Freedom
            +
              Financial.Freedom
            +
              Population..Millions.
            +
              GDP..Billions..PPP.
            +
              FDI.Inflow..Millions.
            +
              Conflicts
            +
              Security...Safety
            +
              Domestic.Movement
            +
              Foreign.Movement
            +
              Religion
            +
              Establishing.and.Operating.Political.Parties
            +
              Operating.Educational
            +
              Political.Pressures.and.Controls.on.Media.Content
            +
              Parental.Authority..In.Marriage
            +
              Transfers.and.subsidies
            +
              Government.enterprises.and.investment
            +
              Protection.of.property.rights
            +
              Money.growth
            +
              Labor.market.regulations
            +
              Hiring.and.firing.regulations
            +
              Starting.a..business
            +
              Business.regulations
            +
              Laws.and.Regulations.that.Influence.Media.Content
            +
              Women.s.Security...Safety
            +
              Criminal.Justice
            , data = data_imp1)
k <-ols_step_backward_p(model, prem=0.1)

ols_step_best_subset(model)

model <- lm(sqrt(Unemployment....) ~ Labor.Freedom+
              Trade.Freedom
            +
              Financial.Freedom
            +
              Conflicts
            +
              Religion
            +
              Government.enterprises.and.investment
            +
              Money.growth
            , data = data)

model <- lm(Unemployment.... ~ Money.growth+
              Laws.and.Regulations.that.Influence.Media.Content
            +
              Parental.Authority..In.Marriage
            +
              Operating.Educational
            +
              Religion
            +
              FDI.Inflow..Millions.
            +
              Government.enterprises.and.investment
            +
              Population..Millions.
            +
              Monetary.Freedom
            +
              GDP..Billions..PPP.
            +
              Establishing.and.Operating.Political.Parties
            +
              Protection.of.property.rights
            , data = data)

lm_pca = get_data("Data/pca_df1.csv")
lm_pca = lm_pca %>% select(PC1,PC2,PC3,PC4,PC5, Unemployment)
lm_pca = pca_df %>% select(PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10,PC11,PC12, Unemployment)
model <- lm(Unemployment ~ PC1+
              PC2
            +
              PC3
            +
              PC4
            +
              PC5
            +
              PC6
            +
              PC7
            +
              PC8
            +
              PC9
            +
              PC10
            +
              PC11
            +
              PC12
            , data = lm_pca)


model <- lm(Unemployment.... ~ FDI.Inflow..Millions.
            +
              Establishing.and.Operating.Political.Parties
            +
              Operating.Educational
            +
              Government.enterprises.and.investment
            +
              Protection.of.property.rights
            +
              Money.growth
            +
              Laws.and.Regulations.that.Influence.Media.Content
            +
              Parental.Authority..In.Marriage
            + 
              Monetary.Freedom
            , data = data_imp1)
k <-ols_step_backward_p(model, prem=0.1)

ols_step_best_subset(model)


library(car)
library(plyr)
vif(model)
k <-ols_step_backward_p(model, prem=0.1)

colnames(data_imp1)
library(Hmisc)
pca_df$bin_unempl = cut(pca_df$Unemployment, 6)
split(pca_df, cut2(pca_df$Unemployment, g=4))
write.csv(pca_df, file = "Data/pca_dataset.csv")

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Unemployment....~., data=data_imp1[-1], method="lm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

fit_glm = glm(Unemployment....~., data = data_imp1[-1])
varImp(fit_glm)

important_features = data_imp1 %>% select(Trade.Freedom,Laws.and.Regulations.that.Influence.Media.Content,Labor.Freedom,Financial.Freedom,Establishing.and.Operating.Political.Parties,Fiscal.Freedom,Religion,Hiring.and.firing.regulations,Business.regulations,Labor.market.regulations,Domestic.Movement,Criminal.Justice,Business.Freedom,Women.s.Security...Safety,Foreign.Movement)
