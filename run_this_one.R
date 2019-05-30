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

# replace non numeric values in the columns
df_join[] <- lapply(df_join, gsub, pattern =".", replacement = "", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern =",", replacement = ".", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern ="$", replacement = "", fixed = TRUE)
df_join[] <- lapply(df_join, gsub, pattern ="[-].*", replacement = "", fixed = TRUE)

# deletion of columns with too many NULL values.
df_join <- df_join[, -c(37:40)]
df_join <- df_join[, -27]
df_join <-df_join[, -c(86:92)]
df_join <-df_join[, -c(102:108)]
df_join$Unemployment <- df_join$Unemployment....
df_join <-df_join[, -32]
df_join <- df_join[, -c(93:99)]
df_join <- df_join[, -c(85:95)]
df_join <- df_join[, -c(38:40)]

# replace symbols for NA
df_join[df_join == "-"] <- NA
df_join[df_join == ""] <- NA
df_join[df_join == "N/"] <- NA

# change ranged value column to numeric values
df_join$Business.regulations <- gsub("[-].*","", df_join$Business.regulations)
df_join$ECONOMIC.FREEDOM..Rank.<- gsub("[-].*","", df_join$ECONOMIC.FREEDOM..Rank.)
df_join$Extra.payments.bribes.favoritism<- gsub("[-].*","", df_join$Extra.payments.bribes.favoritism)

# change all columns except for the first column to numeric
instanceconvert <- colnames(df_join[2:142])

df_join[,instanceconvert] <-
  lapply(df_join[,instanceconvert,drop=FALSE],as.numeric)

df_join2 = df_join
sapply(df_join, class)

#write.csv(df_join, file = "Data/df_join.csv")

# Missing values
sum(is.na(df_join2))

# Missing values per column
map(df_join2, ~sum(is.na(.)))

# Method 1 of NA imputation = predicted value from LR to impute
#imp <- mice(df_join2[1:142], method = "norm.predict", m = 1)
#data_imp1 <- complete(imp)

data_imp1 = get_data("Data/all_var_df.csv")
data_imp1 = data_imp1[-1]
data_imp1 = data_imp1[-c(117,129),]

# Missing values
sum(is.na(data_imp1))
df_scale <- as.data.frame( scale(data_imp1[-1] ))

# Check for correlation of the independent variable to the target
cor_tds <- cor(df_scale, df_scale, method = "pearson")
cor_df <- data.frame(cor=cor_tds[1:140,141], varn = names(cor_tds[1:140,141]))
cor_df <- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
plot(cor_df$cor_abs)
abline(0.2, 0)
# Check for correlation of the independent variable to the target
cor_tds <- cor(data_imp1[-1], data_imp1[-1], method = "pearson")
cor_df <- data.frame(cor=cor_tds[1:140,141], varn = names(cor_tds[1:140,141]))
cor_df <- cor_df%>%mutate(cor_abs = abs(cor)) %>% arrange(desc(cor_abs))
plot(cor_df$cor_abs)

# delete variables that have a correlation to the target lower than 0.2
list_varn <- cor_df %>% filter(cor_abs>0.2)
filter_df <- data.frame(data_imp1) %>% select(Unemployment, one_of(as.character(list_varn$varn)))
head(filter_df)

# Check the correlation between independent variables using a corrgram and reduce multicollinearity.
corrgram(filter_df,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")

https://www.listendata.com/2015/05/correcting-multicollinearity-with-r.html

# create the model including all independent variables and use vfit function to decrease multicollinearity.
model <- lm(Unemployment ~ ., data = filter_df)

vif(model)
drop=TRUE
# the VIF threshold
threshold=2.5

aftervif=data.frame()
while(drop==TRUE){
  vfit=vif(model)
  aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
  if(max(vfit)>threshold){model=
    update(model,as.formula((paste(".","~",".","-", names(which.max(vfit))))))}
  else{ drop=FALSE}}
print(model)

t_aftervif = as.data.frame(t(aftervif))
edit(t_aftervif)

vfit_d = as.data.frame(vfit)

df_vfit = filter_df[, c(-4)]
df_vfit = filter_df %>% select(X5.Year.GDP.Growth.Rate....,Integrity.of.the.legal.system,Change.in.Fiscal.Freedom.from.2015,Hours.Regulations,Mean.tariff.rate.DATA,Domestic.Movement,Inflation....,Unemployment)

corrgram(df_vfit,lower.panel=panel.cor,upper.panel=panel.pie, cor.method = "pearson")
M<-cor(df_vfit)
corrplot(M, method="color")
df_vfit = filter_df %>% select(Change.in.Fiscal.Freedom.from.2015,Size.of.Government,Hours.Regulations,Inheritance.Rights..Daughters,Inflation....,Integrity.of.the.legal.system,Change.in.Trade.Freedom.from.2015,Standard.deviation.of.tariff.rates,Mean.tariff.rate.DATA,Unemployment)
df_vfit = filter_df %>% select(Change.in.Fiscal.Freedom.from.2015,Size.of.Government,Hours.Regulations,Inheritance.Rights..Daughters,Inflation....,Unemployment)
df_vfit = filter_df %>% select(Change.in.Fiscal.Freedom.from.2015,Judicial.independence,Size.of.Government,Gov.t.Spending,Hours.Regulations,Impartial.courts,Inheritance.Rights..Daughters,Mean.tariff.rate.DATA,Inflation....,Unemployment)
# Final LR model using backward selection on the variables returned by the reduction of multicollinearity function

model <- lm(Unemployment ~ ., data = df_vfit)

k <-ols_step_backward_p(model, prem=0.1)

df_regression = data_imp1
df_regression2 = data_imp1[, c(-1, -142)]
df_regression$pred = predict(model, newdata = df_regression2)

### Normalize variables


# Plot for normality before log
plotNormalHistogram(df_regression$Unemployment)
# Plot for normality after log
df_regression$Unemployment_log = log(df_regression$Unemployment)
plotNormalHistogram(df_regression$Unemployment_log)

### Scatter plot pred vs actuals
#windows() 
plot(df_regression$Unemployment, df_regression$pred, main="Scatterplot Example",
     xlab="Ground truth", ylab="Estimations", pch=3) 
abline(a=0, b=1)
plot(df_regression$Unemployment_log, df_regression$pred, main="Scatterplot Example",
     xlab="Ground truth", ylab="Estimations", pch=3) 
abline(a=0,b=1)
plot(df_regression$Unemployment_exp, df_regression$pred, main="Scatterplot Example",
     xlab="Ground truth", ylab="Estimations", pch=3) 

library(ggplot2)

### Residual PLOT
reg.res = resid(model) 
plot(df_regression$Unemployment_log, reg.res)
abline(0, 0)

df_regression$Unemployment_exp <- exp(df_regression$Unemployment)

### QQ PLOT
qqnorm(df_regression$Unemployment,
       ylab="Sample Quantiles for Turbidity")
qqline(df_regression$Unemployment,
       col="red")

#PCA
df_pca = filter_df
res.pca = prcomp(df_pca[-1], scale. = TRUE , graph = FALSE)

plot(res.pca, type="l")

print(res.pca)

eig.val = get_eigenvalue(res.pca)
eig.val


fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 60))

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

fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)

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

pca_df <- data.frame(res.pca$x)
pca_df <- pca_df %>% select(PC1,PC2,PC3,PC4)

pca_df$Unemployment = filter_df$Unemployment
pca_df$County = data_imp1$Country.Name

#write.csv(pca_df, file = "Data/pca_whole3.csv")
#write.csv(data_imp1, file = "Data/all_var_df.csv")

