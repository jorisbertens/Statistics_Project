rm(list=ls())

get_data<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
}

df1 = get_data("Data/EconomicFreedomIndex.csv", sep=";")

df2 = get_data("Data/HumanFreedomIndex.csv")
df2 = df2[(df2$Year=="2016"),]

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

# manually selected columns
cols = c(1, 4 ,6:11, 18, 19, 25, 48, 56, 57, 58, 66, 71, 77, 89, 95, 107, 109, 119, 128, 162, 157, 165, 169, 88, 55, 35, 23)
df_try = df_try[,cols]

library(tidyr)
library(naniar)
library(dplyr)
library(tidyverse)



library(Hmisc)
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
#df_try <- df_try[df_try == "-"] <- NA

# Missing values

miss = df_try[!complete.cases(df_try), ]
is.na(df_try[1,30])

#install.packages("mice")
library(mice)

df_try[is.na(df_try)] <- mice(df_try, m=5, maxit = 50, method = 'pmm', seed = 500)