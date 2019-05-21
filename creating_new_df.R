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

# change - to NA
df_try[df_try=="-"]<-NA
df_try[df_try=="N/A"]<-NA

# download df
write.csv(df_try, file = "Data/unemployment_analysis.csv")
