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

# delete a lot of columns

df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]
df_join= df_join[-2]

df_try = df_join[, -grep("Chang", colnames(df_join))]

colnames(df_try)
