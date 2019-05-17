library(dplyr)
rm(list=ls())

# Import the datasets
df1 = get_data("Data/beds/beds_1000.csv")
df2 = get_data("Data/beds/beds_tourist.csv")
df3 = get_data("Data/companies/tot_companies.csv")
df4 = get_data("Data/culture/museum_vis.csv")
df5 = get_data("Data/employment/eco_active.csv")
df6 = get_data("Data/other_nationals/foreigners.csv")
df7 = get_data("Data/other_nationals/foreign_eu.csv")
df8 = get_data("Data/other_nationals/foreign_non_eu.csv")
df9 = get_data("Data/population/population.csv")
df10 = get_data("Data/private_households/households.csv")
df11 = get_data("Data/transport/cost_pt.csv")

# Change common column name value to something else
df1 = change_col_name(df1,"Value","beds_1000")
df2 = change_col_name(df2,"Value","beds_tourist")
df3 = change_col_name(df3,"Value","tot_companies")
df4 = change_col_name(df4,"Value","museum_vis")
df5 = change_col_name(df5,"Value","employed_tot")
df6 = change_col_name(df6,"Value","foreign_born")
df7 = change_col_name(df7,"Value","foreign_eu")
df8 = change_col_name(df8,"Value","foreign_non_eu")
df9 = change_col_name(df9,"Value","population")
df10 = change_col_name(df10,"Value","household")
df11 = change_col_name(df11,"Value","cost_pt")

"""
df_list = c(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)
for (df in df_list){
  #df = df[,-c(3)]
  return(df)
}
"""

df1 = df1[,-c(3)]
df2 = df2[,-c(3)]
df3 = df3[,-c(3)]
df4 = df4[,-c(3)]
df5 = df5[,-c(3)]
df6 = df6[,-c(3)]
df7 = df7[,-c(3)]
df8 = df8[,-c(3)]
df9 = df9[,-c(3)]
df10 = df10[,-c(3)]
df11 = df11[,-c(3)]
df9 =df9[,-c(4)]

# delete duplicate columns for the columns TIME and CITIES
df1 = df1 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df2 = df2 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df3 = df3 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df4 = df4 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df5 = df5 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df6 = df6 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df7 = df7 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df8 = df8 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df9 = df9 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df10 = df10 %>% distinct(TIME, CITIES, .keep_all = TRUE)
df11 = df11 %>% distinct(TIME, CITIES, .keep_all = TRUE)


# Join the datasets
df_joined = join_data(df1,df2)
df_joined = join_data(df_joined, df3)
df_joined = join_data(df_joined, df4)
df_joined = join_data(df_joined, df5)
df_joined = join_data(df_joined, df6)
df_joined = join_data(df_joined, df7)
df_joined = join_data(df_joined, df8)
df_joined = join_data(df_joined, df9)
df_joined = join_data(df_joined, df10)
df_joined = join_data(df_joined, df11)

# get the subset of selected cities
df_joined = select_cities(df_joined)

sum(df_joined

