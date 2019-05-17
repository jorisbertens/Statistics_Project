rm(list=ls())

library(gdata)
library(dplyr)

## Load data
df1 = get_data_csv("Data/airtransport.csv")
df2 = get_data_csv("Data/fertility_rate.csv")
df3 = get_data_csv("Data/gdp_pps.csv")
df4 = get_data_csv("Data/hr_science_tech.csv")
df5 = get_data_csv("Data/life_expectancy.csv")
df6 = get_data_csv("Data/material_depreviation.csv")
df7 = get_data_csv("Data/motorway.csv")
df8 = get_data_csv("Data/unemployment_rate.csv")
df9 = get_data_csv("Data/population_density.csv")
df10 = get_data_csv("Data/risk_poverty.csv")
df11 = get_data_csv("Data/tertiary_education.csv")
df12 = get_data_csv("Data/tourism_bed_places.csv")

# Change the value column to the specific column value name
df1 = change_col_name(df1, "value","airtransport")
df2 = change_col_name(df2, "value","fertility_rate")
df3 = change_col_name(df3, "value","gdp_pps")
df4 = change_col_name(df4, "value","hr_science_tech")
df5 = change_col_name(df5, "value","life_expectancy")
df6 = change_col_name(df6, "value","material_depreviation")
df7 = change_col_name(df7, "value","motorway")
df8 = change_col_name(df8, "value","unemployment_rate")
df9 = change_col_name(df9, "value","population_density")
df10 = change_col_name(df10, "value","risk_poverty")
df11 = change_col_name(df11, "value","tertiary_education")
df12 = change_col_name(df12, "value","tourism_bed_places")

# delete duplicates in city and year
df1 = df1 %>% distinct(geo.time, variable, .keep_all = TRUE)
df2 = df2 %>% distinct(geo.time, variable, .keep_all = TRUE)
df3 = df3 %>% distinct(geo.time, variable, .keep_all = TRUE)
df4 = df4 %>% distinct(geo.time, variable, .keep_all = TRUE)
df5 = df5 %>% distinct(geo.time, variable, .keep_all = TRUE)
df6 = df6 %>% distinct(geo.time, variable, .keep_all = TRUE)
df7 = df7 %>% distinct(geo.time, variable, .keep_all = TRUE)
df8 = df8 %>% distinct(geo.time, variable, .keep_all = TRUE)
df9 = df9 %>% distinct(geo.time, variable, .keep_all = TRUE)
df10 = df10 %>% distinct(geo.time, variable, .keep_all = TRUE)
df11 = df11 %>% distinct(geo.time, variable, .keep_all = TRUE)
df12 = df12 %>% distinct(geo.time, variable, .keep_all = TRUE)

# Merge all the data
df_join = join_data(df1, df2)
df_join = join_data(df_join, df3)
df_join = join_data(df_join, df4)
df_join = join_data(df_join, df5)
df_join = join_data(df_join, df6)
df_join = join_data(df_join, df7)
df_join = join_data(df_join, df8)
df_join = join_data(df_join, df9)
df_join = join_data(df_join, df10)
df_join = join_data(df_join, df11)
df_join = join_data(df_join, df12)

#change : to NA value
df_join[df_join==":"]<-NA

#Delete some empty noizy rows
df_join = df_join[!is.na(df_join$geo.time), ]
df_join = df_join[!(df_join$geo.time=="Code:"),]
df_join = df_join[!(df_join$geo.time=="Date of extraction:"),]
df_join = df_join[!(df_join$geo.time=="General Disclaimer of the EC website:"),]
df_join = df_join[!(df_join$geo.time=="Hyperlink to the table:"),]
df_join = df_join[!(df_join$geo.time=="Last update:"),]
df_join = df_join[!(df_join$geo.time=="Source of Data:"),]

#df = clean_data()
#write.csv(df, file = "Data/full_dataset.csv")

# Check the null values
sum(is.na(df_join$airtransport))
sum(is.na(df_join$fertility_rate))
sum(is.na(df_join$gdp_pps))
sum(is.na(df_join$hr_science_tech))
sum(is.na(df_join$life_expectancy))
sum(is.na(df_join$material_depreviation))
sum(is.na(df_join$motorway))
sum(is.na(df_join$population_density))
sum(is.na(df_join$risk_poverty))
sum(is.na(df_join$tertiary_education))
sum(is.na(df_join$tourism_bed_places))
sum(is.na(df_join$unemployment_rate))