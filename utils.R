## Place with datasets
library(gdata)
library(dplyr)
library(reshape2)

# load the data
get_data_csv<-function(data, sep=","){
  df <- read.csv(data, header = TRUE, sep)
  df = select(df, geo.time, X2013, X2014, X2015, X2016, X2017)
  df = melt(df,id =c("geo.time"))
  return(df)
}

get_data_xls<-function(data, sep=","){
  df <- read.table(data, header = TRUE, sep)
  return(df)
}

# change columns names before the merge to stop any conflict from happening
change_col_name<-function(data,old_col,new_col){
  names(data)[names(data) == old_col] <- new_col
  return(data)
}

# function to join the dataset by two keys (TIME and CITIES)
join_data<-function(df1,df2){
  df_new <- merge(df1,df2,by.x = c("geo.time","variable"),by.y = c("geo.time","variable"))
  return(df_new)
}

clean_data<-function(){
  
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
  return(df_join)
}





