## Place with datasets

# load the data
get_data<-function(data){
  df <- read.csv(data, header = TRUE)
  return(df)
}

# change columns names before the merge to stop any conflict from happening
change_col_name<-function(data,old_col,new_col){
  names(data)[names(data) == old_col] <- new_col
  return(data)
}

# function to join the dataset by two keys (TIME and CITIES)
join_data<-function(df1,df2){
  df_new <- merge(df1,df2,by.x = c("TIME","CITIES"),by.y = c("TIME","CITIES"))
  return(df_new)
}






