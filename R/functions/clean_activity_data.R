clean_activity_data = function(df) {
  days = df %>%
    select(X)
  
  ids_dates = str_split(days$X, pattern = "_")
  
  
  dates_vec = c()
  ids_vec_1 = c()
  ids_vec_2 = c()
  for(i in 1:length(ids_dates)){
    dates_vec[i] = ids_dates[[i]][3]
    ids_vec_1[i] = ids_dates[[i]][1]
    ids_vec_2[i] = ids_dates[[i]][2]
  }
  
  
  df_clean = df %>%
    mutate(date = ymd(dates_vec),
           id = paste(ids_vec_1, "_", ids_vec_2, sep = "")) %>%
    select(-X) %>%
    pivot_longer(cols = starts_with("X"), names_to = "time", values_to = "value") %>%
    mutate(time = substring(time, 2),
           timestamp = ymd_hm(paste(date, time))) %>%
    group_by(id) %>%
    arrange(timestamp) %>%
    select(id, value, timestamp, date) %>%
    ungroup()
  
  return(df_clean)
}
