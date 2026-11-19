clean_demo_data = function(df) {
  days = df %>%
    select(X)
  
  ids_dates = str_split(days$X, pattern = "_")
  
  ids_vec_1 = c()
  ids_vec_2 = c()
  for(i in 1:length(ids_dates)){
    ids_vec_1[i] = ids_dates[[i]][1]
    ids_vec_2[i] = ids_dates[[i]][2]
  }
  
  
  df_clean = df %>%
    mutate(date = ymd(date),
           id = paste(ids_vec_1, "_", ids_vec_2, sep = "")) %>%
    select(-X, -BLSA_id, -visit_id, -dov, -dob)
  
  return(df_clean)
}
