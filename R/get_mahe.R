library(ihr)
library(dplyr)

# Heart rate excursions for every participant, with ihr's default mahe() settings
# on unsmoothed minute-level HR

data = readRDS("data/processed/data_clean.rds") %>%
  rename(time = timestamp)

mahe_df = mahe(data)

saveRDS(mahe_df, file = "data/processed/mahe.rds")

mahe_exc_df = mahe(data, return_type = "df-exc")

saveRDS(mahe_exc_df, file = "data/processed/mahe_exc.rds")
