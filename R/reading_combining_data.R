# Read, clean, and combine the raw BLSA activity, heart rate, and demographic files.
#
# Outputs:
#   data/intermediate/data_combined.rds  - heart rate + activity merged with demographics
#   data/processed/data_clean.rds        - same, with heart rate == 0 set to NA and hr >= 200 removed

library(tidyverse)
library(lubridate)
library(tidyfun)
library(naniar)

# Raw inputs
ac   = read.csv("data/raw/ActivityCountImputed_2016_Jun.csv")
hr   = read.csv("data/raw/HeartRateImputed_2016_Jun.csv")
demo = read.csv("data/raw/SubjectInfoRaw_2016_Jun.csv")

# Cleaning helpers
source("R/functions/clean_activity_data.R")
source("R/functions/clean_demo_data.R")

# Activity counts and heart rate, each in long format (id, date, timestamp, value)
ac_clean = clean_activity_data(ac) %>%
  rename(ac = value)

hr_clean = clean_activity_data(hr) %>%
  rename(hr = value)

# Rows of hr_clean and ac_clean are assumed to be in the same order (same id/date/minute grid)
ac_hr_combined = cbind(hr_clean, ac = ac_clean$ac)

# Demographics, one row per id and date
demo_clean = clean_demo_data(demo)

# Merge with demographics
data_combined = merge(ac_hr_combined, demo_clean, by = c("id", "date"))

data_final = data_combined %>%
  group_by(id) %>%
  arrange(timestamp)

dir.create("data/intermediate", recursive = TRUE, showWarnings = FALSE)
saveRDS(data_final, file = "data/intermediate/data_combined.rds")

# Cleaning step: treat heart rate of 0 as missing, then drop implausible heart rates (>= 200)
data_combined = readRDS("data/intermediate/data_combined.rds")

data_with_miss = data_combined %>%
  group_by(id, date) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  replace_with_na(list(hr = 0)) %>%
  ungroup()

data_clean = data_with_miss %>%
  filter(hr < 200)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(data_clean, "data/processed/data_clean.rds")
