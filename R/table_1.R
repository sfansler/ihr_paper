library(gtsummary)
library(tableone)
library(tidyverse)
data = readRDS("data/processed/data_clean.rds") %>%
  rename(time = timestamp) %>%
  mutate(bmi = weight / height^2)

valid_subjects = data %>%
  group_by(id, date) %>%
  summarize(n()) %>%
  filter(`n()` >= 1440*.9) %>%
  ungroup(date) %>%
  summarize(n()) %>%
  filter(`n()` >= 3) %>%
  select(id) %>%
  unlist()

data_valid = data %>%
  filter(id %in% valid_subjects)

demo = data_valid %>%
  mutate(bmi = weight / height^2) %>%
  select(id, Age = age, BMI = bmi, Sex = sex) %>%
  group_by(id) %>%
  filter(Age == min(Age)) %>%
  ungroup() %>%
  select(-id) %>%
  distinct()

tbl1 = tbl_summary(demo,
                   statistic = list(all_continuous() ~ "{mean} ({sd})",
                                    all_categorical() ~ "{p}%")) %>%
  as_flex_table()


CreateTableOne(data = demo)
