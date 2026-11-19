library(ihr)
library(tidyverse)
library(hms)
library(lubridate)
library(gridExtra)
library(patchwork)

# Distribution of heart rate excursions overall and by time of day and age:
# Supplementary Figures 6 and 7 and the excursion numbers in the Results.
# Requires data/processed/mahe_exc.rds from R/get_mahe.R.

data = readRDS("data/processed/data_clean.rds") %>%
  rename(time = timestamp)

valid_subjects = data %>%
  dplyr::group_by(id, date) %>%
  summarize(n = n(), .groups = 'drop') %>%
  filter(n >= 1440*.9) %>%
  group_by(id) %>%
  summarize(n_days = n(), .groups = 'drop') %>%
  filter(n_days >= 3) %>%
  dplyr::select(id) %>%
  unlist()

data_valid = data %>%
  filter(id %in% valid_subjects)

mahe_exc_df = unnest(readRDS("data/processed/mahe_exc.rds"), cols = c(MAHE))

excursion_df_metrics = mahe_exc_df %>%
  filter(id %in% valid_subjects) %>%
  na.omit() %>%
  mutate(start_time = as_datetime(start_time),
         peak_time = as_datetime(peak_time),
         end_time = as_datetime(end_time),
         amplitude = plus)

ages = data_valid %>%
  dplyr::select(id, age) %>%
  distinct() %>%
  dplyr::group_by(id) %>%
  filter(age == age[1]) %>%
  ungroup()

exc_age = inner_join(excursion_df_metrics, ages, by = "id") %>%
  group_by(id) %>%
  mutate(start_day = as_date(start_time),
         day_num = as.numeric(start_day) - as.numeric(start_day)[1] + 1) %>%
  ungroup() %>%
  mutate(age_cat = case_when(age <= 39 ~ "26-39",
                             between(age, 40, 49) ~ "40-49",
                             between(age, 50, 59) ~ "50-59",
                             between(age, 60, 69) ~ "60-69",
                             between(age, 70, 79) ~ "70-79",
                             .default = "80+"),
         age_cat = factor(age_cat, levels = c("26-39", "40-49", "50-59", "60-69", "70-79", "80+")),
         start_hour = hour(start_time),
         time_period = case_when(
           between(start_hour, 5, 9) ~ "Morning (5:00-9:59)",
           between(start_hour, 10, 16) ~ "Midday (10:00-16:59)",
           between(start_hour, 17, 21) ~ "Evening (17:00-21:59)",
           TRUE ~ "Night (22:00-4:59)"
         ),
         time_period = factor(time_period, levels = c("Morning (5:00-9:59)", "Midday (10:00-16:59)", "Evening (17:00-21:59)", "Night (22:00-4:59)")))


#### Supplementary Figure 6: histograms ####

freq_hist = exc_age %>%
  group_by(id, day_num) %>%
  summarize(daily_exc_n = n()) %>%
  ungroup(day_num) %>%
  summarize(mean_daily_freq = mean(daily_exc_n)) %>%
  ggplot(aes(x = mean_daily_freq)) +
  geom_histogram(fill = 'gray', color = 'black',  bins = 13) +
  theme_bw() +
  labs(title = 'Histogram of average daily number of excursions', x = 'Average daily number of excursions')

amp_hist = ggplot(data = excursion_df_metrics, aes(x = amplitude)) +
  geom_histogram(fill = 'gray', color = 'black') +
  theme_bw() +
  labs(title = 'Histogram of all excursion amplitudes', x = "Amplitude (bpm)")

dur_hist = ggplot(data = excursion_df_metrics, aes(x = time_length)) +
  geom_histogram(fill = 'gray', color = 'black') +
  theme_bw() +
  labs(title = 'Histogram of all excursion durations', x = 'Duration (min)')

supp_fig_6 = grid.arrange(freq_hist, amp_hist, dur_hist)
ggsave("figures/supp_fig6_excursion_histograms.png", supp_fig_6, bg = "white")


#### Supplementary Figure 7: waking-hour excursions by time of day and age ####

# Per participant-day and window: number of excursions, mean amplitude, mean duration
per_period_per_day = exc_age %>%
  filter(time_period != "Night (22:00-4:59)") %>%
  group_by(id, age, age_cat, start_day, time_period) %>%
  summarize(n_exc = n(),
            mean_amp = mean(amplitude, na.rm = TRUE),
            mean_dur = mean(time_length, na.rm = TRUE),
            .groups = 'drop')

period_fill = scale_fill_manual(name = "Time of day",
                                values = c("Morning (5:00-9:59)" = "#FDB462",
                                           "Midday (10:00-16:59)" = "#80B1D3",
                                           "Evening (17:00-21:59)" = "#FB8072"))

period_boxplot = function(y, title, y_lab){
  ggplot(data = per_period_per_day, aes(x = age_cat, y = .data[[y]], fill = time_period)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    period_fill +
    labs(title = title, x = "Age category", y = y_lab) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, hjust = 0.5),
          legend.position = "right")
}

p_count = period_boxplot("n_exc", "Number of Excursions", "Number of excursions per period per day")
p_amp = period_boxplot("mean_amp", "Average Excursion Amplitude", "Mean amplitude per period per day (bpm)")
p_dur = period_boxplot("mean_dur", "Average Excursion Duration", "Mean duration per period per day (minutes)")

supp_fig_7 = (p_count + p_amp + p_dur) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Excursion characteristics by age",
    subtitle = "Boxplots group by time of day",
    theme    = theme(
      plot.title    = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  ) &
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal"
  )

ggsave("figures/supp_fig7_excursions_time_of_day.png", supp_fig_7, width = 12, height = 7, bg = "white")


#### Numbers reported in the Results ####

cat("Total excursions:", nrow(exc_age), "\n")
cat("Share starting in waking hours (5:00-21:59):",
    round(mean(exc_age$time_period != "Night (22:00-4:59)"), 3), "\n")

per_period_per_day %>%
  group_by(age_cat, time_period) %>%
  summarize(mean_n_exc = mean(n_exc),
            median_amp = median(mean_amp),
            median_dur = median(mean_dur),
            .groups = 'drop') %>%
  print(n = Inf)
