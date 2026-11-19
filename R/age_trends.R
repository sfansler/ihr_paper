library(ihr)
library(tidyverse)
library(lemon)

# Figure 3 (age trends in HRR, RHR, MAHE and excursion duration) and the
# excursion panel of Figure 5. Requires data/processed/mahe.rds and mahe_exc.rds
# from R/get_mahe.R.

data = readRDS("data/processed/data_clean.rds") %>%
  rename(time = timestamp)

mahe_df = readRDS("data/processed/mahe.rds")

mahe_exc_df = unnest(readRDS("data/processed/mahe_exc.rds"), cols = c(MAHE))

excursion_df_metrics = mahe_exc_df %>%
  na.omit() %>%
  mutate(start_time = as_datetime(start_time),
         peak_time = as_datetime(peak_time),
         end_time = as_datetime(end_time),
         amplitude = plus)

subj_exc_metrics = excursion_df_metrics %>%
  group_by(id) %>%
  summarize(amp = mean(amplitude),
            duration = as.numeric(mean(time_length)))

# Times are stored as UTC clock time; tz = "UTC" keeps the RHR window at 3:00-7:00 AM
# (the default uses the computer's local time zone)
hrr_df = calculate_HRR(data, tz = "UTC") %>%
  dplyr::select(id, HRR, RHR)

demo = data %>%
  dplyr::select(id, sex, age) %>%
  distinct()

plot_data = merge(hrr_df,
                  merge(mahe_df,
                        merge(subj_exc_metrics,
                              demo,
                              by = "id"))) %>%
  na.omit()


#### Figure 3 ####

plot_theme = theme_bw(base_size = 16)

theme_legend = theme(legend.title = element_text(size = 20),
                     legend.text = element_text(size = 18))

mahe_plot = ggplot(data = plot_data, aes(x = age, y = MAHE, color = sex)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(ylim = c(15, 55)) +
  plot_theme +
  geom_smooth() +
  scale_color_discrete(name = "Sex") +
  labs(x = "Age", y = "MAHE (beats per minute)", title = "MAHE vs age") +
  theme_legend

duration_plot = ggplot(data = plot_data, aes(x = age, y = duration, color = sex)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(ylim = c(60, 140)) +
  plot_theme +
  geom_smooth() +
  scale_color_discrete(name = "Sex") +
  labs(x = "Age", y = "Average excursion duration (min)", title = "Average excursion duration vs age") +
  theme_legend

hrr_plot = ggplot(data = plot_data, aes(x = age, y = HRR, color = sex)) +
  geom_point(alpha = 0.3) +
  plot_theme +
  geom_smooth() +
  scale_color_discrete(name = "Sex") +
  labs(x = "Age", y = "HRR (beats per minute)", title = "HRR vs age") +
  theme_legend

rhr_plot = ggplot(data = plot_data, aes(x = age, y = RHR, color = sex)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(ylim = c(40, 90)) +
  plot_theme +
  geom_smooth() +
  scale_color_discrete(name = "Sex") +
  labs(x = "Age", y = "RHR (beats per minute)", title = "RHR vs age") +
  theme_legend

fig3 = grid_arrange_shared_legend(hrr_plot, rhr_plot, mahe_plot, duration_plot, nrow = 2, ncol = 2)

ggsave("figures/fig3_age_trends.png", fig3, width = 10, height = 12, bg = "white")


#### Fitted MAHE and duration at ages 40 and 80 (females) ####

fitted_lines_mahe = ggplot_build(mahe_plot)$data[[2]] %>%
  filter(group == 1) # Female

fitted_lines_duration = ggplot_build(duration_plot)$data[[2]] %>%
  filter(group == 1) # Female

mahe_40 = fitted_lines_mahe$y[round(fitted_lines_mahe$x) == 40]
duration_40 = fitted_lines_duration$y[round(fitted_lines_duration$x) == 40]

mahe_80 = fitted_lines_mahe$y[round(fitted_lines_mahe$x) == 80]
duration_80 = fitted_lines_duration$y[round(fitted_lines_duration$x) == 80]

print(c(mahe_40 = mahe_40, duration_40 = duration_40, mahe_80 = mahe_80, duration_80 = duration_80))


#### Figure 5 bottom panel: illustrative excursions at ages 40 and 80 ####
# A parabola through (start, 0), (peak, MAHE) and (end, 0). The peak and end
# values below are the fitted values printed above, fixed for the manuscript.

sim_fun = function(t_start, t_peak, t_end, y_start, y_peak, y_end) {
  A <- matrix(c(t_start^2, t_start, 1,
                t_peak^2,  t_peak,  1,
                t_end^2,   t_end,   1), ncol = 3, byrow = TRUE)
  b <- c(y_start, y_peak, y_end)
  coef <- solve(A, b)

  t_grid <- seq(t_start, t_end, length.out = 60)
  y_grid <- coef[1]*t_grid^2 + coef[2]*t_grid + coef[3]

  return(data.frame(time = t_grid, hr = y_grid))
}

sim_exc_40 = sim_fun(t_start = 1, t_peak = 46.85508, t_end = 93.71017,
                     y_start = 0, y_peak = 40.7806, y_end = 0)

sim_exc_80 = sim_fun(t_start = 1, t_peak = 52.58572, t_end = 105.17144,
                     y_start = 0, y_peak = 31.99051, y_end = 0)

sim_exc_40$age = "40-year-old"
sim_exc_80$age = "80-year-old"

sim_data = rbind(sim_exc_40, sim_exc_80)

exc_comparison = ggplot(data = sim_data, aes(x = time, y = hr, color = age)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = c("#2D8CF0", "#5C0029")) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "black", linewidth = 0.15),
        panel.grid.minor = element_line(color = "black", linewidth = 0.15)
        ) +
  labs(x = "Time (min from excursion start)", y = "HR (beats per minute)")

# Combined with the dHRR envelopes into Figure 5 by R/fosr.R
save(exc_comparison, file = "data/intermediate/exc_comparison.Rdata")
