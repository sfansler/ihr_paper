library(dplyr)
library(ihr)
library(ggplot2)
library(slider)

# Figure 2 and Supplementary Figure 1: example BLSA participant, seven days of
# wear starting 2009-06-25

# Times are stored as UTC clock time. ahp() does not pass its tz argument on to
# the activity-distribution panel, so set the session time zone as well.
Sys.setenv(TZ = "UTC")

one_sub = "1856_3"

data_sub = readRDS("data/processed/data_clean.rds") %>%
  rename(time = timestamp) %>%
  filter(id == one_sub)

data = data_sub %>%
  filter(time > "2009-06-25 00:00:00")

# 15-minute centered moving average, used only for the excursion panel
data_smoothed = data_sub %>%
  mutate(hr = slide_dbl(hr, mean, .before = 7, .after = 7)) %>%
  filter(time >= "2009-06-25 00:00:00")


#### Top panel: raw HR by day and diurnal quantile curves ####

# tz = "UTC": times are stored as UTC clock time, so RHR (3:00-7:00 AM) and the
# activity thresholds are computed on the right window
ahp_full = ahp(data, tz = "UTC")

ahp_summary = ahp_full[[1]]
ahp_curve = ahp_full[[2]]
ahp_raw = ahp_full[[3]]

# Supplementary Figure 1: the full, unmasked profile
supp_fig1 = patchwork::wrap_plots(ahp_raw, ahp_curve, ahp_summary, ncol = 1)
ggsave("figures/supp_fig1_ahp.png", supp_fig1, width = 12, height = 15, bg = "white")

# Replace calendar dates with "Day 1" ... "Day 7"
plot_objects_raw = ggplot_build(ahp_raw)
plot_objects_raw$layout$layout$each_day = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7")
ahp_raw_masked = ggplotify::as.ggplot(ggplot_gtable(plot_objects_raw))

fig2_top = gridExtra::grid.arrange(ahp_raw_masked, ahp_curve)
ggsave("figures/fig2_top_ahp.png", fig2_top, height = 10, width = 13, bg = "white")


#### Bottom panel: excursions on the first day ####
# The shading, arrows and "Excursion 1-3" labels in the manuscript were added by hand.

one_day_smoothed = data_smoothed %>%
  filter(date < "2009-06-26")

fig2_bottom = mahe(one_day_smoothed, plot = T, tz = "UTC", show_excursions = F)
ggsave("figures/fig2_bottom_excursions.png", fig2_bottom, width = 10, bg = "white")
