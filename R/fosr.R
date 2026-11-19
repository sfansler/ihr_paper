library(tidyverse)
library(tidyfun)
library(ihr)
library(hms)
library(lemon)

# Function-on-scalar regression of dHR (median curve) and dHRR (95th - 5th
# percentile curve) on age, BMI and sex.
# Produces Figures 4 and 5 and Supplementary Figures 2-5.
# Requires data/intermediate/exc_comparison.Rdata from R/age_trends.R (Figure 5).

x_scale_paper = scale_x_continuous(
  breaks = seq(0, 1440, by = 360),
  labels = c("12:00am", "6:00am", "12:00pm", "6:00pm", "12:00am")
)

y_scale_median = scale_y_continuous(
  limits = c(59, 87),
  breaks = seq(60, 85, by = 5)
)

y_scale_95_5 = scale_y_continuous(
  limits = c(11.5, 35.5),
  breaks = seq(15, 35, by = 5)
)

base_size_poster = 40


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

length(unique(data_valid$id))


#### Ambulatory HR profiles and diurnal quantile curves ####

ahps = list()
for(i in 1:length(valid_subjects)){
  one_subject = data[data$id == valid_subjects[i],]
  ahps[[i]] = plot_ahp(one_subject)
  print(i)
}

df_hr_median = df_hr_95 = df_hr_5 = data.frame()

for(i in 1:length(valid_subjects)){
  df_hr_median = rbind(df_hr_median, ahps[[i]]$data$median)
  df_hr_5 = rbind(df_hr_5, ahps[[i]]$data$five)
  df_hr_95 = rbind(df_hr_95, ahps[[i]]$data$ninetyfive)
}

# Subject covariates, in the same order as valid_subjects
covariates = data.frame(
  age = data_valid %>%
    select(id, age) %>%
    group_by(id) %>%
    mutate(age = min(age)) %>%
    distinct() %>%
    ungroup() %>%
    select(age) %>%
    unlist(),
  sex = data_valid %>%
    select(id, sex) %>%
    group_by(id) %>%
    distinct() %>%
    ungroup() %>%
    select(sex) %>%
    unlist(),
  bmi = data_valid %>%
    select(id, bmi) %>%
    group_by(id) %>%
    distinct() %>%
    ungroup() %>%
    select(bmi) %>%
    unlist()
)

# Curves as tfd objects with centered age and BMI
make_tfd = function(curve_mat){
  data.frame(id = valid_subjects, covariates) %>%
    mutate(age_cen = age - mean(age),
           bmi_cen = bmi - mean(bmi)) %>%
    mutate(hr_tf = tfd(curve_mat, arg = 1:1440))
}

# Pointwise (minute-by-minute) regressions with 95% confidence bands
pointwise_coefs = function(df_tfd){
  df_tfd %>%
    select(id, age_cen, sex, bmi_cen, hr_tf) %>%
    tf_unnest(hr_tf) %>%
    rename(minute = hr_tf_arg, hr = hr_tf_value) %>%
    nest(data = -minute) %>%
    mutate(
      model = map(.x = data, ~ lm(hr ~ age_cen + sex + bmi_cen, data = .x)),
      result = map(model, broom::tidy)
    ) %>%
    select(minute, result) %>%
    unnest(cols = result) %>%
    rename(coef = estimate, se = std.error) %>%
    mutate(
      ub = coef + 1.96 * se,
      lb = coef - 1.96 * se
    ) %>%
    select(minute, term, coef, ub, lb) %>%
    tf_nest(coef:lb, .id = term, .arg = minute) %>%
    mutate(method = "Pointwise",
           term = c("(Intercept)", "Age", "BMI", "Sex (Male)"))
}

plot_pointwise = function(coefs){
  coefs %>%
    ggplot(aes(y = coef)) +
    geom_spaghetti() +
    geom_meatballs() +
    geom_errorband(aes(ymax = ub, ymin = lb, fill = term)) +
    x_scale_paper +
    facet_wrap("term", scales = "free")
}

# Smooth coefficient functions from a GAM; used for all fitted curves
gam_coefs = function(df_tfd){
  df_gam = df_tfd %>%
    mutate(sex = as.numeric(sex == "Male")) %>%
    tf_unnest(hr_tf) %>%
    rename(epoch = hr_tf_arg, hr = hr_tf_value)

  gam_fit = mgcv::gam(hr ~ s(epoch) + s(epoch, by = age_cen) + s(epoch, by = bmi_cen) + s(epoch, by = sex),
                      data = df_gam)

  gam_pred_obj = tibble(epoch = 1:1440, age_cen = 1, bmi_cen = 1, sex = 1) %>%
    predict(gam_fit, newdata = ., type = "terms")

  tibble(
    method = "GAM",
    term = c("(Intercept)", "age", "bmi", "sex"),
    coef = tfd(t(gam_pred_obj), arg = 1:1440)) %>%
    mutate(coef = coef + c(coef(gam_fit)[1], 0, 0, 0))
}

# Fitted curve for a female with the given age and BMI, or at mean age/BMI by sex
pred_curve = function(gam_coef_df, age = mean_age, bmi = mean_bmi, sex = 0){
  pred = gam_coef_df[1,3] + gam_coef_df[2,3]*(age - mean_age) +
    gam_coef_df[3,3]*(bmi - mean_bmi) + gam_coef_df[4,3]*sex
  colnames(pred) = "hr"
  unnest(data.frame(pred), cols = c(hr)) %>% mutate(time = 1:1440)
}

curve_theme = function(){
  list(theme_bw(base_size = base_size_poster),
       x_scale_paper,
       theme(title = element_text(face = "bold"),
             axis.text = element_text(face = "bold")))
}


#### dHR (median curve) ####

df_hr_median_tfd = make_tfd(data.matrix(df_hr_median[1:1440]))

mean_age = mean(df_hr_median_tfd$age)
mean_bmi = mean(df_hr_median_tfd$bmi)

# Supplementary Figure 4
fosr_median = plot_pointwise(pointwise_coefs(df_hr_median_tfd))
ggsave("figures/supp_fig4_dhr_coefficients.png", fosr_median, width = 8, bg = "white")

gam_coef_df = gam_coefs(df_hr_median_tfd)

pred_40_med = pred_curve(gam_coef_df, age = 40) %>% mutate(Age = "40")
pred_80_med = pred_curve(gam_coef_df, age = 80) %>% mutate(Age = "80")

age_curves_median = ggplot(mapping = aes(x = time, y = hr, color = Age)) +
  curve_theme() +
  geom_line(data = pred_40_med, linewidth = 1.2) +
  geom_line(data = pred_80_med, linewidth = 1.2) +
  y_scale_median +
  scale_color_manual(values = c("#2D8CF0", "#5C0029")) +
  labs(title = "dHR by age", subtitle = "For Females with average BMI (27)", x = "Time", y = "dHR")

# Supplementary Figure 2: age 40 minus age 80
pred_40_minus_80 = data.frame(hr = pred_40_med$hr - pred_80_med$hr, time = 1:1440)

supp_fig_age_diff = ggplot(data = pred_40_minus_80, aes(x = time, y = hr)) +
  geom_line(linewidth = 2) +
  x_scale_paper +
  labs(title = "dHR difference between 40 and 80 years old", subtitle = "For Females with average BMI (27)", x = "Time", y = "dHR difference") +
  theme_bw(base_size = 14)

ggsave("figures/supp_fig2_dhr_difference.png", supp_fig_age_diff, bg = "white")

bmi_curves_median = ggplot(mapping = aes(x = time, y = hr, color = bmi)) +
  curve_theme() +
  map(c(20, 25, 30, 35, 40), ~ geom_line(data = pred_curve(gam_coef_df, bmi = .x) %>% mutate(bmi = as.character(.x)), linewidth = 1.2)) +
  y_scale_median +
  scale_color_discrete(name = "BMI") +
  labs(title = "dHR by BMI", subtitle = "For Females with average age (66)", y = "dHR")

sex_curves_median = ggplot(mapping = aes(x = time, y = hr, color = sex)) +
  curve_theme() +
  geom_line(data = pred_curve(gam_coef_df, sex = 1) %>% mutate(sex = "Male"), linewidth = 1.2) +
  geom_line(data = pred_curve(gam_coef_df, sex = 0) %>% mutate(sex = "Female"), linewidth = 1.2) +
  y_scale_median +
  scale_color_discrete(name = "Sex") +
  labs(title = "dHR by sex", subtitle = "At average age (66) with average BMI (27)", y = "dHR")


#### dHRR (95th - 5th percentile curve) ####

df_hr_5_95_tfd = make_tfd(data.matrix(df_hr_95[1:1440]) - data.matrix(df_hr_5[1:1440]))

# Supplementary Figure 5
fosr_5_95 = plot_pointwise(pointwise_coefs(df_hr_5_95_tfd))
ggsave("figures/supp_fig5_dhrr_coefficients.png", fosr_5_95, width = 8, bg = "white")

gam_coef_df_5_95 = gam_coefs(df_hr_5_95_tfd)

pred_40_dhrr = pred_curve(gam_coef_df_5_95, age = 40) %>% mutate(Age = "40")
pred_80_dhrr = pred_curve(gam_coef_df_5_95, age = 80) %>% mutate(Age = "80")

age_curves_95_5 = ggplot(mapping = aes(x = time, y = hr, color = Age)) +
  curve_theme() +
  geom_line(data = pred_40_dhrr, linewidth = 1.2) +
  geom_line(data = pred_80_dhrr, linewidth = 1.2) +
  y_scale_95_5 +
  scale_color_manual(values = c("#2D8CF0", "#5C0029")) +
  labs(title = "dHRR by age", subtitle = "For Females with average BMI (27)", y = "dHRR")

bmi_curves_95_5 = ggplot(mapping = aes(x = time, y = hr, color = bmi)) +
  curve_theme() +
  map(c(20, 25, 30, 35, 40), ~ geom_line(data = pred_curve(gam_coef_df_5_95, bmi = .x) %>% mutate(bmi = as.character(.x)), linewidth = 1.2)) +
  y_scale_95_5 +
  scale_color_discrete(name = "BMI") +
  labs(title = "dHRR by BMI", subtitle = "For Females with average age (66)", y = "dHRR")

sex_curves_95_5 = ggplot(mapping = aes(x = time, y = hr, color = sex)) +
  curve_theme() +
  geom_line(data = pred_curve(gam_coef_df_5_95, sex = 1) %>% mutate(sex = "Male"), linewidth = 1.2) +
  geom_line(data = pred_curve(gam_coef_df_5_95, sex = 0) %>% mutate(sex = "Female"), linewidth = 1.2) +
  y_scale_95_5 +
  scale_color_discrete(name = "Sex") +
  labs(title = "dHRR by sex", subtitle = "At average age (66) with average BMI (27)", y = "dHRR")


#### Supplementary Figure 3: fitted curves by BMI ####

supp_fig3 = grid_arrange_shared_legend(bmi_curves_median, bmi_curves_95_5, nrow = 1)
ggsave("figures/supp_fig3_bmi_curves.png", supp_fig3, width = 20, height = 9, bg = "white")


#### Figure 4: fitted dHR and dHRR by age and sex ####

figure_theme = theme(
  plot.title = element_text(size = 35, face = "bold"),
  plot.subtitle = element_text(size = 28, face = "bold"),
  axis.title = element_text(size = 28, face = "bold"),
  axis.text = element_text(size = 20),
  legend.text = element_text(size = 28),
  legend.title = element_text(size = 28, face = "bold"),
  legend.key.width = unit(3, "cm")
)

fig4_labs = function(p, y) p + figure_theme + labs(x = "Time of day", y = y)

top = grid_arrange_shared_legend(fig4_labs(age_curves_median, "dHR (beats per minute)"),
                                 fig4_labs(age_curves_95_5, "dHRR (beats per minute)"),
                                 position = "bottom")
bot = grid_arrange_shared_legend(fig4_labs(sex_curves_median, "dHR (beats per minute)"),
                                 fig4_labs(sex_curves_95_5, "dHRR (beats per minute)"),
                                 position = "bottom")

fig4 = gridExtra::grid.arrange(top, bot, nrow = 2)
ggsave("figures/fig4_dhr_dhrr_curves.png", fig4, height = 17, width = 20, bg = "white")


#### Figure 5: diurnal HR envelopes and excursions at ages 40 and 80 ####

pred_40_med$upper = pred_40_med$hr + 0.5* pred_40_dhrr$hr
pred_40_med$lower = pred_40_med$hr - 0.5* pred_40_dhrr$hr
pred_40_med$age = "40-year-old"

pred_80_med$upper = pred_80_med$hr + 0.5* pred_80_dhrr$hr
pred_80_med$lower = pred_80_med$hr - 0.5* pred_80_dhrr$hr
pred_80_med$age = "80-year-old"

pred_med = rbind(pred_40_med, pred_80_med)

fitted_curves_40_80 = ggplot(mapping = aes(x = time, y = hr)) +
  geom_line(data = pred_med, aes(color = age, y = upper), linetype = "longdash") +
  geom_line(data = pred_med, aes(color = age, y = lower), linetype = "longdash") +
  geom_ribbon(data = pred_med, aes(fill = age, ymin = lower, ymax = upper), alpha = 0.4) +
  x_scale_paper +
  scale_color_manual(name = "Age", values = c("#2D8CF0", "#5C0029")) +
  scale_fill_manual(name = "Age", values = c("#2D8CF0", "#5C0029")) +
  labs(x = "Time of day",
       y = "HR (beats per minute)") +
  theme_bw(base_size = 17)

load("data/intermediate/exc_comparison.Rdata")

fig5 = grid_arrange_shared_legend(fitted_curves_40_80,
                                  exc_comparison, ncol = 1, nrow = 2, position = "right")

ggsave("figures/fig5_age_40_vs_80.png", fig5, height = 8, width = 10, bg = "white")
