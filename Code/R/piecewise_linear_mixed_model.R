### Piecewise linear mixed models ###

# - Piecewise linear mixed model with 2 time segments
#   (Time_1, Time_2) replacing time variable
# - Breakpoint assumed to be at time 100
# - Response: dopamine (DA), serotonin (HT_5)
# - Response log transformed in order to satisfy normality assumption (for DA and HT_5)
# - Baseline included as covariate in fixed effects
# - Fixed effects: dose, Time_1, Time_2, dose:Time_1, dose:Time_2, 
#   baseline (DA or HT_5, log transformed)
# - Produces: QQ plots, residual plots, plots of observed vs fitted values,
#   plots of fitted values per unique_id, t-table of fixed effects

#####################################

library(tidyverse)
library(nlme)
library(ggpubr)
library(xtable)

# Theme for plots
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                               plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=13))

# Read the tidy data set from disk 
path_data <- "../../Intermediate/Data_tidy.csv"
data_tidy <- read_csv(path_data, col_types = cols(
  id = col_factor(), 
  position = col_factor(), 
  sample = col_integer(), 
  Type = col_factor(), 
  time = col_double(), 
  NOA = col_double(), 
  DA = col_double(), 
  MT_3 = col_double(), 
  NM = col_double(), 
  HT_5 = col_double(), 
  DOPAC = col_double(), 
  HIAA = col_double(), 
  HVA = col_double(), 
  unique_id = col_factor(), 
  dose = col_factor(), 
  baseline_NOA = col_double(), 
  baseline_DA = col_double(), 
  baseline_HT_5 = col_double(),
  b_NOA = col_double(), 
  b_DA = col_double(), 
  b_HT_5 = col_double()))

# ================================================================================================
# Create analysis datasets
# ================================================================================================

data <- data_tidy

# Refactor dose
dose_ordered <- fct_relevel(data$dose, "NaCl", after = 0) %>%
  fct_relevel("CC_5.6_mumol/kg", after = 1) %>%
  fct_relevel("CC_16.7_mumol/kg", after = 2) %>%
  fct_relevel("CC_50.0_mumol/kg", after = 3) %>%
  fct_relevel("CC_150.0_mumol/kg", after = 4)
data$dose <- dose_ordered

data <- data %>%
  mutate(dose = fct_relabel(dose, str_replace, "NaCl", "Control")) %>%
  mutate(dose = fct_relabel(dose, str_replace, "CC_5.6_mumol/kg", " Dose 5.6")) %>%
  mutate(dose = fct_relabel(dose, str_replace, "CC_16.7_mumol/kg", "Dose 16.7")) %>%
  mutate(dose = fct_relabel(dose, str_replace, "CC_50.0_mumol/kg", "Dose 50")) %>%
  mutate(dose = fct_relabel(dose, str_replace, "CC_150.0_mumol/kg", "Dose 150"))


# Time vectors for piecewise linear mixed model
# Naive assumption: breakpoint at time 100
data <- data %>%
  mutate(Time_1 = case_when(time == 20 ~ -4,
                            time == 40 ~ -3,
                            time == 60 ~ -2,
                            time == 80 ~ -1,
                            TRUE ~ 0)) %>%
  mutate(Time_2 = case_when(time == 120 ~ 1,
                            time == 140 ~ 2,
                            time == 160 ~ 3,
                            time == 180 ~ 4,
                            TRUE ~ 0))

# Dopamine & cortex
data_da_c <- data %>%
  select(unique_id, position, time, dose, DA, baseline_DA, Time_1, Time_2) %>%
  filter(position=='Cortex') %>%
  filter(time > 0) %>%
  mutate(log_DA=log(DA), log_baseline_DA=log(baseline_DA)) %>% # log transformed response
  select(unique_id,time,dose,DA,log_DA,baseline_DA,log_baseline_DA, Time_1, Time_2) %>%
  filter(!is.na(log_DA))
dim(data_da_c)

# Dopamine & striatum
data_da_s <- data %>%
  select(unique_id, position, time, dose, DA, baseline_DA, Time_1, Time_2) %>%
  filter(position=='Striatum') %>%
  filter(time > 0) %>%
  mutate(log_DA=log(DA), log_baseline_DA=log(baseline_DA)) %>% # log transformed response
  select(unique_id,time,dose,DA,log_DA,baseline_DA,log_baseline_DA, Time_1, Time_2) %>%
  filter(!is.na(log_DA))
dim(data_da_s)

# Serotonin & cortex
data_ht_5_c <- data %>%
  select(unique_id, position, time, dose, HT_5, baseline_HT_5, Time_1, Time_2) %>%
  filter(position=='Cortex') %>%
  filter(time > 0) %>%
  mutate(log_HT_5=log(HT_5), log_baseline_HT_5=log(baseline_HT_5)) %>% # log transformed response
  select(unique_id,time,dose,HT_5,log_HT_5,baseline_HT_5,log_baseline_HT_5, Time_1, Time_2) %>%
  filter(!is.na(log_HT_5))
dim(data_ht_5_c)

# Serotonin & cortex
data_ht_5_s <- data %>%
  select(unique_id, position, time, dose, HT_5, baseline_HT_5, Time_1, Time_2) %>%
  filter(position=='Striatum') %>%
  filter(time > 0) %>%
  mutate(log_HT_5=log(HT_5), log_baseline_HT_5=log(baseline_HT_5)) %>% # log transformed response
  select(unique_id,time,dose,HT_5,log_HT_5,baseline_HT_5,log_baseline_HT_5, Time_1, Time_2) %>%
  filter(!is.na(log_HT_5))
dim(data_ht_5_s)

# ================================================================================================
# Piecewise linear mixed models
# ================================================================================================

ctrl <- lmeControl(opt='optim');

lme_da_c <- lme(log_DA ~ (Time_1 + Time_2)*dose +
                  log_baseline_DA,
                random = ~ Time_1 + Time_2|unique_id,
                data=data_da_c, method="REML", control=ctrl)

lme_da_s <- lme(log_DA ~ (Time_1 + Time_2)*dose +
                  log_baseline_DA,
                random = ~ Time_1 + Time_2|unique_id,
                data=data_da_s, method="REML", control=ctrl)

lme_ht_5_c <- lme(log_HT_5 ~ (Time_1 + Time_2)*dose +
                    log_baseline_HT_5,
                  random = ~ Time_1 + Time_2|unique_id,
                  data=data_ht_5_c, method="REML", control=ctrl)

lme_ht_5_s <- lme(log_HT_5 ~ (Time_1 + Time_2)*dose +
                    log_baseline_HT_5,
                  random = ~ Time_1 + Time_2|unique_id,
                  data=data_ht_5_s, method="REML", control=ctrl)

# ================================================================================================
# Extract standardized residuals
# ================================================================================================

std_residuals <- residuals(lme_da_c, type="pearson") # standardized residuals
data_da_c$std_res <- std_residuals

std_residuals <- residuals(lme_da_s, type="pearson") # standardized residuals
data_da_s$std_res <- std_residuals

std_residuals <- residuals(lme_ht_5_c, type="pearson") # standardized residuals
data_ht_5_c$std_res <- std_residuals

std_residuals <- residuals(lme_ht_5_s, type="pearson") # standardized residuals
data_ht_5_s$std_res <- std_residuals

# ================================================================================================
# Extract fitted values
# ================================================================================================

fitted_values <- fitted(lme_da_c)
data_da_c$fitted_val <- fitted_values

fitted_values <- fitted(lme_da_s)
data_da_s$fitted_val <- fitted_values

fitted_values <- fitted(lme_ht_5_c)
data_ht_5_c$fitted_val <- fitted_values

fitted_values <- fitted(lme_ht_5_s)
data_ht_5_s$fitted_val <- fitted_values

# ================================================================================================
# QQ plots
# ================================================================================================

p1 <- ggplot(data_da_c, aes(sample=std_res)) + 
  stat_qq(size=0.5) + 
  stat_qq_line() + 
  my_theme + 
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")

path_save1 <- "../../Result/piecewise_qq_da_c.pdf"
ggsave(filename = path_save1, plot = p1, height = 6, width = 9)

p2 <- ggplot(data_da_s, aes(sample=std_res)) + 
  stat_qq(size=0.5) + 
  stat_qq_line() + 
  my_theme + 
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")

path_save2 <- "../../Result/piecewise_qq_da_s.pdf"
ggsave(filename = path_save2, plot = p2, height = 6, width = 9)

p3 <- ggplot(data_ht_5_c, aes(sample=std_res)) + 
  stat_qq(size=0.5) + 
  stat_qq_line() + 
  my_theme + 
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")

path_save3 <- "../../Result/piecewise_qq_ht_5_c.pdf"
ggsave(filename = path_save3, plot = p3, height = 6, width = 9)

p4 <- ggplot(data_ht_5_s, aes(sample=std_res)) + 
  stat_qq(size=0.5) + 
  stat_qq_line() + 
  my_theme + 
  labs(x = "Theoretical quantiles",
       y = "Sample quantiles")

path_save4 <- "../../Result/piecewise_qq_ht_5_s.pdf"
ggsave(filename = path_save4, plot = p4, height = 6, width = 9)

# ================================================================================================
# Residual plots
# ================================================================================================

p1 <- ggplot(data_da_c, aes(x=fitted_val)) +
  geom_point(aes(y=std_res), size=0.5) + 
  my_theme + 
  labs(title = "Dopamine, cortex",
       x = "Fitted values (log scale)",
       y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = 1)

path_save1 <- "../../Result/piecewise_residuals_da_c.pdf"
ggsave(filename = path_save1, plot = p1, height = 6, width = 9)

p2 <- ggplot(data_da_s, aes(x=fitted_val)) +
  geom_point(aes(y=std_res), size=0.5) + 
  my_theme +
  labs(title="Dopamine, striatum",
       x = "Fitted values (log scale)",
       y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = 1)

path_save2 <- "../../Result/piecewise_residuals_da_s.pdf"
ggsave(filename = path_save2, plot = p2, height = 6, width = 9)

p3 <- ggplot(data_ht_5_c, aes(x=fitted_val)) +
  geom_point(aes(y=std_res), size=0.5) + 
  my_theme +
  labs(title="Serotonin, cortex",
       x = "Fitted values (log scale)",
       y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = 1)

path_save3 <- "../../Result/piecewise_residuals_ht_5_c.pdf"
ggsave(filename = path_save3, plot = p3, height = 6, width = 9)

p4 <- ggplot(data_ht_5_s, aes(x=fitted_val)) +
  geom_point(aes(y=std_res), size=0.5) + 
  my_theme +
  labs(title="Serotonin, striatum",
       x = "Fitted values (log scale)",
       y = "Standardized residuals") +
  geom_hline(yintercept = 0, linetype = 1)

path_save4 <- "../../Result/piecewise_residuals_ht_5_s.pdf"
ggsave(filename = path_save4, plot = p4, height = 6, width = 9)

# ================================================================================================
# Plots of log observed vs. fitted values
# ================================================================================================

p1 <- ggplot(data_da_c, aes(x=fitted_val)) +
  geom_point(aes(y=log_DA), size=0.5) + 
  my_theme + 
  labs(title = "Dopamine, cortex",
       x = "Fitted values (log scale)",
       y = "Observed values (log scale)") +
  geom_abline(slope = 1, intercept = 0, linetype = 1)

path_save1 <- "../../Result/piecewise_obs_vs_fitted_da_c.pdf"
ggsave(filename = path_save1, plot = p1, height = 6, width = 9)

p2 <- ggplot(data_da_s, aes(x=fitted_val)) +
  geom_point(aes(y=log_DA), size=0.5) + 
  my_theme +
  labs(title = "Dopamine, striatum",
       x = "Fitted values (log scale)",
       y = "Observed values (log scale)") +
  geom_abline(slope = 1, intercept = 0, linetype = 1)

path_save2 <- "../../Result/piecewise_obs_vs_fitted_da_s.pdf"
ggsave(filename = path_save2, plot = p2, height = 6, width = 9)

p3 <- ggplot(data_ht_5_c, aes(x=fitted_val)) +
  geom_point(aes(y=log_HT_5), size=0.5) + 
  my_theme +
  labs(title = "Serotonin, cortex",
       x = "Fitted values (log scale)",
       y = "Observed values (log scale)") +
  geom_abline(slope = 1, intercept = 0, linetype = 1)

path_save3 <- "../../Result/piecewise_obs_vs_fitted_ht_5_c.pdf"
ggsave(filename = path_save3, plot = p3, height = 6, width = 9)

p4 <- ggplot(data_ht_5_s, aes(x=fitted_val)) +
  geom_point(aes(y=log_HT_5), size=0.5) + 
  my_theme +
  labs(title = "Serotonin, striatum",
       x = "Fitted values (log scale)",
       y = "Observed values (log scale)") +
  geom_abline(slope = 1, intercept = 0, linetype = 1)

path_save4 <- "../../Result/piecewise_obs_vs_fitted_ht_5_s.pdf"
ggsave(filename = path_save4, plot = p4, height = 6, width = 9)

# ================================================================================================
# Plots of fitted values per unique_id
# ================================================================================================

# Dopamine & cortex
p1 <- ggplot(data_da_c, aes(x=time)) +
  geom_point(aes(y=log_DA)) + 
  geom_line(aes(y=fitted_val)) +
  facet_wrap(~ dose + unique_id, ncol=7) +
  theme(strip.background=element_rect(fill="white"))+
  my_theme + 
  labs(title = "Observed and fitted dopamine concentration in cortex by subject",
       x = "Time (min)", y = "Dopamine concentration (log scale)")

path_save1 <- "../../Result/piecewise_by_subject_da_c.pdf"
ggsave(filename = path_save1, plot = p1, height = 12, width = 12)

# Dopamine & striatum
p2 <- ggplot(data_da_s, aes(x=time)) +
  geom_point(aes(y=log_DA)) + 
  geom_line(aes(y=fitted_val)) +
  facet_wrap(~ dose + unique_id, ncol=7) +
  theme(strip.background =element_rect(fill="white"))+
  my_theme + 
  labs(title = "Observed and fitted dopamine concentration in striatum by subject",
       x = "Time (min)", y = "Dopamine concentration (log scale)")

path_save2 <- "../../Result/piecewise_by_subject_da_s.pdf"
ggsave(filename = path_save2, plot = p2, height = 12, width = 12)

# Serotonin & cortex
p3 <- ggplot(data_ht_5_c, aes(x=time)) +
  geom_point(aes(y=log_HT_5)) + 
  geom_line(aes(y=fitted_val)) +
  facet_wrap(~ dose + unique_id, ncol=7) +
  theme(strip.background =element_rect(fill="white"))+
  my_theme + 
  labs(title = "Observed and fitted serotonin concentration in cortex by subject",
       x = "Time (min)", y = "Serotonin concentration (log scale)")

path_save3 <- "../../Result/piecewise_by_subject_ht_5_c.pdf"
ggsave(filename = path_save3, plot = p3, height = 12, width = 12)

# Serotonin & striatum
p4 <- ggplot(data_ht_5_s, aes(x=time)) +
  geom_point(aes(y=log_HT_5)) + 
  geom_line(aes(y=fitted_val)) +
  facet_wrap(~ dose + unique_id, ncol=7) +
  theme(strip.background =element_rect(fill="white"))+
  my_theme + 
  labs(title = "Observed and fitted serotonin concentration in striatum by subject",
       x = "Time (min)", y = "Serotonin concentration (log scale)")

path_save4 <- "../../Result/piecewise_by_subject_ht_5_s.pdf"
ggsave(filename = path_save4, plot = p4, height = 12, width = 12)

# ================================================================================================
# Tables of fixed effects
# ================================================================================================

# Dopamine & cortex
da_c_table <- summary(lme_da_c)$tTable
da_c_table
xtable(da_c_table) # create LaTeX table
anova(lme_da_c)

# Dopamine & striatum
da_s_table <- summary(lme_da_s)$tTable
da_s_table
xtable(da_s_table) # create LaTeX table
anova(lme_da_s)

# Serotonin & cortex
ht_5_c_table <- summary(lme_ht_5_c)$tTable
ht_5_c_table
xtable(ht_5_c_table) # create LaTeX table
anova(lme_ht_5_c)

# Serotonin & striatum
ht_5_s_table <- summary(lme_ht_5_s)$tTable
ht_5_s_table
xtable(ht_5_s_table) # create LaTeX table
anova(lme_ht_5_s)