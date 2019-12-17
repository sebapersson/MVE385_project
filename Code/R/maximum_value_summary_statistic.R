### Maximum value summary statistic ###

# - Calculates maximum value per unique id as summary statistic
#       - baseline transformed dopamine (b_DA) and serotonin (b_HT_5) used
#       - Removed outlier with unique_id==BML894_2a and time==180 (NaCl group)
# - Maximum values compared between doses:
#         - graphically using boxplots
#         - Non-parametric test (Kruskal-Wallis)
#         - Post-hoc test: Wilcoxon rank sum test with NaCl as reference level,
#           Bonferroni method to adjust for multiplicity

#######################################

library(tidyverse)
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

# Dopamine (baseline transformed) & cortex
data_da_c<-data %>%
  select(unique_id, position, time, dose, b_DA) %>%
  filter(position=='Cortex') %>%
  filter(time > 0) %>%
filter(!is.na(b_DA)) %>%
  select(unique_id,time,dose,b_DA) %>%
  filter(!(unique_id=="BML894_2a" & time==180)) # remove outlier
dim(data_da_c) #      248 4 
# Calculate max response per unique_id
max_data_da_c <- data_da_c %>% 
  group_by(dose,unique_id) %>%
  summarise(max_b_DA=max(b_DA))

# Dopamine (baseline transformed) & striatum
data_da_s<-data %>%
  select(unique_id, position, time, dose, b_DA) %>%
  filter(position=='Striatum') %>%
  filter(time > 0) %>%
  filter(!is.na(b_DA)) %>%
  select(unique_id,time,dose,b_DA)
dim(data_da_s) #      257 4 
# Calculate max response per unique_id
max_data_da_s <- data_da_s %>% 
  group_by(dose,unique_id) %>%
  summarise(max_b_DA=max(b_DA))

# Serotonin (baseline transformed) & cortex
data_ht_5_c<-data %>%
  select(unique_id, position, time, dose, b_HT_5) %>%
  filter(position=='Cortex') %>%
  filter(time > 0) %>%
  filter(!is.na(b_HT_5)) %>%
  select(unique_id,time,dose,b_HT_5)
dim(data_ht_5_c) #      246 4 
# Calculate max response per unique_id
max_data_ht_5_c <- data_ht_5_c %>% 
  group_by(dose,unique_id) %>%
  summarise(max_b_HT_5=max(b_HT_5))

# Serotonin (baseline transformed) & striatum
data_ht_5_s<-data %>%
  select(unique_id, position, time, dose, b_HT_5) %>%
  filter(position=='Striatum') %>%
  filter(time > 0) %>%
  filter(!is.na(b_HT_5)) %>%
  select(unique_id,time,dose,b_HT_5)
dim(data_ht_5_s) #      255 4 
# Calculate max response per unique_id
max_data_ht_5_s <- data_ht_5_s %>% 
  group_by(dose,unique_id) %>%
  summarise(max_b_HT_5=max(b_HT_5))

# ================================================================================================
# Boxplots of maximum responses
# ================================================================================================

title <- "Maximum dopamine concentration in cortex by dose"
p1 <- ggplot(max_data_da_c, aes(y=max_b_DA, x=dose)) +
  geom_boxplot() + 
  labs(title = title, x = "Time (min)", y = "Maximum concentration") +
  my_theme + 
  ylim(0,8)

path_save1 <- "../../Result/maximum_boxplot_da_c.pdf"
ggsave(filename = path_save1, plot = p1, height = 6, width = 9)

title <- "Maximum dopamine concentration in striatum by dose"
p2 <- ggplot(max_data_da_s, aes(y=max_b_DA, x=dose)) +
  geom_boxplot() + 
  labs(title = title, x = "Time (min)", y = "Maximum concentration") +
  my_theme +
  ylim(0,8)

path_save2 <- "../../Result/maximum_boxplot_da_s.pdf"
ggsave(filename = path_save2, plot = p2, height = 6, width = 9)

title <- "Maximum serotonin concentration in cortex by dose"
p3 <- ggplot(max_data_ht_5_c, aes(y=max_b_HT_5, x=dose)) +
  geom_boxplot() + 
  labs(title = title, x = "Time (min)", y = "Maximum concentration") +
  my_theme +
  ylim(0,8.5)

path_save3 <- "../../Result/maximum_boxplot_ht_5_c.pdf"
ggsave(filename = path_save3, plot = p3, height = 6, width = 9)

title <- "Maximum serotonin concentration in striatum by dose"
p4 <- ggplot(max_data_ht_5_s, aes(y=max_b_HT_5, x=dose)) +
  geom_boxplot() + 
  labs(title = title, x = "Time (min)", y = "Maximum concentration") +
  my_theme +
  ylim(0,8.5)

path_save4 <- "../../Result/maximum_boxplot_ht_5_s.pdf"
ggsave(filename = path_save4, plot = p4, height = 6, width = 9)

# ================================================================================================
# Kruskal-Wallis test
# ================================================================================================

# Dopamine (baseline transformed) & cortex
kruskal.test(max_b_DA ~ dose, data = max_data_da_c) # p-value = 0.0008633 *** significant ***
# If outlier removed: p-value = 0.0002921

# Dopamine (baseline transformed) & striatum
kruskal.test(max_b_DA ~ dose, data = max_data_da_s) # p-value = 0.4139

# Serotonin (baseline transformed) & cortex
kruskal.test(max_b_HT_5 ~ dose, data = max_data_ht_5_c) # p-value = 0.02358 *** significant ***

# Serotonin (baseline transformed) & striatum
kruskal.test(max_b_HT_5 ~ dose, data = max_data_ht_5_s) # p-value = 0.0949

# ================================================================================================
# Wilcoxon rank sum test
# ================================================================================================

compare_means(max_b_DA ~ dose, max_data_da_c, ref.group = "NaCl",
              p.adjust.method="bonferroni")
# p.adj: 1.000 (dose 5.6), 0.011 (dose 16.7), 0.004 (dose 50), 0.011 (dose 150)
# Significant at all doses except dose 5.6

compare_means(max_b_HT_5 ~ dose, max_data_ht_5_c, ref.group = "NaCl",
              p.adjust.method="bonferroni")
# p.adj: 1.000 (dose 5.6), 0.0045 (dose 16.7), 0.028 (dose 50), 0.011 (dose 150)
# Significant at all doses except dose 5.6