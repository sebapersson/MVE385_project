library(tidyverse)
library(latex2exp)

# General parameters for making good looking plots 
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                               plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=13)) 

# Colour-blind friendly palette
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

DOSE_START <- 0

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
  dose = col_factor()))

# ================================================================================================
# Start of functions 
# ================================================================================================
# Function that wil for a certain dosage and molecule plot the time series for each individual. 
# Note that the data won't be scaled by baseline 
# Args:
#   data_tidy, the data set in tidy format 
#   specie, the specie to plot
#   dose, the relevant dosage
#   specie_name, the name of the specie (with dose)
#   dose_name, dosage name (for the title)
# Returns:
#   void 
plot_individuals_non_scaled <- function(data_tidy, specie, dose_input, specie_name, dose_name)
{
  data_150_a <- data_tidy %>% 
    filter(dose == dose_input | dose == "NaCl") %>%
    select(id, position, Type, time, molecule, unique_id)  %>% 
    filter(position == "Cortex")
  
  data_150_s <- data_tidy %>% 
    filter(dose == dose_input | dose == "NaCl") %>%
    select(id, position, Type, time, DA, unique_id)  %>% 
    filter(position == "Striatum")
  
  # Required by gg-plot in order to plot 
  names(data_150_a)[5] <- "specie"
  names(data_150_s)[5] <- "specie"
  
  ylab <- specie
  title <- str_c(specie_name, " ", dose_name, " a")
  p1 <- ggplot(data_150_a, aes(time, specie, color = Type)) + 
    geom_line(aes(group = id)) + 
    geom_point() + 
    geom_vline(xintercept = DOSE_START, linetype = 2) + 
    scale_color_manual(values = cbPalette[-1]) + 
    labs(title = title, x = "Time [min]", y = specie_name) + 
    my_theme
  
  title <- str_c(specie_name, " ", dose_name, " s")
  p2 <- ggplot(data_150_s, aes(time, specie, color = Type)) + 
    geom_line(aes(group = id)) + 
    geom_point() + 
    geom_vline(xintercept = DOSE_START, linetype = 2) + 
    scale_color_manual(values = cbPalette[-1]) + 
    labs(title = title, x = "Time [min]", y = specie) + 
    my_theme
  
  ggpubr::ggarrange(p1, p2, ncol = 2)
}

# ================================================================================================
# Plotting over time for different indiviuals and different doses (and regions)
# ================================================================================================

# Dopamin, case vs control dosages, CC150 
molecule <- "DA"
dose <- "CC_150.0_mumol/kg"
specie_name = "Dopamin"; dose_name <- "CC 150"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)

# Dopamin, case vs control dosages, CC50 
molecule <- "DA"
dose <- "CC_50.0_mumol/kg"
specie_name = "Dopamin"; dose_name <- "CC 50"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)
# Note DA:
# Significant difference high dosage for cortex (bencmark test)

# NOA, case vs control dosages, CC150 
molecule <- "NOA"
dose <- "CC_150.0_mumol/kg"
specie_name = "NOA"; dose_name <- "CC 150"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)

# NOA, case vs control dosages, CC50 
molecule <- "NOA"
dose <- "CC_50.0_mumol/kg"
specie_name = "NOA"; dose_name <- "CC 50"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)
# Note NOA:
# cortex significant, bigger scale than s, s-region not significant

# HT_5, case vs control dosages, CC150 
molecule <- "HT_5"
dose <- "CC_150.0_mumol/kg"
specie_name = "HT 5"; dose_name <- "CC 150"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)

# HT_5, case vs control dosages, CC50
molecule <- "HT_5"
dose <- "CC_50.0_mumol/kg"
specie_name = "HT 5"; dose_name <- "CC 50"
plot_individuals_non_scaled(data_tidy, molecule, dose, specie_name, dose_name)

# ------------------------------------------------------------------------------------------------
# DA, summarise 
# ------------------------------------------------------------------------------------------------
da_sum_c <- data_tidy %>% 
  filter(position == "Cortex") %>% 
  select(time, DA, dose) %>%
  group_by(time, dose) %>% 
  summarise(mean = mean(DA, na.rm = T), 
            sd = sd(DA, na.rm = T)) %>% 
  mutate(lower_int = mean - sd) %>%
  mutate(upper_int = mean + sd)

ggplot(da_sum_c, aes(time, mean, color = dose, fill = dose)) + 
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin = lower_int, ymax = upper_int), alpha = 0.2, color = NA)
  scale_color_manual(values = cbPalette[-1]) + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "Cortex DA different dosages") + 
  my_theme

da_sum_s <- data_tidy %>% 
  filter(position == "Striatum") %>% 
  select(time, DA, dose) %>%
  group_by(time, dose) %>% 
  summarise(mean = mean(DA, na.rm = T), 
            sd = sd(DA, na.rm = T)) %>% 
  mutate(lower_int = mean - sd) %>%
  mutate(upper_int = mean + sd)

ggplot(da_sum_s, aes(time, mean, color = dose, fill = dose)) + 
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin = lower_int, ymax = upper_int), alpha = 0.2, color = NA) +
  scale_color_manual(values = cbPalette[-1]) + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "S-region DA different dosages") + 
  my_theme

# ------------------------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------------------------

# Convert to wide format
noa_wide <- data_tidy
noa_wide <- noa_wide %>%
  select(unique_id,time,NOA,position,dose) %>%
  mutate(time=paste0('noa_', time)) %>%
  spread(time, NOA) %>%
  select(unique_id, noa_0, noa_20, noa_40, noa_60, noa_80, noa_100,
        noa_120, noa_140, noa_160, noa_180, noa_200,
        noa_220, noa_240, noa_260, noa_280, noa_300,position,dose)

da_wide <- data_tidy
da_wide <- da_wide %>%
  select(unique_id,time,DA,position,dose) %>%
  mutate(time=paste0('da_', time)) %>%
  spread(time, DA) %>%
  select(unique_id, da_0, da_20, da_40, da_60, da_80, da_100,
         da_120, da_140, da_160, da_180, da_200,
         da_220, da_240, da_260, da_280, da_300,position,dose)

ht_5_wide <- data_tidy
ht_5_wide <- ht_5_wide %>%
  select(unique_id,time,HT_5,position,dose) %>%
  mutate(time=paste0('ht_5_', time)) %>%
  spread(time, HT_5) %>%
  select(unique_id, ht_5_0, ht_5_20, ht_5_40, ht_5_60, ht_5_80, ht_5_100,
         ht_5_120, ht_5_140, ht_5_160, ht_5_180, ht_5_200,
         ht_5_220, ht_5_240, ht_5_260, ht_5_280, ht_5_300,position,dose)


# View rows with any NAs from 20, 40, ..., 280 minutes
dim(noa_wide[rowSums(is.na(noa_wide[,seq(3,16)])) > 0,])[1]
dim(da_wide[rowSums(is.na(da_wide[,seq(3,16)])) > 0,])
dim(ht_5_wide[rowSums(is.na(ht_5_wide[,seq(3,16)])) > 0,])

# ----------------------------------------------------------------------------------------------
# Quanitfying the number of missing values 
# ----------------------------------------------------------------------------------------------
data_missing <- data_tidy %>%
  select(position, time, unique_id, NOA, DA, HT_5) %>%
  filter(time > -60) %>%
  filter(time != 200) %>%
  group_by(unique_id) %>%
  summarise(number_na_noa = sum(is.na(NOA)), 
            number_na_da = sum(is.na(DA)), 
            number_na_ht = sum(is.na(HT_5)))

missing_noa <- data_missing %>%
  filter(number_na_noa >= 10) %>%
  select(unique_id)

data_no_missing_noa <- data_missing %>%
  filter(number_na_noa == 0) %>%
  select(unique_id)

# For NOA we will drop (more than 10 missing values), only drop for NOA 
# AFA1041_1s
# AFA1041_2s
# AFA1042_1s
# AFA1042_2s
# AFA1053_2s
# AFA1062_1s
# AFA1062_2s
# LW854_1s  
# BML894_1s 
# BML1047_2s

data_sum_noa <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  filter(!(unique_id %in% missing_noa$unique_id)) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))


p1 <- ggplot(data_sum_noa, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "NOA with NA") + 
  ylim(0, 10) + 
  my_theme

data_complete_noa <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  filter(unique_id %in% data_no_missing_noa$unique_id) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))

p2 <- ggplot(data_complete_noa, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "NOA without NA") +
  ylim(0, 10) + 
  my_theme
  
ggpubr::ggarrange(p1, p2, ncol = 2)

# ---------------------------------------------------------------------------------------------
# NA-check for NOA 
# ---------------------------------------------------------------------------------------------
id_to_check_noa <- data_missing %>%
  filter(!(unique_id %in% missing_noa$unique_id)) %>%
  filter(number_na_noa != 0) %>%
  select(unique_id)

data_to_plot <- data_tidy %>%
  filter(time > -60) %>% 
  filter(time != 200) %>%
  filter(unique_id %in% id_to_check_noa$unique_id) %>%
  select(time, position, unique_id, NOA) %>%
  mutate(value = case_when(NOA > 0 ~ 0, 
                           is.na(NOA) ~ 1)) %>%
  mutate(time = as.factor(time))
  

ggplot(data_to_plot, aes(time, value, fill = unique_id)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = cbPalette) + 
  labs(title = "NOA missing values", x = "Time") + 
  my_theme

# ---------------------------------------------------------------------------------------------
# NA-check for DA 
# ---------------------------------------------------------------------------------------------
id_to_check_da <- data_missing %>%
  filter(number_na_da != 0) %>%
  select(unique_id)

data_to_plot <- data_tidy %>%
  filter(time > -60) %>%
  filter(time != 200) %>%
  filter(unique_id %in% id_to_check_da$unique_id) %>%
  select(time, position, unique_id, DA) %>%
  mutate(value = case_when(DA > 0 ~ 0, 
                           is.na(DA) ~ 1)) %>%
  mutate(time = as.factor(time))

ggplot(data_to_plot, aes(time, value, fill = unique_id)) +
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Paired") + 
  labs(title = "DA missing values", x = "Time") + 
  my_theme

# Making the bar-graphs 
data_da_with_na <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))

p1 <- ggplot(data_da_with_na, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "DA with NA") +
  ylim(0, 10) + 
  my_theme

data_no_missing_da <- data_missing %>%
  filter(number_na_da == 0) %>%
  select(unique_id)

data_complete_da <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  filter(unique_id %in% data_no_missing_da$unique_id) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))

p2 <- ggplot(data_complete_da, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "DA without NA") +
  ylim(0, 10) + 
  my_theme


ggpubr::ggarrange(p1, p2, ncol = 2)

# ---------------------------------------------------------------------------------------------
# HT_5 checking 
# ---------------------------------------------------------------------------------------------
id_to_check_ht <- data_missing %>%
  filter(number_na_ht != 0) %>%
  select(unique_id)

data_to_plot <- data_tidy %>%
  filter(time > -60) %>%
  filter(time != 200) %>%
  filter(unique_id %in% id_to_check_ht$unique_id) %>%
  select(time, position, unique_id, HT_5) %>%
  mutate(value = case_when(HT_5 > 0 ~ 0, 
                           is.na(HT_5) ~ 1)) %>%
  mutate(time = as.factor(time))

ggplot(data_to_plot, aes(time, value, fill = unique_id)) +
  geom_bar(stat = "identity") + 
  scale_fill_viridis_d() + 
  labs(title = "HT_5 missing values", x = "Time") + 
  my_theme

# Making the bar-graphs 
data_da_with_ht5 <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))

p1 <- ggplot(data_da_with_ht5, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "HT_5 with NA") +
  ylim(0, 10) + 
  my_theme

data_no_missing_ht5 <- data_missing %>%
  filter(number_na_ht == 0) %>%
  select(unique_id)

data_complete_ht5 <- data_tidy %>%
  filter(time > -60 & time != 200) %>%
  filter(unique_id %in% data_no_missing_ht5$unique_id) %>%
  mutate(time = as.factor(time)) %>%
  group_by(dose, position) %>%
  summarise(count = length(unique(unique_id)))

p2 <- ggplot(data_complete_ht5, aes(position, count, fill = dose)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = cbPalette[-1]) + 
  labs(title = "HT_5 without NA") +
  ylim(0, 10) + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)

# ---------------------------------------------------------------------------------------------
# Fix base-line 
# ---------------------------------------------------------------------------------------------
unique_id_vec <- levels(data_tidy$unique_id)
baseline_vec <- data.frame(matrix(0, nrow = length(unique_id_vec), ncol = 4))
j <- 1
for(i in 1:length(unique_id_vec)){
  data <- data_tidy %>%
    filter(unique_id == unique_id_vec[i])
  
  data <- data %>%
    filter(time >= -40 & time <= 0) %>%
    summarise(mean_val_NOA = mean(NOA, na.rm = T), 
              mean_val_DA = mean(DA, na.rm = T), 
              mean_val_HT_5 = mean(HT_5, na.rm = T))
  baseline_vec[j, 1] <- as.numeric(data$mean_val_NOA)
  baseline_vec[j, 2] <- as.numeric(data$mean_val_DA)
  baseline_vec[j, 3] <- as.numeric(data$mean_val_HT_5)
  baseline_vec[j, 4] <- unique_id_vec[i]
  j <- j + 1
}

data_baseline <- tibble(baseline_NOA = as.numeric(baseline_vec[, 1]), 
                        baseline_DA = as.numeric(baseline_vec[, 2]), 
                        baseline_HT_5 = as.numeric(baseline_vec[, 3]), 
                        unique_id = as.factor(baseline_vec[, 4])) 
data_baseline$baseline_NOA[is.nan(data_baseline$baseline_NOA)] <- NA
data_baseline$baseline_DA[is.nan(data_baseline$baseline_DA)] <- NA
data_baseline$baseline_HT_5[is.nan(data_baseline$baseline_HT_5)] <- NA


# Merge with the tidy data-set 
test <- inner_join(data_tidy, data_baseline, by = "unique_id") %>%
  mutate(unique_id = as.factor(unique_id)) %>%
  mutate(b_NOA = NOA / baseline_NOA) %>%
  mutate(b_DA = DA / baseline_DA) %>%
  mutate(b_HT_5 =  HT_5 / baseline_HT_5) %>%
  select(position, sample, Type, time, NOA, DA, HT_5, unique_id, b_NOA, b_DA, b_HT_5, dose)

max(test$b_DA, na.rm = T)
min(test$b_DA, na.rm = T)







