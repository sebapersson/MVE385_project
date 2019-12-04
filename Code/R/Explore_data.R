library(tidyverse)
library(latex2exp)
library(RColorBrewer)

# General parameters for making good looking plots 
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                               plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=13)) 

# Colour-blind friendly palette
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# For plots were conditions have an ordering 
my_palette <- brewer.pal(n = 9, name = "GnBu")[-c(1, 2, 3)]

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
  dose = col_factor(), 
  baseline_NOA = col_double(), 
  baseline_DA = col_double(), 
  baseline_HT_5 = col_double(), 
  b_NOA = col_double(), 
  b_DA = col_double(), 
  b_HT_5 = col_double()))

# Refactor dose (to make sense when plotting)
dose_ordered <- fct_relevel(data_tidy$dose, "NaCl", after = 0) %>%
  fct_relevel("CC_5.6_mumol/kg", after = 1) %>%
  fct_relevel("CC_16.7_mumol/kg", after = 2) %>%
  fct_relevel("CC_50.0_mumol/kg", after = 3) %>%
  fct_relevel("CC_150.0_mumol/kg", after = 4)
data_tidy$dose <- dose_ordered

# ================================================================================================
# Start of functions 
# ================================================================================================


# Function that will for a certain dosage and molecule plot the time series for each individual. 
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
  
  ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = T, legend = "bottom")
}


# Function that will plot summarised data for a certain spece. 
# TODO: This function will in the future
# be extended to handle base-line corrected data.
# Args:
#   data_tidy, the data set in tidy format 
#   specie, the specie to plot
#   specie_name, the name of the specie
# Returns:
#   void 
plot_summarised_data <- function(data_tidy, specie, specie_name)
{
  
  data_sum_c <- data_tidy %>% 
    filter(position == "Cortex") %>% 
    select(time, specie, dose)
  
  # To avoid errors with dplyr
  names(data_sum_c)[2] <- "specie"
  
  # Compute spread and mean   
  data_sum_c <- data_sum_c %>%
    group_by(time, dose) %>% 
    summarise(mean = mean(specie, na.rm = T), 
              sd = sd(specie, na.rm = T)) %>% 
    mutate(lower_int = mean - sd) %>%
    mutate(upper_int = mean + sd) %>%
    rename("Dose" = "dose")
  
  data_sum_s <- data_tidy %>% 
    filter(position == "Striatum") %>% 
    select(time, specie, dose)
  
  names(data_sum_s)[2] <- "specie"
  data_sum_s <- data_sum_s %>%
    group_by(time, dose) %>% 
    summarise(mean = mean(specie, na.rm = T), 
              sd = sd(specie, na.rm = T)) %>% 
    mutate(lower_int = mean - sd) %>%
    mutate(upper_int = mean + sd) %>%
    rename("Dose" = "dose")
  
  title = str_c(specie_name, " cortex different dosages")
  p1 <-  ggplot(data_sum_c, aes(time, mean, color = Dose, fill = Dose)) + 
    geom_line(size = 1.2) + 
    geom_ribbon(aes(ymin = lower_int, ymax = upper_int), alpha = 0.2, color = NA) + 
    scale_color_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    labs(title = title, y = specie_name, x = "Time [min]") + 
    my_theme + theme(legend.title = element_blank())
  
  title = str_c(specie_name, " striatium different dosages")
  p2 <- ggplot(data_sum_s, aes(time, mean, color = Dose, fill = Dose)) + 
    geom_line(size = 1.2) + 
    geom_ribbon(aes(ymin = lower_int, ymax = upper_int), alpha = 0.2, color = NA) +
    scale_color_manual(values = my_palette) +
    scale_fill_manual(values = my_palette) +
    labs(title = title, y = specie_name, x = "Time [min]") +
    my_theme + theme(legend.title = element_blank())
  
  ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = T, legend = "bottom")
}


# Function that will plot the number of observations for a certain dose with and without 
# missing values. The purpose of this test is to find the number of observations that 
# are lost using an ANOVA-test 
# Args:
#   specie, the molecule of interest
#   specie_name, the name of the specie
# Returns:
#   void 
plot_missing_values <- function(specie, specie_name)
{
  
  # Calculate number of missing values 
  data_missing <- data_tidy %>%
    select(position, time, unique_id, specie) %>%
    filter(time > -60) %>%
    filter(time != 200) 
  # For the tidyverse (and ggplot) to work
  names(data_missing)[4] <- "specie"
  data_missing <- data_missing %>%
    group_by(unique_id) %>%
    summarise(number_na = sum(is.na(specie)))
  
  # The id if more (or equal) to 10 observations are missing 
  missing_many <- data_missing %>%
    filter(number_na >= 10) %>%
    select(unique_id)
  
  # The id if no observations are missing 
  data_no_missing <- data_missing %>%
    filter(number_na == 0) %>%
    select(unique_id)
  
  # Aggregate number of individuals with missing (but less than 10)
  data_with_missing <- data_tidy %>%
    filter(time > -60 & time != 200) %>%
    filter(!(unique_id %in% missing_many$unique_id)) %>%
    mutate(time = as.factor(time)) %>%
    group_by(dose, position) %>%
    summarise(count = length(unique(unique_id)))
  
  # Aggreate number of samples with no missing 
  data_without_missing <- data_tidy %>%
    filter(time > -60 & time != 200) %>%
    filter(unique_id %in% data_no_missing$unique_id) %>%
    mutate(time = as.factor(time)) %>%
    group_by(dose, position) %>%
    summarise(count = length(unique(unique_id)))
  
  title <- str_c(specie_name, " with NA-values")
  p1 <- ggplot(data_with_missing, aes(position, count, fill = dose)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = cbPalette[-1]) + 
    labs(title = title, x = "", y = "Number of samples") + 
    ylim(0, 10) + 
    my_theme
  
  title <- str_c(specie_name, " without NA-values")
  p2 <- ggplot(data_without_missing, aes(position, count, fill = dose)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    scale_fill_manual(values = cbPalette[-1]) + 
    labs(title = title, x = "", y = "Number of samples") +
    ylim(0, 10) + 
    my_theme
  
  ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = T, legend = "bottom")
}


# Function that plots the missing values over time for a certain individual, this is in order 
# to see that the missing values aren't accumulated at a certain location. 
# Args:
#   specie, the molecule of interest
#   specie_name, the name of the specie
# Returns:
#   void 
plot_dist_missing_values <- function(specie, specie_name)
{
  # Calculate number of missing values 
  data_missing <- data_tidy %>%
    select(position, time, unique_id, specie) %>%
    filter(time > -60) %>%
    filter(time != 200) 
  # For the tidyverse (and ggplot) to work
  names(data_missing)[4] <- "specie"
  data_missing <- data_missing %>%
    group_by(unique_id) %>%
    summarise(number_na = sum(is.na(specie)))
  
  # The id if more (or equal) to 10 observations are missing 
  missing_many <- data_missing %>%
    filter(number_na >= 10) %>%
    select(unique_id)
  
  # Select id:s to plot
  id_to_check <- data_missing %>%
    filter(!(unique_id %in% missing_many$unique_id)) %>%
    filter(number_na != 0) %>%
    select(unique_id)
  
  # Fix the data in a manner it can be plotted in 
  data_to_plot <- data_tidy %>%
    filter(time > -60) %>% 
    filter(time != 200) %>%
    filter(unique_id %in% id_to_check$unique_id) %>%
    select(time, position, unique_id, specie)
  # For the tidyverse to work 
  names(data_to_plot)[4] <- "specie"
  data_to_plot <- data_to_plot %>%
    mutate(value = case_when(specie > 0 ~ 0, 
                             is.na(specie) ~ 1)) %>%
    mutate(time = as.factor(time))
  
  title <- str_c(specie_name, " distribution missing values")
  ggplot(data_to_plot, aes(time, value, fill = unique_id)) +
    geom_bar(stat = "identity") + 
    scale_fill_viridis_d() + 
    labs(title = title, x = "Time", y = "") + 
    my_theme + theme(legend.title = element_blank())
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


# ================================================================================================
# Plotting summarised data 
# ================================================================================================

# Dopamin summarised data 
molecule <- "DA"
specie_name = "Dopamin"
plot_summarised_data(data_tidy, molecule, specie_name)
# Dopamin summarised data baseline
molecule <- "b_DA"
specie_name = "Dopamin Baseline"
plot_summarised_data(data_tidy, molecule, specie_name)

# NOA summarised data 
molecule <- "NOA"
specie_name = "NOA"
plot_summarised_data(data_tidy, molecule, specie_name)
# NOA summarised data baseline
molecule <- "b_NOA"
specie_name = "NOA baseline"
plot_summarised_data(data_tidy, molecule, specie_name)

# HT_5 summarised data
molecule <- "HT_5"
specie_name = "HT-5"
plot_summarised_data(data_tidy, molecule, specie_name)
# HT_5 summarised data baseline
molecule <- "b_HT_5"
specie_name = "HT-5 baseline"
plot_summarised_data(data_tidy, molecule, specie_name)

# ------------------------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------------------------
# TODO: Consider removing the wide-data format 
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

# ================================================================================================
# Quantifaying the number of missing values 
# ================================================================================================
# For NOA
specie <- "NOA"
specie_name <- "NOA"
plot_missing_values(specie, specie_name)
plot_dist_missing_values(specie, specie_name)

# For DA
specie <- "DA"
specie_name <- "Dopamin"
plot_missing_values(specie, specie_name)
plot_dist_missing_values(specie, specie_name)

# HT-5
specie <- "HT_5"
specie_name <- "HT-5"
plot_missing_values(specie, specie_name)
plot_dist_missing_values(specie, specie_name)

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
