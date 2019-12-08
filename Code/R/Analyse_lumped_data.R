library(tidyverse)

# General parameters for making good looking plots 
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                               plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=13)) 

# Colour-blind friendly palette
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# This file contains the analysis by repeated measure ANOVA (and mixed modelling) where each the time points 
# are lumped into three regions (20-60, 80-120, 140-180). Note that the data-set contains the baseline and none 
# baseline scaled data. (Variables on the form b_NOA refers to baseline scaled NOA)

# Note that time is cast as a factor this time 
path_data <- "../../Intermediate/Data_tidy_agg.csv"
data_tidy_agg <- read_csv(path_data, col_types = cols(
  unique_id = col_factor(), 
  position = col_factor(), 
  Type = col_factor(), 
  time = col_factor(), 
  NOA = col_double(), 
  b_NOA = col_double(), 
  DA = col_double(), 
  b_DA = col_double(), 
  HT_5 = col_double(), 
  b_HT_5 = col_double(), 
  base_DA = col_double(),
  base_NOA = col_double(), 
  base_HT_5 = col_double()))
