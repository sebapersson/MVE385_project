library(tidyverse)
library(ez)

# General parameters for making good looking plots 
my_theme <- theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"), 
                               plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.title=element_text(size=13)) 

# Colour-blind friendly palette
cbPalette <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# This file contains the analysis by repeated measure ANOVA (and mixed modelling) where each the time points 
# aren't limpued. Note that the data-set contains the baseline and none baseline scaled data. (Variables on the 
# form b_NOA refers to baseline scaled NOA)
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
# Filtering data for missing DA-values 
# ================================================================================================
specie="DA"
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

# ================================================================================================
# Start of functions
# ================================================================================================
  
# Function performing and plotting results from RMANOVA for Dopamin at different dosages and 
# positions for non-transformed data. 
# Args:
#   data_tidy, the data set in tidy format
#   test_position, the brain region to be tested
#   test_dose, the relevant dosage to be compared to control (NaCl)
# Returns:
#   ANOVA table and plot 
RMANOVA_nontransformed_DA <- function(data_tidy, test_position, test_dose){
  data_tidy_RMANOVA_nontransformed<-data_tidy %>%
    select(id, position, unique_id, time, DA, Type, dose) %>%
    mutate(time_cat = as.factor(time)) %>%
    filter(position == test_position) %>%
    filter(dose == 'NaCl' | dose == test_dose) %>%
    filter(time_cat == '20' | time_cat == '40'| time_cat == '60'| time_cat == '80' | time_cat == '100' | time_cat == '120' | time_cat == '140' | time_cat == '160' | time_cat == '180') %>%
    filter(unique_id %in% data_no_missing$unique_id)
  
  ezDesign(data_tidy_RMANOVA_nontransformed,time_cat,Type)
  
  result_DA <- ezANOVA(
    data_tidy_RMANOVA_nontransformed
    , dv=.(DA)
    , wid=.(unique_id)
    , within=.(time_cat)
    , between = .(Type)
    , type = 3
    , detailed = TRUE
  )
  
  
  p1 <- ezPlot(data_tidy_RMANOVA_nontransformed
    , dv=.(DA)
    , wid=.(unique_id)
    , within=.(time_cat)
    , between = .(Type)
    , type = 3
    , x=.(time_cat)
    , x_lab='time'
    , y_lab='Dopamin value'
    , split=.(Type)
  )
  
  return(list(result_DA, p1))
}
  


# Function performing and plotting results from RMANOVA for Dopamin at different dosages and 
# positions for transformed (baseline) data. 
# Args:
#   data_tidy, the data set in tidy format
#   test_position, the brain region to be tested
#   test_dose, the relevant dosage to be compared to control (NaCl)
# Returns:
#   ANOVA table and plot 
RMANOVA_transformed_DA <- function(data_tidy, test_position, test_dose){
  data_tidy_RMANOVA_transformed<-data_tidy %>%
    select(id, position, unique_id, time, b_DA, Type, dose) %>%
    mutate(time_cat = as.factor(time)) %>%
    filter(position == test_position) %>%
    filter(dose == 'NaCl' | dose == test_dose) %>%
    filter(time_cat == '20' | time_cat == '40'| time_cat == '60'| time_cat == '80' | time_cat == '100' | time_cat == '120' | time_cat == '140' | time_cat == '160' | time_cat == '180') %>%
    filter(unique_id %in% data_no_missing$unique_id)
  
  ezDesign(data_tidy_RMANOVA_transformed,time_cat,Type)
  
  result_b_DA <- ezANOVA(
    data_tidy_RMANOVA_transformed
    , dv=.(b_DA)
    , wid=.(unique_id)
    , within=.(time_cat)
    , between = .(Type)
    , type = 3
    , detailed = TRUE
  )
  
  
  p1 <- ezPlot(data_tidy_RMANOVA_transformed
               , dv=.(b_DA)
               , wid=.(unique_id)
               , within=.(time_cat)
               , between = .(Type)
               , type = 3
               , x=.(time_cat)
               , x_lab='time'
               , y_lab='Dopamin baseline value'
               , split=.(Type)
  )
  
  return(list(result_b_DA, p1))
}

# ================================================================================================
# Results nontransformed Dopamin (DA)
# ================================================================================================

### Striatum
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_150.0_mumol/kg") # This test only has 2 case subjects!
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_50.0_mumol/kg")  # not significant
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_16.7_mumol/kg")  # time significant, violated sphericity 
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_5.6_mumol/kg")   # time and Type significant, violated sphericity. One significant HF-correction
### Cortex
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_150.0_mumol/kg")   # significant, violated sphericity. Significant corrections  ??
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_50.0_mumol/kg")    # significant, violated sphericity. Significant corrections  ??
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_16.7_mumol/kg")    # significant, violated sphericity. Significant corrections  ??
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_5.6_mumol/kg")     # time significant, violated sphericity. One GG and one HF correction significant  ??


# ================================================================================================
# Results transformed Dopamin (b_DA)
# ================================================================================================

### Striatum
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_150.0_mumol/kg") # not significant
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_50.0_mumol/kg")  # time and Type significant, violated sphericity
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_16.7_mumol/kg")  # time significant, violated sphericity. One significant HF-correction  ??
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_5.6_mumol/kg")   # time significant, violated sphericity

### Cortex
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_150.0_mumol/kg")   # significant, violated sphericity. Significant corrections  ??
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_50.0_mumol/kg")    # significant, violated sphericity. Significant corrections  ??
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_16.7_mumol/kg")    # time and interaction significant, violated sphericity. One GG, twp HF-corrections significant  ??
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_5.6_mumol/kg")     # time and Type significant, violated sphericity. One significant HF-correction  ??





  

