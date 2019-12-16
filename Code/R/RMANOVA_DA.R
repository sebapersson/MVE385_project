library(tidyverse)
library(ez)
library(afex)
library(ggplot2)


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
  filter(time > 0) %>%
  filter(time != 200) 
# For the tidyverse (and ggplot) to work
names(data_missing)[4] <- "specie"
data_missing <- data_missing %>%
  group_by(unique_id) %>%
  summarise(number_na = sum(is.na(specie)))

# The id if no observations are missing 
data_no_missing <- data_missing %>%
  filter(number_na == 0) %>%
  select(unique_id)

# Aggreate number of samples with no missing 
data_without_missing <- data_tidy %>%
  filter(time > 0 & time != 200) %>%
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
#   short_dose, test dose without decimal point (i.e. CC_150.0_mumol/kg -> 150)
#   save_plot, TRUE/FALSE statement wether or not to save plots. Only save significant
# Returns:
#   ANOVA table and plot 
RMANOVA_nontransformed_DA <- function(data_tidy, test_position, test_dose, short_dose, save_plot){
  data_tidy_RMANOVA_nontransformed<-data_tidy %>%
    select(position, unique_id, time, DA, Type, dose) %>%
    mutate(time_cat = as.factor(time)) %>%
    filter(position == test_position) %>%
    filter(dose == 'NaCl' | dose == test_dose) %>%
    filter(time_cat == '20' | time_cat == '40'| time_cat == '60'| time_cat == '80' | time_cat == '100' | time_cat == '120' | time_cat == '140' | time_cat == '160' | time_cat == '180') %>%
    filter(unique_id %in% data_no_missing$unique_id)
  
  result_DA <- ez.glm(
    id="unique_id"
    , dv="DA"
    , data=data_tidy_RMANOVA_nontransformed
    , between = "Type"
    , within =  "time_cat"
    , type = 3
    , return = "univariate"
    , print.formula = TRUE
  )
  
  lm <- ez.glm(
    id="unique_id"
    , dv="DA"
    , data=data_tidy_RMANOVA_nontransformed
    , between = "Type"
    , within =  "time_cat"
    , type = 3
    , return ="lm"
    , print.formula = TRUE
  )
  
  nind=length(unique(data_tidy_RMANOVA_nontransformed$unique_id))
  res <- tibble(res = as.numeric(lm$residuals)) %>%
    mutate(time = rep(seq(from = 20, by = 20, to = 180), each = nind))
  
  # Currently not used
  title <- str_c("DA ", test_position, " ", test_dose)
  p1 <- ggplot(res, aes(time, res)) + 
    geom_point() + 
    geom_hline(yintercept = 0) + 
    geom_smooth(method = "loess", se = F, color = cbPalette[4], size = 1.2) +
    labs(title = title , x = "time [min]", y = "residuals") +
    my_theme
  
  title <- str_c("DA ", test_position, " ", test_dose)
  p2 <- ggplot(res, aes(sample = res)) + 
    geom_qq(colour=cbPalette[3]) + 
    geom_qq_line() + 
    labs(title = title) +
    my_theme 
  
  p3 <- ezPlot(data_tidy_RMANOVA_nontransformed
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
  
  # Saving plots:
  if(save_plot == TRUE){
  path_save1 <- str_c("../../Result/qq_Dopamin_", test_position,"_", short_dose, ".pdf")
  print(p2)
  ggsave(filename = path_save1, plot = p2, height = 6, width = 9)
  dev.off()
  path_save2 <- str_c("../../Result/ez_Dopamin_", test_position,"_", short_dose, ".pdf")
  print(p3)
  ggsave(filename = path_save2, plot = p3, height = 6, width = 9)
  dev.off()
  
  print(p2)
  print(p3)
  }
  return(result_DA)
}

# Function performing and plotting results from RMANOVA for Dopamin at different dosages and 
# positions for transformed (baseline) data. 
# Args:
#   data_tidy, the data set in tidy format
#   test_position, the brain region to be tested
#   test_dose, the relevant dosage to be compared to control (NaCl)
#   short_dose, test dose without decimal point (i.e. CC_150.0_mumol/kg -> 150)
#   save_plot, TRUE/FALSE statement wether or not to save plots. Only save significant
# Returns:
#   ANOVA table and plot 
RMANOVA_transformed_DA <- function(data_tidy, test_position, test_dose, short_dose, save_plot){
  data_tidy_RMANOVA_transformed<-data_tidy %>%
    select(id, position, unique_id, time, b_DA, Type, dose) %>%
    mutate(time_cat = as.factor(time)) %>%
    filter(position == test_position) %>%
    filter(dose == 'NaCl' | dose == test_dose) %>%
    filter(time_cat == '20' | time_cat == '40'| time_cat == '60'| time_cat == '80' | time_cat == '100' | time_cat == '120' | time_cat == '140' | time_cat == '160' | time_cat == '180') %>%
    filter(unique_id %in% data_no_missing$unique_id)
  
  result_b_DA <- ez.glm(
    id="unique_id"
    , dv="b_DA"
    , data=data_tidy_RMANOVA_transformed
    , between = "Type"
    , within =  "time_cat"
    , type = 3
    , return = "univariate"
    , print.formula = TRUE
  )
  
  lm <- ez.glm(
    id="unique_id"
    , dv="b_DA"
    , data=data_tidy_RMANOVA_transformed
    , between = "Type"
    , within =  "time_cat"
    , type = 3
    , return ="lm"
    , print.formula = TRUE
  )
  
  nind=length(unique(data_tidy_RMANOVA_transformed$unique_id))
  res <- tibble(res = as.numeric(lm$residuals)) %>%
    mutate(time = rep(seq(from = 20, by = 20, to = 180), each = nind))
  
  # Currently not used
  title <- str_c("Baseline DA ", test_position, " ", test_dose)
  p1 <- ggplot(res, aes(time, res)) + 
    geom_point() + 
    geom_hline(yintercept = 0) + 
    geom_smooth(method = "loess", se = F, color = cbPalette[4], size = 1.2) +
    labs(title = title, x = "time [min]", y = "residuals") +
    my_theme
  
  title <- str_c("Baseline DA ", test_position, " ", test_dose)
  p2 <- ggplot(res, aes(sample = res)) + 
    geom_qq(colour=cbPalette[3]) + 
    geom_qq_line() + 
    labs(title = title) +
    my_theme 
  
  p3 <- ezPlot(data_tidy_RMANOVA_transformed
               , dv=.(b_DA)
               , wid=.(unique_id)
               , within=.(time_cat)
               , between = .(Type)
               , type = 3
               , x=.(time_cat)
               , x_lab='time'
               , y_lab='Baseline Dopamin value'
               , split=.(Type)
  )
  # Saving plots:
  if(save_plot == TRUE){
  path_save1 <- str_c("../../Result/qq_Baseline_Dopamin_", test_position,"_", short_dose, ".pdf")
  print(p2)
  ggsave(filename = path_save1, plot = p2, height = 6, width = 9)
  dev.off()
  path_save2 <- str_c("../../Result/ez_Baseline_Dopamin_", test_position,"_", short_dose, ".pdf")
  print(p3)
  ggsave(filename = path_save2, plot = p3, height = 6, width = 9)
  dev.off()
  
  print(p2)
  print(p3)
  }
  return(result_b_DA)
}

# ================================================================================================
# Results nontransformed Dopamin (DA)
# ================================================================================================

### Striatum
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_150.0_mumol/kg", short_dose = "150", save_plot=FALSE) # not significant
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_50.0_mumol/kg", short_dose = "50", save_plot=FALSE)  # not significant
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_16.7_mumol/kg", short_dose = "16", save_plot=FALSE)  # not significant
RMANOVA_nontransformed_DA(data_tidy, test_position="Striatum", test_dose="CC_5.6_mumol/kg", short_dose = "5", save_plot=TRUE)   # time significant, violated sphericity, significant corrections

### Cortex
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_150.0_mumol/kg", short_dose = "150", save_plot=TRUE)   # Type and time significant, violated sphericity. Significant corrections  
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_50.0_mumol/kg", short_dose = "50", save_plot=TRUE)    # Type and time significant, violated sphericity. Significant corrections  
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_16.7_mumol/kg", short_dose = "16", save_plot=TRUE)    # Type and time significant, violated sphericity. Significant corrections  
RMANOVA_nontransformed_DA(data_tidy, test_position="Cortex", test_dose="CC_5.6_mumol/kg", short_dose = "5", save_plot=TRUE)     # time significant, violated sphericity. Significant corrections


# ================================================================================================
# Results transformed Dopamin (b_DA)
# ================================================================================================

### Striatum
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_150.0_mumol/kg", short_dose = "150", save_plot=FALSE) # not significant
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_50.0_mumol/kg", short_dose = "50", save_plot=TRUE)  # Type significant
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_16.7_mumol/kg", short_dose = "16", save_plot=TRUE)  # time significant, violated sphericity. Significant corrections   
RMANOVA_transformed_DA(data_tidy, test_position="Striatum", test_dose="CC_5.6_mumol/kg", short_dose = "5", save_plot=TRUE)   # time significant, violated sphericity. Significant corrections

### Cortex
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_150.0_mumol/kg", short_dose = "150", save_plot=FALSE)   # significant, violated sphericity. Significant corrections
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_50.0_mumol/kg", short_dose = "50", save_plot=FALSE)    # significant, violated sphericity. Significant corrections
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_16.7_mumol/kg", short_dose = "16", save_plot=TRUE)    # time significant, violated sphericity. Significant corrections
RMANOVA_transformed_DA(data_tidy, test_position="Cortex", test_dose="CC_5.6_mumol/kg", short_dose = "5", save_plot=TRUE)     # time significant, violated sphericity. Significant corrections

