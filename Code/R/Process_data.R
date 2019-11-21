library(tidyverse)
library(stringr)
library(xlsx)

# Function that will read in the data for an experiment on one person for a certain position and convert the data 
# into a tibble. Note that the file will be read upwards to downward, left to right. 
# Args: 
#   path_data, the p  ath to the data file (from where the R-file is located)
#   rows_to_read, a list with the start and end rows
#   cols_to_read, a list with the columns to read for each condition 
#   dose, a string with the dosage 
#   position, a vector with the position for each experiment 
#   id_patient, a vector with the id for each patient
#   n_samples, the number of samples per patient, by defult this tends to be 15 
# Returns:
#   a tibble with the observed data in tidy-format 
read_one_experiment <- function(path_data, rows_to_read, cols_to_read, dose, position, id_patient, n_samples=15)
{

  # The time vector 
  t_vec <- c(seq(from = 0, by = 20, length.out = 7), seq(from = 140, by = 20, length.out = n_samples - 7))
  # The different substances 
  col_names <- c("NOA", "DA", "MT_3", "NM", "HT_5", "DOPAC", "HIAA", "HVA")

  # For having unique id:s
  to_concatenate <- as.character(1:12)
  
  # Read all the cases in the file 
  data_list <- lapply(1:length(rows_to_read), function(i){
    data_raw <- read.xlsx2(path_data, 1, startRow = rows_to_read[[i]][1], endRow = rows_to_read[[i]][2], colIndex = cols_to_read[[i]])
    # Make data into tibble for ease of working with
    data_values <- as_tibble(as.matrix(data_raw[4:(n_samples+4-1), 6:13]) )
    names(data_values) <- col_names
    
    # Fix missing values 
    data_values[data_values == "n.a." | data_values == ""] <- NA
    
    # Fix data-type for each column 
    data_values <- data_values %>% mutate_if(is.character, as.double)
    
    # Add dose, position and id 
    data_values <- data_values %>%
      mutate(id = id_patient[i]) %>%
      mutate(unique_id = str_c(id_patient[i], to_concatenate[i])) %>%
      mutate(position = position[i]) %>%
      mutate(sample = 0:(n_samples-1)) %>% 
      mutate(time = t_vec) %>%  
      mutate(dose = dose) %>%
      select(id, position, sample, time, everything())
    return(data_values)
  })
  
  # Make the data into a tibble 
  data_read <- do.call(rbind, data_list)
  return(data_read)
}

# -------------------------------------------------------------------------------
# Experiment AFA1062
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC6/AFA1062.xlsx"
dose <- "CC_5.6_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1062_1", "AFA1062_2", "AFA1062_1", "AFA1062_2")

data_AFA1062 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1066
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC6/AFA1063.xlsx"
dose <- "CC_5.6_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1063_1", "AFA1063_2", "AFA1063_1", "AFA1063_2")

data_AFA1063 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1068
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC6/AFA1068.xlsx"
dose <- "CC_5.6_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1068_1", "AFA1068_2", "AFA1068_1", "AFA1068_2")

data_AFA1068 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1024
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC16/AFA1024.xlsx"
dose <- "CC_16.7_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(20, 38), c(20, 38))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1024_1", "AFA1024_2", "AFA1024_1", "AFA1024_2")

data_AFA1024 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA167
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC16/AFA1067.xlsx"
dose <- "CC_16.7_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1067_1", "AFA1067_2", "AFA1067_1", "AFA1067_2")

data_AFA1067 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1041
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC50/AFA1041.xlsx"
dose <- "CC_50.0_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13)
position <- c("Striatum", "Striatum", "Cortex")
id_patient <- c("AFA1041_1", "AFA1041_2", "AFA1041_1")

data_AFA1041 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1042
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC50/AFA1042.xlsx"
dose <- "CC_50.0_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1042_1", "AFA1042_2", "AFA1042_1", "AFA1042_2")

data_AFA1042 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment LW854
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC50/LW854_stri_pfc.xls"
dose <- "CC_50.0_mumol/kg"
rows_to_read <- list(c(1, 20), c(1, 20), c(24, 43), c(24, 43))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("LW854_1", "LW854_2", "LW854_1", "LW854_2")

data_LW854 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient, n_samples = 16)

# -------------------------------------------------------------------------------
# Experiment AFA1052
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC150/AFA1052.xlsx"
dose <- "CC_150.0_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1052_1", "AFA1052_2", "AFA1052_1", "AFA1052_2")

data_AFA1052 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1053
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/CC150/AFA1053.xlsx"
dose <- "CC_150.0_mumol/kg"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1053_1", "AFA1053_2", "AFA1053_1", "AFA1053_2")

data_AFA1053 <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment AFA1049NaCl
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/AFA1049NaCl.xlsx"
dose <- "NaCl"
rows_to_read <- list(c(1, 18))
cols_to_read <- list(1:13)
position <- c("Striatum")
id_patient <- c("AFA1049_1")

data_AFA1049_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient, n_samples = 14)

# -------------------------------------------------------------------------------
# Experiment AFA1076
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/AFA1076NaCl.xlsx"
dose <- "NaCl"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("AFA1076_1", "AFA1076_2", "AFA1076_1", "AFA1076_2")

data_AFA1076_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment BML805
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/BML805stri_pfc.xls"
dose <- "NaCl"
rows_to_read <- list(c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(15:27, 1:13, 15:27)
position <- c("Striatum", "Cortex", "Cortex")
id_patient <- c("BML805_2", "BML805_1", "BML805_2")

data_BML805_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment BML807
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/BML807stri_pfc.xls"
dose <- "NaCl"
rows_to_read <- list(c(1, 19), c(24, 42))
cols_to_read <- list(1:13, 1:13)
position <- c("Striatum", "Cortex")
id_patient <- c("BML807_1", "BML807_1")

data_BML807_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment BML894
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/BML894.xlsx"
dose <- "NaCl"
rows_to_read <- list(c(1, 19), c(1, 19), c(24, 42), c(24, 42))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("BML894_1", "BML894_2", "BML894_1", "BML894_2")

data_BML894_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# -------------------------------------------------------------------------------
# Experiment BML1047
# -------------------------------------------------------------------------------
path_data <- "../../Data/Raw_data/Controlls_NaCl/BML1047stri_pfc.xlsx"
dose <- "NaCl"
rows_to_read <- list(c(1, 19), c(1, 19), c(23, 41), c(23, 41))
cols_to_read <- list(1:13, 15:27, 1:13, 15:27)
position <- c("Striatum", "Striatum", "Cortex", "Cortex")
id_patient <- c("BML1047_1", "BML1047_2", "BML1047_1", "BML1047_2")

data_BML1047_c <- read_one_experiment(path_data, rows_to_read, cols_to_read, dose, position, id_patient)

# Cleaning the workspace
rm(path_data, dose, rows_to_read, cols_to_read, position, id_patient)

# -------------------------------------------------------------------------------
# Aggregate the data 
# -------------------------------------------------------------------------------
control_data <- data_AFA1049_c %>% 
  bind_rows(data_AFA1076_c) %>% 
  bind_rows(data_BML805_c) %>% 
  bind_rows(data_BML807_c) %>% 
  bind_rows(data_BML894_c) %>%
  bind_rows(data_BML1047_c) %>%
  mutate(Type = "Control") %>%
  mutate_if(is.character, as.factor) %>%
  select(id, position, sample, Type, time, everything())

case_data <- data_AFA1024 %>%
  bind_rows(data_AFA1041) %>%
  bind_rows(data_AFA1042) %>%
  bind_rows(data_AFA1052) %>%
  bind_rows(data_AFA1053) %>%
  bind_rows(data_AFA1062) %>%
  bind_rows(data_AFA1063) %>%
  bind_rows(data_AFA1067) %>%
  bind_rows(data_AFA1068) %>%
  bind_rows(data_LW854) %>%
  mutate(Type = "Case") %>%
  select(id, position, sample, Type, time, everything())

# Combind the data into one data-set 
data_tot <- case_data %>% 
  bind_rows(control_data) %>%
  mutate_if(is.character, as.factor)

# Save the data to the intermediate folder 
path_save <- "../../Intermediate/Data_tidy.csv"
write_csv(data_tot, path_save)
  