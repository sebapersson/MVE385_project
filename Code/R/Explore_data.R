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

DOSE_START <- 120

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

# ------------------------------------------------------------------------------------------------
# Dopamin, case vs control dosages, 150 
# ------------------------------------------------------------------------------------------------

da_150_a <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, DA, unique_id)  %>% 
  filter(position == "Cortex")

da_150_s <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, DA, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(da_150_a, aes(time, DA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "Dopamin 150 a") + 
  my_theme

p2 <- ggplot(da_150_s, aes(time, DA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "Dopamin 150 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)


# ------------------------------------------------------------------------------------------------
# Dopamin, case vs control dosages 
# ------------------------------------------------------------------------------------------------

da_50_a <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, DA, unique_id)  %>% 
  filter(position == "Cortex")

da_50_s <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, DA, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(da_50_a, aes(time, DA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 20) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "Dopamin 50 a") + 
  my_theme

p2 <- ggplot(da_50_s, aes(time, DA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 20) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "Dopamin 50 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)

# Significant difference high dosage for cortex (bencmark test)
# No appearent difference regarding s-region 

# ------------------------------------------------------------------------------------------------
# NOA, case vs control dosages, 150 
# ------------------------------------------------------------------------------------------------

noa_150_a <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, NOA, unique_id)  %>% 
  filter(position == "Cortex")

noa_150_s <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, NOA, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(noa_150_a, aes(time, NOA, color = Type)) + 
  geom_line(aes(group = id)) +
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "NOA 150 a") + 
  my_theme

p2 <- ggplot(noa_150_s, aes(time, NOA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "NOA 150 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)


# ------------------------------------------------------------------------------------------------
# NOA, case vs control dosages, 50 
# ------------------------------------------------------------------------------------------------

noa_50_a <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, NOA, unique_id)  %>% 
  filter(position == "Cortex")

noa_50_s <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, NOA, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(noa_50_a, aes(time, NOA, color = Type)) + 
  geom_line(aes(group = id)) +
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "NOA 50 a") + 
  my_theme

p2 <- ggplot(noa_50_s, aes(time, NOA, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "NOA 50 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)

# cortex significant, bigger scale than s 
# s-region nt significant

# ------------------------------------------------------------------------------------------------
# HT-5, case vs control dosages, 150 
# ------------------------------------------------------------------------------------------------

ht5_150_a <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, HT_5, unique_id)  %>% 
  filter(position == "Cortex")

ht5_150_s <- data_tidy %>% 
  filter(dose == "CC_150.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, HT_5, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(ht5_150_a, aes(time, HT_5, color = Type)) + 
  geom_line(aes(group = id)) +
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "HT-5 150 a") + 
  my_theme

p2 <- ggplot(ht5_150_s, aes(time, HT_5, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = 2, linetype = DOSE_START) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "HT-5 150 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)

# signifcant cortex, same scale :) 
# maybe significant for s-region 

# ------------------------------------------------------------------------------------------------
# HT-5, case vs control dosages, 150 
# ------------------------------------------------------------------------------------------------

ht5_50_a <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, HT_5, unique_id)  %>% 
  filter(position == "Cortex")

ht5_50_s <- data_tidy %>% 
  filter(dose == "CC_50.0_mumol/kg" | dose == "NaCl") %>%
  select(id, position, Type, time, HT_5, unique_id)  %>% 
  filter(position == "Striatum")

p1 <- ggplot(ht5_50_a, aes(time, HT_5, color = Type)) + 
  geom_line(aes(group = id)) +
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "HT-5 50 a") + 
  my_theme

p2 <- ggplot(ht5_50_s, aes(time, HT_5, color = Type)) + 
  geom_line(aes(group = id)) + 
  geom_point() + 
  geom_vline(xintercept = DOSE_START, linetype = 2) + 
  scale_color_manual(values = cbPalette[-1]) + 
  labs(title = "HT-5 50 s") + 
  my_theme

ggpubr::ggarrange(p1, p2, ncol = 2)


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

noa_wide[rowSums(is.na(noa_wide[,seq(3,16)])) > 0,]
da_wide[rowSums(is.na(da_wide[,seq(3,16)])) > 0,]
ht_5_wide[rowSums(is.na(ht_5_wide[,seq(3,16)])) > 0,]
