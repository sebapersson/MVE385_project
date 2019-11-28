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
  select(unique_id,time,NOA) %>%
  mutate(time=paste0('noa_', time)) %>%
  spread(time, NOA) %>%
  select(unique_id, noa_0, noa_20, noa_40, noa_60, noa_80, noa_100,
        noa_120, noa_140, noa_160, noa_180, noa_200,
        noa_220, noa_240, noa_260, noa_280, noa_300)

da_wide <- data_tidy
da_wide <- da_wide %>%
  select(unique_id,time,DA) %>%
  mutate(time=paste0('da_', time)) %>%
  spread(time, DA) %>%
  select(unique_id, da_0, da_20, da_40, da_60, da_80, da_100,
         da_120, da_140, da_160, da_180, da_200,
         da_220, da_240, da_260, da_280, da_300)

mt_3_wide <- data_tidy
mt_3_wide <- mt_3_wide %>%
  select(unique_id,time,MT_3) %>%
  mutate(time=paste0('mt_3_', time)) %>%
  spread(time, MT_3) %>%
  select(unique_id, mt_3_0, mt_3_20, mt_3_40, mt_3_60, mt_3_80, mt_3_100,
         mt_3_120, mt_3_140, mt_3_160, mt_3_180, mt_3_200,
         mt_3_220, mt_3_240, mt_3_260, mt_3_280, mt_3_300)


# View rows with any NAs from 20, 40, ..., 280 minutes

noa_wide[rowSums(is.na(noa_wide[,seq(3,16)])) > 0,]
da_wide[rowSums(is.na(da_wide[,seq(3,16)])) > 0,]
mt_3_wide[rowSums(is.na(mt_3_wide[,seq(3,16)])) > 0,]
