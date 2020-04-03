## PROJECT: COP20 MWI
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: validate targets from DP against memo
## DATE:    2020-04-03
## UPDATED: 

library(datapackr)
library(tidyverse)
library(tameDP)
library(janitor)
library(knitr)
library(scales)

path <- "Data/Data Pack_Malawi_20200401_0700.xlsx"

df_dp <- tame_dp(path, FALSE)


glimpse(df_dp)


df_dp %>% 
  group_by(indicator, disaggregate) %>% 
  summarise_at(vars(targets), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  rename(target_21 = targets) %>%
  kable(format.args = list(big.mark = ",", zero.print = FALSE))

df_dp_agg <- df_dp %>% 
  mutate(indicator = case_when(numeratordenom == "D" ~ paste0(indicator, "_D"), 
                               str_detect(indicator, "INDEX") ~ "HTS_INDEX",
                               TRUE ~ indicator)) %>% 
  group_by(indicator, disaggregate) %>% 
  summarise_at(vars(targets), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  rename(target_21 = targets) 
  

df_msd <- list.files('Data', 'OU_IM', full.names = TRUE) %>% 
  read_rds()

df_msd_agg <- df_msd %>% 
  filter(operatingunit == "Malawi",
         fiscal_year == 2020,
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>% 
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
  group_by(indicator) %>% 
  summarise_at(vars(targets), sum, na.rm = TRUE) %>%
  ungroup() %>% 
  rename(target_20 = targets)

df_dp_agg %>% 
  filter(str_detect(disaggregate, "KeyPop", negate = TRUE)) %>% 
  left_join(df_msd_agg) %>% 
  select(-target_21, everything()) %>% 
  mutate(delta = (target_21 - target_20)/target_20,
         delta = ifelse(is.infinite(delta), NA, delta)) %>% 
  arrange(desc(delta)) %>% 
  mutate(delta = percent(delta, 1)) %>% 
  kable(format.args = list(big.mark = ",", zero.print = FALSE))
