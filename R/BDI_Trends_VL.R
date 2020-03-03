## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province trends in VL
## DATE:     2020-03-03
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(extrafont)
library(scales)



# GLOBAL OPTIONS ----------------------------------------------------------

snu_sel <- c("Bujumbura Mairie", "Gitega", "Kirundo", "Kayanza", "Ngozi", "Bujumbura")

pal <- add_color("tidepools")

theme_set(theme_minimal(base_size = 11, base_family = "Calibri Light"))

# IMPORT ------------------------------------------------------------------

df_bdi <- list.files("~/Data", "PSNU_IM_FY18", full.names = TRUE) %>% 
  read_rds() %>% 
  filter(operatingunit == "Burundi")


# MUNGE -------------------------------------------------------------------

  df_vl <- df_bdi %>% 
    filter(snu1 %in% snu_sel,
           indicator %in% c("TX_CURR", "TX_PVLS"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
    group_by(snu1, fiscal_year, indicator) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    select(-period_type) %>% 
    mutate(snu1 = factor(snu1, snu_sel)) %>% 
    arrange(snu1, indicator, period)
  
  df_vl <- df_vl %>% 
    spread(indicator, val) %>% 
    group_by(snu1) %>% 
    mutate(`VL Coverage (%)` = (TX_PVLS_D / lag(TX_CURR, 2)) * 100) %>% 
    ungroup() %>%
    mutate(`VL Suppression (%)` = (TX_PVLS / TX_PVLS_D)*100) %>% 
    select(-TX_PVLS_D, -TX_PVLS) %>% 
    gather(indicator, val, -snu1, -period)
  

# PLOT --------------------------------------------------------------------

df_vl %>% 
  ggplot(aes(period, val, group = snu1, color = indicator)) +
  geom_hline(yintercept = 0) +
  geom_path(size = 1) +
  geom_point(size = 3, na.rm = TRUE) +
  facet_grid(indicator ~ snu1, scales = "free_y", switch = "y") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("FY18", "", "", "",
                              "FY19", "", "", "",
                              "FY20")) +
  scale_color_manual(values = c(pal[3], pal[4], pal[5], pal[6]))+
  labs(x = NULL, y = NULL,
       title = "BURUNDI | VIRAL LOAD TRENDS",
       subtitle = "COP18 provinces",
       caption = "Note: VL Coverage = TX_PVLS_D / TX_CURR_2Q_prior
       VL Suppression = TX_PVLS / TX_PVLS_D
         Source: FY20Q1i MSD") +
  theme(strip.placement = "outside",
        legend.position = "none",
        plot.caption = element_text(color = "gray30"),
        plot.title = element_text(family = "Calibri", face = "bold", size = 14),
        strip.text = element_text(family = "Calibri", face = "bold", size = 13))

ggsave("out/plots/BDI_Trends_VL.png", dpi = 300,
       width = 10, height = 5.66)      
