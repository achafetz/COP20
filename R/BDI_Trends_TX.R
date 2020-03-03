## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province trends in treatment
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

  df_tx <- df_bdi %>% 
    filter(snu1 %in% snu_sel,
           indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, fiscal_year, indicator) %>% 
    summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    reshape_msd(clean = TRUE) %>% 
    select(-period_type) %>% 
    mutate(snu1 = factor(snu1, snu_sel)) %>% 
    arrange(snu1, indicator, period) %>% 
    mutate(val = ifelse(indicator == "TX_NET_NEW" & val > 3000, NA, val))
  
# PLOT --------------------------------------------------------------------
  
  df_tx %>% 
    ggplot(aes(period, val, group = snu1, color = indicator)) +
    geom_hline(yintercept = 0) +
    geom_path(size = 1) +
    geom_point(size = 3, na.rm = TRUE) +
    facet_grid(indicator ~ snu1, scales = "free_y", switch = "y") +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(labels = c("FY18", "", "", "",
                                "FY19", "", "", "",
                                "FY20")) +
    scale_color_manual(values = c(pal[3], pal[4], pal[5]))+
    labs(x = NULL, y = NULL,
         title = "BURUNDI | TREATMENT TRENDS",
         subtitle = "COP18 provinces",
         caption = "Source: FY20Q1i MSD") +
    theme(strip.placement = "outside",
          legend.position = "none",
          plot.caption = element_text(color = "gray30"),
          plot.title = element_text(family = "Calibri", face = "bold", size = 14),
          strip.text = element_text(family = "Calibri", face = "bold", size = 13))
  
  ggsave("out/plots/BDI_Trends_TX.png", dpi = 300,
         width = 10, height = 5.66)      
  