## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province FY20 LTFU
## DATE:     2020-03-03
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(extrafont)
library(scales)



# GLOBAL OPTIONS ----------------------------------------------------------

#order provinces by FY20 TX_CURR target
  snu_order <- c("Mwaro", "Cankuzo", "Bubanza", "Ruyigi",
                 "Muramvya", "Bujumbura", "Cibitoke", "Rutana", 
                 "Karusi", "Rumonge", "Bururi", "Makamba", "_Military Burundi", 
                 "Muyinga", "Ngozi", "Kayanza", "Kirundo", "Gitega", "Bujumbura Mairie")
  
    
  pal <- add_color("tidepools")
  
  theme_set(theme_minimal(base_size = 11, base_family = "Calibri Light"))
  

# IMPORT ------------------------------------------------------------------

  df_bdi <- list.files("~/Data", "PSNU_IM_FY18", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Burundi")

# MUNGE -------------------------------------------------------------------
  
##Total
  #filter
  df_ltfu <- df_bdi %>% 
    filter(indicator %in% c("TX_CURR", "TX_ML", "TX_RTT"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2020) %>% 
    group_by(snu1, fiscal_year, indicator) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(snu1 = factor(snu1, snu_order)) %>% 
    arrange(snu1, indicator)
  
  df_ltfu <- df_ltfu %>% 
    spread(indicator, cumulative) %>% 
    mutate(LTFU = TX_ML / TX_CURR,
           RTT = TX_RTT /TX_CURR) %>% 
    select(-starts_with("TX")) %>% 
    gather(indicator, val, -snu1, -fiscal_year) %>% 
    mutate(lab = ifelse(is.na(val), as.character(NA), percent(val, .1)))
    
  
  

# PLOT --------------------------------------------------------------------

  ##Total
    df_ltfu %>% 
      ggplot(aes(snu1, val, color = indicator)) +
      geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_point(size = 4, na.rm = TRUE) +
      geom_text(aes(label = lab, vjust = ifelse(indicator == "LTFU", 1.6, -1)), family = "Calibri Light",
                color = "gray30", size = 3, na.rm = TRUE) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(1)) +
      scale_color_manual(values = c(pal[5], pal[3])) +
      facet_wrap(. ~ indicator) +
      labs(x = NULL, y = NULL, color = NULL,
           title = "BURUNDI | LOST AND RETURNED TO CARE",
           subtitle = "FY20Q1",
           caption = "Note:  LTFU = TX_ML / TX_CURR
           Return to Tx (RTT) = TX_RTT / TX_CURR
           Source: FY20Q1i MSD") +
      theme(strip.placement = "outside",
            legend.position = "none",
            plot.caption = element_text(color = "gray30"),
            plot.title = element_text(family = "Calibri", face = "bold", size = 14),
            strip.text = element_text(family = "Calibri", face = "bold", size = 13))
  
  
  ggsave("out/plots/BDI_Achv_LostAndReturned.png", dpi = 300,
         width = 10, height = 5.66)      
  
 