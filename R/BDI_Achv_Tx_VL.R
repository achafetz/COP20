## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province FY20 VL Suppression
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
  df_vl <- df_bdi %>%
    filter(indicator == "TX_PVLS",
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
           fiscal_year == 2020) %>% 
    mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>% 
    group_by(snu1, fiscal_year, indicator) %>% 
    summarise_at(vars(cumulative), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(snu1 = factor(snu1, snu_order)) %>% 
    arrange(snu1, indicator)
  
  df_vl <- df_vl %>% 
    spread(indicator, cumulative) %>% 
    mutate(vl_suppression = TX_PVLS / TX_PVLS_D) %>% 
    mutate(lab =  percent(vl_suppression, 1))
    
  
  

# PLOT --------------------------------------------------------------------

  ##Total
    df_vl %>% 
      ggplot(aes(snu1, vl_suppression)) +
      #geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_point(size = 4, color = pal[3], na.rm = TRUE) +
      geom_text(aes(label = lab), color = "gray30", family = "Calibri Light",
                hjust = -.6, size = 3, na.rm = TRUE) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(1)) +
      labs(x = NULL, y = NULL, color = NULL,
           title = "BURUNDI | VL SUPPRESSION",
           subtitle = "FY20Q1",
           caption = "Note:  VL Suppression = TX_PVLS / TX_PVLS_D
           Source: FY20Q1i MSD") +
      theme(strip.placement = "outside",
            legend.position = "none",
            plot.caption = element_text(color = "gray30"),
            plot.title = element_text(family = "Calibri", face = "bold", size = 14),
            strip.text = element_text(family = "Calibri", face = "bold", size = 13))
  
  
  ggsave("out/plots/BDI_Achv_VLSuppression.png", dpi = 300,
         width = 10, height = 5.66)      
  
 