## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province trends in testing
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

##Total trends
  #filter to hts
    df_hts <- df_bdi %>% 
      filter(snu1 %in% snu_sel,
             indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(snu1, fiscal_year, indicator) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      mutate(snu1 = factor(snu1, snu_sel)) %>% 
      arrange(snu1, indicator, period)
    
  #gen positivity
    df_hts <- df_hts %>% 
      spread(indicator, val) %>% 
      mutate(`Positivity (%)` = HTS_TST_POS/HTS_TST *100) %>% 
      gather(indicator, val, -snu1, -period)
  
  #create label
    df_hts <- df_hts %>% 
      mutate(lab = ifelse(indicator == "Positivity (%)", percent(val, 1), comma(val)))

##Modality trends
    
    #filter to hts
    df_hts_mod <- df_bdi %>% 
      filter(snu1 %in% snu_sel,
             indicator %in% c("HTS_TST", "HTS_TST_POS"),
             standardizeddisaggregate == "Modality/Age/Sex/Result") %>% 
      group_by(snu1, modality, fiscal_year, indicator) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      mutate(snu1 = factor(snu1, snu_sel)) %>% 
      arrange(snu1, indicator, modality, period)
    
    df_mod_map <- df_hts_mod %>% 
      filter(indicator == "HTS_TST",
             str_detect(period, "FY19")) %>% 
      count(modality, wt = val, sort = TRUE) %>% 
      mutate(share = n/sum(n),
             mods = ifelse(share > .02, modality, "Other Mods")) %>% 
      select(modality, mods)
    
    mod_order <- df_mod_map %>% pull(mods) %>% unique()
    
    #sum to other
    df_hts_mod <- df_hts_mod %>% 
      left_join(df_mod_map) %>% 
      mutate(modality = mods) %>%
      group_by(snu1, modality, period, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(modality = factor(modality, mod_order))
    
    #gen positivity
    df_hts_mod <- df_hts_mod %>% 
      spread(indicator, val, fill = 0) %>% 
      mutate(`Positivity` = HTS_TST_POS/HTS_TST) %>% 
      gather(indicator, val, -snu1, -modality, -period)

# PLOT --------------------------------------------------------------------

    df_hts %>% 
      ggplot(aes(period, val, group = snu1, color = indicator)) +
      geom_hline(yintercept = 0) +
      geom_path(size = 1) +
      geom_point(size = 3) +
      facet_grid(indicator ~ snu1, scales = "free_y", switch = "y") +
      scale_y_continuous(labels = comma) +
      scale_x_discrete(labels = c("FY18", "", "", "",
                                  "FY19", "", "", "",
                                  "FY20")) +
      scale_color_manual(values = c(pal[3], pal[4], pal[5]))+
      labs(x = NULL, y = NULL,
           title = "BURUNDI | TESTING TRENDS",
           subtitle = "COP18 provinces",
           caption = "Source: FY20Q1i MSD") +
      theme(strip.placement = "outside",
            legend.position = "none",
            plot.caption = element_text(color = "gray30"),
            plot.title = element_text(family = "Calibri", face = "bold", size = 14),
            strip.text = element_text(family = "Calibri", face = "bold", size = 13))

    ggsave("out/plots/BDI_Trends_HTS.png", dpi = 300,
           width = 10, height = 5.66)      
    
    


    df_hts_mod %>% 
      filter(indicator == "Positivity") %>% 
      ggplot(aes(period, val, group = snu1, color = indicator)) +
      geom_hline(yintercept = 0) +
      geom_path(size = 1) +
      geom_point(size = 3) +
      facet_grid(modality ~ snu1, scales = "free_y", switch = "y") +
      scale_y_continuous(labels = percent_format(1)) +
      scale_x_discrete(labels = c("FY18", "", "", "",
                                  "FY19", "", "", "",
                                  "FY20")) +
      scale_color_manual(values = c(pal[5]))+
      labs(x = NULL, y = NULL,
           title = "BURUNDI | MODALITY POSITIVITY TRENDS",
           subtitle = "COP18 provinces",
           caption = "Note: Limited to the modalities with more than 1% of over all tests in FY19
           Source: FY20Q1i MSD") +
      theme(strip.placement = "outside",
            legend.position = "none",
            plot.caption = element_text(color = "gray30"),
            plot.title = element_text(family = "Calibri", face = "bold", size = 14),
            strip.text.x = element_text(family = "Calibri", face = "bold", size = 13),
            strip.text.y = element_text(family = "Calibri", face = "bold", size = 9))
    
    
    
    ggsave("out/plots/BDI_Trends_HTS_modalities.png", dpi = 300,
           width = 10, height = 5.66) 
    