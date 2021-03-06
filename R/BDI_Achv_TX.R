## PROJECT:  COP20 BDI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  compare province FY20 achievement in treatment
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
  
  ach_goal <- .9
  ach_goal_lower <- ach_goal - .1
  ach_goal_upper <- ach_goal + .1

# IMPORT ------------------------------------------------------------------

  df_bdi <- list.files("~/Data", "PSNU_IM_FY18", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Burundi")

# MUNGE -------------------------------------------------------------------
  
##Total
  #filter
  df_tx <- df_bdi %>% 
    filter(indicator %in% c("TX_CURR"),
           standardizeddisaggregate == "Total Numerator",
           fiscal_year == 2020) %>% 
    group_by(snu1, fiscal_year, indicator) %>% 
    summarise_at(vars(cumulative, targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(achv = cumulative / targets,
          snu1 = factor(snu1, snu_order)
          ) %>% 
    arrange(snu1, indicator)
  
  
  #achievement
  df_tx <- df_tx %>% 
      mutate(achv_lower = ach_goal_lower * targets,
             achv_upper = ach_goal_upper * targets,
             achv_color = case_when(cumulative < achv_lower ~ paste0("<", percent(ach_goal_lower,1)),
                                    cumulative < achv_upper ~ paste(percent(ach_goal_lower,1), "-", percent(ach_goal_upper,1)),
                                    TRUE ~ paste0("+", percent(ach_goal_upper,1))),
             achv_color = factor(achv_color, 
                                 c(paste0("<", percent(ach_goal_lower,1)), 
                                          paste(percent(ach_goal_lower,1), "-", percent(ach_goal_upper,1)),
                                   paste0("+", percent(ach_goal_upper,1)))),
             achv_lab = percent(achv, 1))
    
  

# PLOT --------------------------------------------------------------------

  ##Total
    df_tx %>% 
      ggplot(aes(snu1, cumulative, color = achv_color)) +
      geom_hline(aes(yintercept = 0), color = "gray30") +
      geom_linerange(aes(ymin = achv_lower, ymax = achv_upper), 
                     color = "gray30") +
                     # color = pal[3]) +
      geom_point(size = 4) +
      geom_text(aes(label = achv_lab), color = "gray30", family = "Calibri Light", size = 3, vjust = -1) +
      coord_flip() +
      facet_wrap(. ~ indicator, scales = "free_x") +
      scale_y_continuous(labels = comma) +
      scale_color_manual(values = c(pal[5], pal[3], "gray60")) +
      labs(x = NULL, y = NULL, color = "Q1 Achievement",
           title = "BURUNDI | CURRENT ON TREATMENT ACHIEVEMENT",
           subtitle = "FY20Q1",
           caption = "Source: FY20Q1i MSD") +
      theme(strip.placement = "outside",
            #legend.position = "bottom",
            plot.caption = element_text(color = "gray30"),
            plot.title = element_text(family = "Calibri", face = "bold", size = 14),
            strip.text = element_text(family = "Calibri", face = "bold", size = 13))
  
  
  ggsave("out/plots/BDI_Achv_TX_CURR.png", dpi = 300,
         width = 10, height = 5.66)      
  