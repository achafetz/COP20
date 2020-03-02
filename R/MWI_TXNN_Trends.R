## PROJECT:  COP20 MWI
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  Trends in TX_CURR and NET_NEW by partner
## DATE:     2020-03-02
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(Wavelength)
library(lubridate)
library(ICPIutilities)
library(scales)
library(extrafont)
library(patchwork)



# IMPORT ------------------------------------------------------------------

path <- "data/TX_Trend_Data_02232020.xlsx"

df_dha <- read_excel(path, sheet = "ART Cohort")

df_msd <- read_excel(path, sheet = "MSD-TX-Age-Sex")



# MUNGE -------------------------------------------------------------------


##DHA

  #clean names
    df_dha <- df_dha %>% 
      rename(primepartner = `prime partner`) %>% 
      rename_all(~str_replace_all(., " ", "_"))
  
  #create fy period
    df_dha <- df_dha %>% 
      mutate(date = yq(cy_quarter),
             period = date %>% 
               quarter(with_year = TRUE, fiscal_start = 10) %>%
               as.character(.) %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q"))
  
  #filter to key periods and columns
    df_dha <- df_dha %>% 
      filter(date >= "2018-04-01",
             date < "2019-07-01") %>% #FY18Q3-FY19Q3
      select(primepartner, orgunit, period, TX_CURR, TX_NEW)


##MSD
  
  #filter to total numerator
    df_msd <- df_msd %>% 
      filter(standardizeddisaggregate == "Total Numerator",
             psnu != "_Military Malawi")
    
  #adjust period 
    df_msd <- df_msd %>% 
      rename(period = fy_qtr) %>% 
      mutate(period = str_replace(period, "20", "FY"))

  #assign partners to each site
    df_sitepartner <- df_dha %>% 
      distinct(orgunit, primepartner)
    
    df_msd <- df_msd %>% 
      rename(orgunit = facilityuid) %>% 
      left_join(df_sitepartner)

  #filter to just FY19Q4 and FY20Q1
    df_msd <- df_msd %>% 
      filter(period %in% c("FY19Q4", "FY20Q1")) %>% 
      select(primepartner, orgunit, period, TX_CURR, TX_NEW)
      
## Combine
    
  #combine DHA + MSD
    df_combo <- bind_rows(df_dha, df_msd)
    
## NET NEW
    
  #calc net new
    df_combo <- df_combo %>% 
      arrange(orgunit, period) %>%
      group_by(orgunit) %>%
      mutate(TX_NET_NEW = TX_CURR - lag(TX_CURR)) %>%
      ungroup() %>% 
      filter(period != "FY18Q3")
    
  #aggregate to mech level
    df_combo <- df_combo %>% 
      group_by(primepartner, period) %>% 
      summarise_at(vars(starts_with("TX")), sum, na.rm = TRUE) %>% 
      ungroup()
    
    #order by TX_CURR value in FY20
    df_combo <- df_combo %>% 
      filter(!is.na(primepartner)) %>% #no match for St Mary's Chisumulu Health Centre
      mutate(fy20q1_tx_curr = case_when(period == "FY20Q1" ~ TX_CURR),
             primepartner = ifelse(primepartner == "Project Concern International", "PCI", primepartner),
             primepartner = fct_reorder(primepartner, fy20q1_tx_curr, sum, na.rm = TRUE, .desc = TRUE))
  

# PLOT --------------------------------------------------------------------

pal <- add_color()

v_curr <- df_combo %>% 
  ggplot(aes(period, TX_CURR)) +
  geom_hline(yintercept = 0) +
  geom_blank(aes(y = TX_CURR * 1.4)) +
  geom_col(fill = pal[1]) +
  geom_text(aes(label = comma(TX_CURR)),
            vjust = -1, color = "gray30",
            family = "Calibri Light", size = 3) +
  facet_grid(primepartner ~ ., switch = "y") +
  #expand_limits(y = c(-1200, 400000)) +
  labs(x = NULL, y = NULL,
       title = "TX_CURR") +
  theme_minimal() +
  theme(text = element_text(family = "Calibri Light"),
        title = element_text(family = "Calibri", face = "bold"),
        axis.text.y = element_blank(),
        panel.grid = element_blank())


v_nn <- df_combo %>% 
  ggplot(aes(period, TX_NET_NEW)) +
  geom_hline(yintercept = 0) +
  geom_blank(aes(y = TX_NET_NEW * 1.6)) +
  geom_col(fill = pal[2]) +
  geom_text(aes(label = comma(TX_NET_NEW, 1),
                vjust = ifelse(TX_NET_NEW < 0, 1, -1)),
            color = "gray30", size = 3,
            family = "Calibri Light") +
  facet_grid(primepartner ~ .) +
  #expand_limits(y = c(-1200, 400000)) +
  labs(x = NULL, y = NULL,
       title = "TX_NET_NEW") +
  theme_minimal() +
  theme(text = element_text(family = "Calibri Light"),
        title = element_text(family = "Calibri", face = "bold"),
        axis.text.y = element_blank(),
        panel.grid = element_blank())


v_curr + v_nn +
  plot_annotation(title = 'TRACKING PARTNER TREATMENT TRENDS',
                  subtitle = "FY18Q4-FY20Q1 Malawi",
                  caption = "Note: NET_NEW calculated by site, agnostic to partner 
                  Source: MWI DHA (before FY19Q4) + MSD (FY19Q4 + FY20Q1)",
                  theme = theme(title = element_text(family = "Calibri", size = 13, face = "bold"),
                                plot.subtitle = element_text(family = "Calibri", size = 11),
                                plot.caption = element_text(family = "Calibri Light", color = "gray30",
                                                            size = 9)))
ggsave("out/plots/MWI_TXTrends.png",
       dpi = 330, width = 10, height = 7)
