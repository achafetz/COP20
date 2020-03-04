## PROJECT:  COP20 TZA
## AUTHOR:   A.CHAFTEZ | USAID
## LICENSE:  MIT
## PURPOSE:  assess VL coverage and suppression for pregnant women
## DATE:     2020-03-04
## UPDATED:  



# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)


# IMPORT ------------------------------------------------------------------


  df_tza <- list.files("~/Data", "OU_IM_FY18", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")


# MUNGE -------------------------------------------------------------------

  #filter to key vars
    df_pmtct <- df_tza %>% 
      filter((indicator == "PMTCT_ART" & 
                standardizeddisaggregate == "Age/NewExistingArt/Sex/HIVStatus") |
               (indicator == "TX_PVLS" &
                  standardizeddisaggregate == "PregnantOrBreastfeeding/Indication/HIVStatus" &
                  str_detect(otherdisaggregate, "Pregnant")))%>% 
      mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) 
    
  #aggregate & reshape & order vars
    df_pmtct <- df_pmtct %>% 
      group_by(operatingunit, fiscal_year, indicator) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      select(-period_type) %>% 
      spread(indicator, val) %>% 
      select(operatingunit, period, PMTCT_ART, TX_PVLS_D, TX_PVLS) %>% 
      arrange(period) 
  
  #cal VL
    df_pmtct <- df_pmtct %>% 
      mutate(VL_Cov = TX_PVLS_D/lag(PMTCT_ART, 2),
             VL_Supp = TX_PVLS / TX_PVLS_D)

