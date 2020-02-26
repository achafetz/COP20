## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: recalculating NET NEW
## NOTE:    building off work from Fy19AgencySelfAssessments/Fy19Q4i_TZA_NET_NEW_Adjustment.R
## DATE:    2020-02-23
## UPDATED: 2020-02-26



# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(ICPIutilities)
library(scales)
library(extrafont)

# API ---------------------------------------------------------------------

  #DATIM Information
    myuser <- ""
    baseurl <- "https://www.datim.org/"
  
  #API url 
  url <- 
    paste0(baseurl,
      "api/29/analytics.json?",
      "dimension=ou:LEVEL-7;mdXu6iCbn2G&", 
      "dimension=bw8KHXzxd9i:FPUgmtt8HRi;NLV6dy7BE2O&", #Funding Agency - CDC and USAID
      "dimension=SH885jaRe0o&", #Funding Mechanism
      "dimension=pe:2017Q3;2017Q4;2018Q1;2018Q2;2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4&",
      "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area: TX_CURR
      "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results: Results
      "dimension=RUkVjD3BsS1:PE5QVF0w4xj&",  #Top Level Numerator
      "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
    )
  
  
  #pull site TX_CURR data
    df_site_tx <- get_datim_targets(url, myuser, mypwd(myuser))


# FUNCTION - AGGREGATE BY -------------------------------------------------

  agg_by <- function(df, ...){
    df_agg <- df %>% 
      group_by_at(c(vars(..., period))) %>% 
      summarise_if(is.double, sum, na.rm = TRUE) %>%
      ungroup() 
    
    df_agg <- arrange_at(df_agg, vars(..., period))
    
    
    df_agg_nn <- df_agg %>%
      group_by_at(vars(...)) %>%
      mutate(tx_net_new = tx_curr - lag(tx_curr)) %>%
      ungroup() %>%
      filter(period != "FY17Q4")
    
    return(df_agg_nn)
  }

# FUNCTION - AGGREGATE REASSIGNING BY -------------------------------------

    agg_reassign_by <- function(df, ...){
      
      df_site_assignments <- df %>%
        distinct(period, orgunituid, fundingagency, mech_code, mech_name, primepartner) #%>% 
      #rename_at(vars(fundingagency, mech_code, mech_name, primepartner), ~paste0(., "_adj"))
      
      df_nn <- df %>% 
        select(-c(fundingagency, mech_code, mech_name, primepartner, sitename)) %>% 
        arrange(orgunituid, period) %>% 
        group_by(orgunituid) %>%
        mutate(tx_net_new = tx_curr - lag(tx_curr)) %>%
        ungroup() %>%
        filter(period != "FY17Q4")
      
      df_site_tx_reassigned <- df_nn %>% 
        left_join(df_site_assignments, by = c("orgunituid", "period"))
      
      df_site_tx_reassigned <- df_site_tx_reassigned %>% 
        group_by_at(c(vars(..., period))) %>% 
        summarise_if(is.double, sum, na.rm = TRUE) %>%
        ungroup() 
      
      
      return(df_site_tx_reassigned)
    }
    
# MUNGE -------------------------------------------------------------------

  #keep select vars
    df_site_tx_clean <- df_site_tx %>% 
      select(operatingunit = orglvl_3,
             snu1 = orglvl_4,
             # psnu = orglvl_5,
             sitename = `Organisation unit`,
             orgunituid,
             fundingagency = `Funding Agency`,
             mech_code = `Funding Mechanism`,
             # indicator = `Technical Area`,
             period = `Period`,
             tx_curr = Value) 

  #clean up mech_code and convert period to FY period
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(mech_code = str_extract(mech_code, "[:digit:]+"),
             period = str_replace(period, "to [:alpha:]{3}", "1,") %>% 
               mdy() %>%
               quarter(with_year = TRUE, fiscal_start = 10) %>% 
               as.character %>% 
               str_replace("20", "FY") %>% 
               str_replace("\\.", "Q"))
    
  #clean agencies
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(fundingagency = str_remove(fundingagency, "HHS/"),
             fundingagency = factor(fundingagency, c("CDC","USAID", "DOD")))
    
    
  #add in partner and mech names
    df_site_tx_clean <- rename_official(df_site_tx_clean)
    
  #rename partners
    df_site_tx_clean <- df_site_tx_clean %>% 
      mutate(primepartner = case_when(primepartner == "AMREF HEALTH AFRICA HQ" ~ "AMREF",
                                      primepartner == "ARIEL GLASER PEDIATRIC AIDS H EALTHCARE INITIATIVE" ~ "APAHI",
                                      primepartner == "MANAGEMENT AND DEVELOPMENT FO R HEALTH" ~ "MDH",
                                      primepartner == "TANZANIA HEALTH PROMOTION SUPP ORT (THPS)" ~ "THPS",
                                      primepartner %in% c("Baylor College of Medicine", "BAYLOR COLLEGE OF MEDICINE CH ILDREN FOUNDATION TANZANIA") ~ "Baylor",
                                      primepartner == "DELOITTE CONSULTING LIMITED" ~ "Deloitte",
                                      primepartner == "Elizabeth Glaser Pediatric Aids Foundation" ~ "EGPAF",
                                      primepartner == "JSI Research And Training Institute, INC." ~ "JSI",
                                      primepartner == "Henry M. Jackson Foundation For The Advancement Of Military Medicine, Inc., The" ~ "HJF",
      ))
    
  #order IPs
    ip_order <- df_site_tx_clean %>% 
      filter(period == "FY20Q1") %>% 
      agg_by(primepartner) %>% 
      arrange(desc(tx_curr)) %>% 
      pull(primepartner) %>%
      c(., "THPS")
    
    df_site_tx_clean <- mutate(df_site_tx_clean, primepartner = factor(primepartner, ip_order))

    
# ADJUST SITE TX VIA INHERITING -------------------------------------------
    
    #are there any sites with more than one mechanism working across pds?
    # multi_partner_sites <- df_site_tx_clean %>% 
    #   distinct(period, orgunituid, mech_code) %>% 
    #   count(period, orgunituid) %>% 
    #   filter(n > 1) %>% 
    #   pull(orgunituid)
    # 
    # df_site_tx_clean %>%
    #   filter(orgunituid %in% multi_partner_sites,
    #          period == "FY20Q1") %>%
    #   arrange(orgunituid, mech_code) %>%
    #   View()
    
    #drop dup mech in two sites
    df_site_tx_clean_adj <- df_site_tx_clean %>% 
      filter(mech_code != "12217")

    
# FUNCTION - PLOT TX ------------------------------------------------------

  plot_tx <- function(df, var, title){
    
    if("fundingagency_adj" %in% names(df))
      df <- mutate(df, fundingagency = fundingagency_adj)
    
    df %>% 
      mutate(placeholder = {{var}} * 1.2) %>% 
      ggplot(aes(period, {{var}}, fill = fundingagency)) +
      geom_hline(yintercept = 0) +
      geom_col() +
      geom_blank(aes(y = placeholder)) +
      geom_text(aes(label = comma({{var}}), 
                    vjust = ifelse({{var}} < 0, 1, -1)),
                    size = 3.5,
                color = '#595959',
                family = "Franklin Gothic Medium Cond") +
      scale_y_continuous(label = comma) +
      scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                  "FY19Q1", "", "FY19Q3", "",
                                  "FY20Q1")) +
      scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
      facet_grid(fundingagency ~ ., switch = "y") +
      labs(x = NULL, y = NULL, 
           title = title,
           caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
      theme_minimal() +
      theme(text = element_text(family = "Franklin Gothic Medium Cond"),
            strip.text = element_text(size = 14),
            axis.text.y = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(family = "Franklin Gothic Medium"),
            plot.caption = element_text(color = "#595959"),
            legend.position = "none")
  }
    

    plot_tx_ip <- function(df, var, title){
      
      if("primepartner_adj" %in% names(df))
        df <- mutate(df, primepartner = primepartner_adj)
      
      if("fundingagency_adj" %in% names(df))
        df <- mutate(df, fundingagency = fundingagency_adj)
      
      df %>% 
        mutate(placeholder = {{var}} * 1.4) %>% 
        ggplot(aes(period, {{var}}, fill = fundingagency)) +
        geom_hline(yintercept = 0) +
        geom_col() +
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = comma({{var}}, accuracy = 1), 
                      vjust = ifelse({{var}} < 0, 1, -1)),
                  color = "#595959", size = 3,
                  family = "Franklin Gothic Medium Cond") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "",
                                    "FY20Q1")) +
        scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
        facet_grid(primepartner ~ ., switch = "y") +
        labs(x = NULL, y = NULL, 
             title = title,
             caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Franklin Gothic Medium Cond"),
              axis.text.y = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(family = "Franklin Gothic Medium"),
              plot.caption = element_text(color = "#595959"),
              legend.position = "none")
    }
    
    plot_nn_gr <- function(df, title){
      
      df %>% 
        group_by(fundingagency) %>% 
        mutate(growth = tx_net_new/lag(tx_net_new)) %>% 
        ungroup() %>% 
        mutate(placeholder = growth * 1.2) %>% 
        ggplot(aes(period, growth, group = fundingagency, color = fundingagency)) +
        geom_hline(yintercept = 0) +
        geom_line(size = 1, na.rm = TRUE) +
        geom_point(size = 5, na.rm = TRUE) + 
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = percent(growth, 1), 
                      vjust = ifelse(growth < 0, 2, -1)),
                  size = 3.5,
                  color = '#595959',
                  family = "Franklin Gothic Medium Cond", na.rm = TRUE) +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "",
                                    "FY20Q1")) +
        scale_color_manual(values = c("#335B8E", "#6CA18F")) +
        facet_grid(fundingagency ~ ., switch = "y") +
        labs(x = NULL, y = NULL, 
             title = title,
             caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Franklin Gothic Medium Cond"),
              strip.text = element_text(size = 14),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(family = "Franklin Gothic Medium"),
              plot.caption = element_text(color = "#595959"),
              legend.position = "none")
    }
    


# PLOT TX TRENDS ----------------------------------------------------------

  
  #AGENCY UNADJUSTED
    df_site_tx_clean_adj %>% 
      agg_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_curr, "TX_CURR, unadjusted")
      
    ggsave("out/plots/TZA_NNAdj_TXCURR.png",
           dpi = 330, width = 7, height = 3.6)
    
    df_site_tx_clean_adj %>% 
      agg_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_net_new, "TX_NET_NEW, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_NETNEW.png",
           dpi = 330, width = 7, height = 3.6)
  
    
    df_site_tx_clean_adj %>% 
      agg_by(operatingunit, fundingagency) %>%
      plot_nn_gr("Growth in TX_NET_NEW, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_NETNEW_gr.png",
           dpi = 330, width = 7, height = 3.6)
    
  #AGENCY REASSIGNED
    df_site_tx_clean_adj  %>% 
      agg_reassign_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_net_new, "TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ.png",
           dpi = 330, width = 7, height = 3.6)
    
    df_site_tx_clean_adj %>% 
      agg_reassign_by(operatingunit, fundingagency) %>% 
      plot_nn_gr("Growth in TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ_gr.png",
           dpi = 330, width = 7, height = 3.6)
    
  #PARTNER UNADJUSTED
    df_site_tx_clean_adj %>% 
      agg_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_curr, "TX_CURR, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_TXCURR_IM.png",
           dpi = 330, width = 7, height = 7.11)
    
    df_site_tx_clean_adj %>% 
      agg_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_net_new, "TX_NET_NEW, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_NETNEW_IM.png",
           dpi = 330, width = 7, height = 7.11)

    
    
    #PARTNER REASSIGNED
    df_site_tx_clean_adj %>% 
      agg_reassign_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_net_new, "TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ_IM.png",
           dpi = 330, width = 7, height = 7.11)

    
