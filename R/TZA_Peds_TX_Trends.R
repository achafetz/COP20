## PROJECT: COP20 TZA
## AUTHOR:  A.Chafetz | USAID
## LICENSE: MIT
## PURPOSE: visualize trends in TX for peds
## NOTE:    adapted from FY19AgencySelfAssessments/FY19Q4_TZA_TX_Comparison.R
## DATE:    2020-02-09
## UPDATED: 2020-02-17


# DEPENDENCIES ------------------------------------------------------------

  library(tidyverse)
  library(ICPIutilities)
  library(scales)
  library(extrafont)
  library(patchwork)


# IMPORT ------------------------------------------------------------------

  path <-"data/PEPFAR-Data-Genie-OUByIMs-2020-02-17.zip"
  
  df_genie <- path %>% 
    read_msd(remove_txt = FALSE) %>% 
    filter(fiscal_year == 2020)
  
  df_msd <- list.files("~/Data", "OU_IM", full.names = TRUE) %>% 
    read_rds() %>% 
    filter(operatingunit == "Tanzania")

# MUNGE -------------------------------------------------------------------

  df_tza <- bind_rows(df_msd, df_genie)

  #filter for partners and peds
    df_tx <- df_tza %>% 
      # mutate(partner = case_when(str_detect(primepartner, "DELOITTE") ~ "Deloitte",
      #                            str_detect(primepartner, "Elizabeth Glaser") ~ "EGPAF")) %>% 
      filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
             ((fiscal_year == 2017 & standardizeddisaggregate == "MostCompleteAgeDisagg") |
             (fiscal_year >= 2018 & standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age Aggregated/Sex/HIVStatus"))),
            #  fundingagency == "USAID",
            # !is.na(partner),
            ageasentered %in% c("<01", "01-09", "01-04", "05-09", "10-14", "<15"))
    
  #aggregate and reshape
    df_tx <- df_tx %>% 
      group_by(indicator, fiscal_year) %>% 
      summarise_at(vars(starts_with("qtr")), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      reshape_msd(clean = TRUE) %>% 
      arrange(indicator, period) 

  #add elements for plotting
    df_viz <- df_tx %>% 
      mutate(pd_fill = case_when(period == "FY20Q1" ~ "z_now",
                                 str_detect(period, "19") ~ "y_rest of fy19",
                                 TRUE ~ "x_earlier"),
             indicator = paste(indicator, "<15"))



# PLOT FUNCTION -----------------------------------------------------------


    plot_tx <- function(partner_sel = NULL){
      
      
      theme_set(theme_minimal(base_family = "Gill Sans MT"))
      
      save_name <- "out/plots/TZA_Peds_TxTrends.png"
      
      if(!is.null(partner_sel)){
        df_viz <- filter(df_viz <-  partner == partner_sel) 
        save_name <- paste0("out/plots/TZA_Peds_TxTrends_", partner_sel,".png")
      }
      
      v_curr <- df_viz %>% 
        filter(indicator == "TX_CURR <15") %>% 
        ggplot(aes(period, val, fill = pd_fill)) +
        geom_col() +
        geom_text(aes(label = comma(val,1)),
                  family = "Calibri Light", color = "gray30", size = 3,
                  vjust = -1) +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values = c("#739bcc", "#335B8E", "#26456a")) +
        scale_y_continuous(labels = comma) +
        scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                                    "FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "",
                                    "FY20Q1")) +
        facet_wrap(indicator ~ ., scales = "free_y") +
        theme_light() +
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.text = element_text(size = 16),
              legend.position = "none")
      
      v_new <- df_viz %>% 
        filter(indicator %in% c("TX_NEW <15", "TX_NET_NEW <15")) %>% 
        ggplot(aes(period, val, fill = pd_fill)) +
        geom_col() +
        geom_blank(aes(y = 1.2 * val)) +
        geom_text(aes(label = comma(val, 1),
                      vjust = ifelse(val <0, 1, -.8)), size = 3,
                  family = "Calibri Light", color = "gray30") +
        geom_hline(yintercept = 0) +
        labs(x = NULL, y = NULL) +
        scale_fill_manual(values =  c("#739bcc", "#335B8E", "#26456a")) +
        scale_y_continuous(labels = comma)+
        scale_x_discrete(labels = c("FY17Q1", "", "FY17Q3", "",
                                    "FY18Q1", "", "FY18Q3", "",
                                    "FY19Q1", "", "FY19Q3", "",
                                    "FY20Q1")) +
        facet_wrap(. ~ indicator, ncol = 1) +
        theme_light() +
        theme(axis.ticks = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.text = element_text(size = 16),
              legend.position = "none")
      
      
      v_out <- (v_curr + v_new) + 
        plot_annotation(title = paste(ifelse(!is.null(partner_sel), partner_sel, "Tanzania"), 'Peds Treatment Trends'),
                        caption = "Source: FY20Q1 DATIM Genie [2020-02-17]",
                        theme = theme(title = element_text(size = 18, face = "bold"),
                                      plot.caption = element_text(color = "gray30", size = 10)))
      
      
      ggsave(save_name, plot = v_out,
             height = 5.63, width = 10,dpi = 300)
      
      return(v_out)
    }


# PLOT AND EXPLORT --------------------------------------------------------
  
  plot_tx()
    
  plot_tx("Deloitte")
  
  plot_tx("EGPAF")

