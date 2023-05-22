# PROJECT: FY23Q3 Review -- KP Summary
# PURPOSE: Munge and Analysis of KP data
# AUTHOR: Tim Essam | SI
# REF ID:   98cdc5c1
# LICENSE: MIT
# DATE: 2023-05-17
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    
    
  # SI specific paths/functions  
    load_secrets()
    
  # Get old PSNU MSD to link quarters across time
    msd_path_old <- return_latest(folderpath = merdata,
                                  pattern = "PSNU_IM_FY20-23.*Zambia.zip")
    
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "98cdc5c1"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  df_genie <- read_msd(file_path) %>% 
      fix_mech_names() %>% 
      clean_agency() %>% 
      swap_targets() 
    
  df_msd <- read_psd(msd_path_old) %>% 
    fix_mech_names() %>% 
    clean_agency() %>% 
    swap_targets() 

# MUNGE ============================================================================
 
    df_cxca <- df_genie %>% 
    bind_rows(., df_msd) %>% 
      filter(funding_agency == "USAID",
             str_detect(indicator, "CXCA"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year <= metadata$curr_fy)
    
    df_cxca <- df_cxca %>% 
      group_by(fiscal_year, funding_agency, indicator, operatingunit) %>% 
      summarise(across(matches("qtr2|qtr4"), sum, na.rm = TRUE)) %>% 
      pivot_longer(cols = matches("qtr"),
                   names_to = "period") %>% 
      spread(indicator, value) %>% 
      mutate(tx_rate = CXCA_TX / CXCA_SCRN_POS,
             period = paste0("FY", str_sub(fiscal_year, 3, 4), "Q", str_sub(period, 4))) %>% 
      group_by(operatingunit) %>% 
      mutate(order_var = sum(CXCA_SCRN, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(ou_order = fct_reorder(operatingunit, order_var, .desc = T)) %>% 
      filter(period != max(period))
    
# VIZ ============================================================================

     si_blue <- "#4974a5"
      nudge_space  <-  0.15
      
      a <-  df_cxca %>% 
        mutate(positivity = CXCA_SCRN_POS/CXCA_SCRN) %>% 
        ggplot(aes(x = period)) +
        geom_col(aes(y = CXCA_SCRN), fill = golden_sand_light,
                 width = 0.6) +
        geom_col(aes(y = CXCA_SCRN_POS), fill = golden_sand, width = 0.5, 
                 position = position_nudge(x = nudge_space)) + 
        geom_col(aes(y = CXCA_TX), fill = si_blue, width = 0.5, position = position_nudge(x = -nudge_space)) +
        geom_text(aes(y = CXCA_SCRN_POS, label = percent(positivity, 1)),
                  size = 12/.pt, family = "Source Sans Pro SemiBold", vjust = -0.5, hjust = -0.5) +
        si_style_xline() +
        geom_hline(yintercept = seq(2.5e4, 1e5, 2.5e4), color = "white", size = 0.5) +
        scale_y_continuous(position = "right", labels = label_number()) +
        theme(strip.text = element_blank()) +
        coord_cartesian(expand = F) +
        labs(x = NULL, y = NULL)
      a
      
      b <- df_cxca %>% 
        ggplot(aes(x = period, y = tx_rate, group = operatingunit)) +
        geom_area(aes(y = 1), fill = "#bdcee2", alpha = 0.5)+
        geom_area(fill = si_blue, alpha = 0.5)+
        geom_line(color = si_blue, size = 2) +
        # geom_textpath(aes(label = "Treatment rate"), hjust = 0.95, vjust = -1, include_line = F)+
        geom_text(aes(label = "Treatment rate", y = 1, x = "FY21Q2"), vjust = -1,
                  family = "Source Sans Pro", size = 12/.pt) +
        geom_hline(yintercept = 1, size = 0.25, linetype = "dotted") +
        geom_label(aes(label = percent(tx_rate, 1)), size = 12/.pt, family = "Source Sans Pro SemiBold") +
        si_style_xline() +
        # coord_cartesian(expand = F) +
        scale_x_discrete(expand = expansion(add = 0.25))+
        scale_y_continuous(expand = expansion(mult = 0), lim = c(0, 1.5)) +
        labs(x = NULL, y = NULL) +
        theme(axis.text = element_blank()) +
        labs(caption = glue("Source: {metadata$msd_source}}
                     US Agency for International Development"))
      
      a / b + plot_layout(heights = c(6, 2))

      si_save(glue("Graphics/{metadata$curr_pd}_CXCA-screening-trends.svg"), scale = 1.2)  
      