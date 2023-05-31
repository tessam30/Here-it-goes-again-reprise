# PROJECT: COP23 Target check
# PURPOSE: Munge and Alysis of COP23 targets
# AUTHOR: Tim Essam | SI
# REF ID:   5d595d32
# LICENSE: MIT
# DATE: 2023-05-26
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
    library(tameDP)
    
    
  # SI specific paths/functions  
    load_secrets()
    data_pack_path <- list.files(path = "../../../Downloads/", pattern = "2023-04-14 Target Setting Tool_5PM \\(1", full.mes = T)
  
  # REF ID for plots
    ref_id <- "5d595d32"
    
  # Functions  
   indic_order <-  c("TX_NEW", "TX_CURR", "TX_PVLS_D", "TX_PVLS", "HTS_SELF", "HTS_TST", "HTS_TST_POS", "HTS_RECENT", "HTS_INDEX",
      "PMTCT_STAT",  "PMTCT_STAT_POS", "PMTCT_ART", "PMTCT_EID", "TB_STAT", "TB_ART", "TB_PREV", "TX_TB", "VMMC_CIRC", 
      "KP_PREV", "PrEP_NEW", "PrEP_CT", "CXCA_SCRN", "PP_PREV", "OVC_SERV", "OVC_HIVSTAT", "GEND_GBV", "AGYW_PREV")

  disags <- c("Age/Sex/DREAMSPrimaryComplete", "Age/Sex/HIVStatus", "ViolenceServiceType", "Age/Sex/Result", "Age/Sex/HIVStatus",
              "Age/Sex", "Modality/Age/Sex/Result", "KeyPop", "No Disagg", "Age/Sex/ProgramStatus", "Age/Sex/Indication/HIVStatus", "Age/Sex/KnownNewResult",
              "Age/Sex/NewExistingArt/HIVStatus", "Age/Sex/HIVStatus", "Age/Sex/TBScreen/NewExistingART/HIVStatus")

# LOAD DATA ============================================================================  

  dp <- tame_dp(data_pack_path)
  
  dp <- tame_dp
  
  dp %>% 
    mutate(trendscoarse = ifelse(ageasentered < 15, "<15", "15+")) %>% 
    count(indicator, standardizeddisaggregate, trendscoarse) %>% prinf()

# MUNGE ============================================================================
  
  tbl1 <- dp %>% 
     mutate(trendscoarse = ifelse(ageasentered < 15, "<15", "15+")) %>% 
    clean_indicator() %>% 
     filter(indicator %in% indic_order, 
            standardizeddisaggregate %in% disags, 
            fiscal_year == 2024) %>% 
    mutate(drop_flag = case_when(
      indicator == "HTS_SELF" & standardizeddisaggregate == "KeyPop" ~ "drop",
      indicator == "PrEP_CT" & standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "KeyPop") ~ "drop",
      indicator == "PrEP_NEW" & standardizeddisaggregate == "KeyPop" ~ "drop",
      indicator == "HTS_RECENT" & standardizeddisaggregate == "KeyPop/HIVStatus" ~ "drop",
      indicator == "HTS_TST" & standardizeddisaggregate == "KeyPop/Result" ~ "drop",
      indicator == "TX_CURR" & standardizeddisaggregate == "KeyPop/HIVStatus" ~ "drop",
      TRUE ~ "keep"
      )
    ) %>% 
    filter(drop_flag == "keep") %>% 
     group_by(indicator, trendscoarse, funding_agency, standardizeddisaggregate, drop_flag
              ) %>% 
     summarize(targets = sum(targets, na.rm = T), .groups = "drop") %>% 
    mutate(Total = sum(targets, na.rm = T), .by = c(indicator, snuprioritization)) %>% 
    spread(trendscoarse, targets) %>% 
    select(-`<NA>`) %>% 
    select(indicator, snuprioritization, `<15`, `15+`, Total) %>% 
    pivot_longer(cols = where(is.double), 
                 names_to = "trendscoarse", 
                 values_to = "targets") %>% 
    mutate(indicator = fct_relevel(indicator, indic_order)) %>% 
    arrange(indicator) %>% 
    spread(snuprioritization, targets) %>% 
    rowwise() %>% 
    mutate(Total = sum(c_across(3:8), na.rm = T))
  
  tbl1 %>% write_csv("Dataout/DP_tally_tbl1.csv", na = "")
  
  tbl1 %>% 
    gt(rowname_col  = "indicator") %>% 
    fmt_number(columns = where(is.numeric),
               decimals = 0) %>% 
    sub_missing(missing_text = ".") %>% 
    tab_options(
      data_row.padding = px(0.5)
    ) 
    gtExtras::gtsave_extra("Images/dp_summary_tbl1.png")

s# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

