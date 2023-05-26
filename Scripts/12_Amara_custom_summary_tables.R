# PROJECT: FY23Q2 Review summary tables
# PURPOSE: Munge and Analysis of MSD
# AUTHOR: Tim Essam | SI
# REF ID:   7a9165bb
# LICENSE: MIT
# DATE: 2023-05-23
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
    library(selfdestructin5)
  
  # SI specific paths/functions  
    source("Scripts/helper-call_all_helpers.R")

  # Grab metadata
    get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "f91a4f6d"
  
  # Load functions    
  df_msd_all_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM_FY21-23_20230512_v1_1.zip")
    

# LOAD DATA ============================================================================  

  df_msd_all <- read_psd(df_msd_all_path)

# MUNGE ============================================================================
  
  mdb_df   <- make_mdb_df(df_msd_all)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_msd_all, resolve_issues = T)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  

  # Generate base table for global results
  mdb_tbl %>% 
    filter(indicator %ni% c("GEND_GBV", "KP_PREV", "TB_PREV")) %>%
    create_mdb(ou = "Global", type = "main", metadata$curr_pd, metadata$source) %>% 
    shrink_rows() %>% 
    cols_width(
      indicator2 ~ px(200),
      contains("achv") ~ px(5),
      contains("present_z") ~ px(10)
    ) %>% 
    gtsave_extra(path = "Images", filename = glue::glue("Global_{metadata$curr_pd}_mdb_main_AMARA.png"))  
  
  create_mdb(mdb_tbl_tx, ou = "Global", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
    embiggen() %>% 
    tab_source_note(
      source_note = md("**TX_CURR NOTE:** South Africa has no national MMD program and has been excluded from MMD and TX_CURR calculations")
    ) %>% 
    shrink_rows() %>% 
    gtsave_extra(path = "Images", filename = glue::glue("Global_{metadata$curr_pd}_Zambia_MMD_VL_MD.png"))  
  

 #Zambia Results
  
  mdb_tbl %>% 
    filter(indicator %ni% c("GEND_GBV", "KP_PREV", "TB_PREV")) %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
    shrink_rows() %>% 
    cols_width(
      indicator2 ~ px(200),
      contains("achv") ~ px(5),
      contains("present_z") ~ px(10)
    ) %>% 
    gtsave_extra(path = "Images", filename = glue::glue("metadata$curr_pd}_mdb_main_AMARA.png"))  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
    embiggen() %>% 
    shrink_rows() %>% 
    gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_MMD_VL_MD_AMARA.png"))  
  
  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

