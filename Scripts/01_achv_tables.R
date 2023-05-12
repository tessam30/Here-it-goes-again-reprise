# PROJECT: FY23 Q2 Review of Zambia Data
# PURPOSE: Munge and Analysis of MSD for Zambia
# AUTHOR: Tim Essam | SI
# REF ID:   f91a4f6d
# LICENSE: MIT
# DATE: 2023-05-12
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
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata, pattern = "Genie.*Zambia")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "f91a4f6d"
  
  # Load functions    
  source("Scripts/helper-call_all_helpers.R")
    

# CUSTOM IP TABLES --------------------------------------------------------

  mk_ptr_tbl <- function(df, mech_id)  {    
      ip_mdb <- 
        df %>% 
        filter(mech_code == mech_id) %>% 
        make_mdb_df() %>% 
        reshape_mdb_df(., metadata$curr_pd) 
      
      mech_name <-  
        df %>% 
        filter(mech_code == mech_id) %>%
        distinct(mech_name) %>% 
        pull(mech_name)
      
      ip_mdb %>%   
        create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
        tab_header(
          title = glue::glue("{mech_name} PERFORMANCE SUMMARY")
        ) %>% 
        gtsave(path = "Images", filename = glue::glue("{mech_name}_mdb_main.png"))
    }
  

  shrink_rows <- function(gt_obj){
    gt_obj %>% 
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      ) 
  }  
    
# LOAD DATA ============================================================================  

  df_genie <- read_psd(file_path) 
  
  df_genie %>% filter(fiscal_year == 2023) %>% count(mech_name, mech_code) %>% prinf()

# MUNGE ============================================================================
  
  # Check if data is in yet 
  df_genie %>% filter(fiscal_year == 2023, indicator %in% cascade_ind) %>% View()
  
  df_pepfar <- df_genie %>% mutate(funding_agency = "PEPFAR")
  
# SUMMARY TABLES PEPFAR ===================================================================
    
  mdb_df   <- make_mdb_df(df_pepfar)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd) %>%  
    mutate(agency = "PEPFAR") 
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_pepfar)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd) %>% 
    mutate(agency = "PEPFAR") 
  
  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>%
    shrink_rows() %>% 
    gtsave_extra(path = "Images", filename = glue::glue("Zambia_PEPFAR_{metadata$curr_pd}_mdb_main.png"))  
  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
    embiggen() %>% 
    tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) %>% 
    gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_PEPFAR_MMD_VL_MD.png"))   

# SUMMARY TABLES BY USAID OTHER AGENCIES

  mdb_df   <- make_mdb_df(df_genie)
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_genie)
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  
  mdb_tbl %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
    shrink_rows() %>% 
    gtsave_extra(path = "Images", filename = glue::glue("Zambia_{metadata$curr_pd}_mdb_main.png"))  
  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
    embiggen() %>% 
    shrink_rows() %>% 
    gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_MMD_VL_MD.png"))  
  

# Summary Table copperbelt only
  # SUMMARY TABLES BY USAID OTHER AGENCIES
  
  mdb_df   <- make_mdb_df(df_genie %>% filter(snu1 == "Copperbelt Province"))
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)  
  
  # Create the treatment data frame needed for derived indicators
  mdb_df_tx    <- make_mdb_tx_df(df_genie %>% filter(snu1 == "Copperbelt Province"))
  mdb_tbl_tx   <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd)
  
  mdb_tbl %>% 
    mutate(agency = ifelse(agency != "USAID", "PEACE CORPS", "USAID")) %>% 
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Zambia", type = "main", metadata$curr_pd, metadata$source) %>% 
    shrink_rows() %>% 
    tab_header("COPPERBELT PERFORMANCE SUMMARY") %>% 
    gtsave_extra(path = "Images", filename = glue::glue("Zambia_{metadata$curr_pd}_CBELT_mdb_main.png"))  
  
  
  create_mdb(mdb_tbl_tx, ou = "Zambia", type = "treatment", metadata$curr_pd, metadata$source) %>% 
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>% 
    embiggen() %>% 
    shrink_rows() %>% 
    tab_header("COPPERBELT PERFORMANCE SUMMARY") %>%
    gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_Zambia_CBELT_MMD_VL_MD.png"))  
  
# Partner Tables ============================================================================
  
  # Loop over function and create tables for each of the main C&T mechs
  mech_list <- c(82075, 17413, 82086, 17399)
  map(mech_list, ~mk_ptr_tbl(df_genie, .x))
  
  # Special table that maps ZIHA target back to SAFE
  df_safe <- df_genie %>% 
    filter(mech_code %in% c(86412, 17413)) %>% 
    mutate(mech_code = "17413", 
           mech_name = "SAFE (with ZIHA Targets)")
  
  mk_ptr_tbl(df_safe, 17399)
  
# ZAMBIA OVERALL TABLE
  df_genie %>% 
    filter(indicator == "OVC_SERV", snu1 == "Copperbelt Province",
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(funding_agency, fiscal_year, indicator) %>% 
    summarise(tgt = sum(targets, na.rm = T))
  

# MECH CODE ACHV TABLE ----------------------------------------------------

  mech_list <- c(82075, 17413, 17399,  82086)
  
  df_usaid <- df_genie %>% 
    filter(funding_agency == "USAID")
  
  df_achv <- df_usaid %>% 
    filter(indicator %in% c("VMMC_CIRC", "PrEP_NEW", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate == "Total Numerator", 
           fiscal_year == metadata$curr_fy, 
           funding_agency != "DEDUP") %>% 
    group_by(fiscal_year, mech_code, mech_name, funding_agency, indicator) %>% 
    summarise(across(matches("cumul|targ"), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    calc_achievement() %>% 
    adorn_achievement(qtr = metadata$curr_qtr) %>% 
    mutate(tgt_rslt_sum = format_achv(cumulative, targets))
  
  # So, we want to create a similar shaped data frame so we can bind rows. 
  # BUT -- VLS and VLC do not have "targets" so we'll use actual TX_PVLS and TX_CURR #s in place
  
  df_vl <- df_usaid %>%
    create_vl_df(mech_name, mech_code, funding_agency) %>% 
    filter(period == metadata$curr_pd) %>% 
    select(-vls_adj) %>% 
    pivot_longer(cols = vlc:vls, 
                 names_to = "indicator",
                 values_to = "achievement") %>% 
    mutate(tgt_rslt_sum = case_when(
      indicator == "vls" ~ format_achv(tx_pvls, tx_pvls_d),
      indicator == "vlc" ~ format_achv(tx_pvls_d, tx_curr_lag2)
    )) %>% 
    adorn_achievement() %>% 
    mutate(indicator = str_to_upper(indicator),
           fiscal_year = metadata$curr_fy, .after = "period") %>% 
    select(-c(period, tx_curr, tx_curr_lag2, tx_pvls, tx_pvls_d))  
  
  # Bind'em together so we can plot them in a table
  df_achv_all <- 
    bind_rows(df_achv, df_vl) %>% 
    mutate(indicator = fct_relevel(indicator, c("PrEP_NEW","VMMC_CIRC",  "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC"))) %>% 
    arrange(mech_name, indicator)  
  
  # One Take  
  df_achv_all %>% 
    select(mech_code, mech_name, indicator, achievement, tgt_rslt_sum, achv_color) %>% 
    mutate(achv = percent(achievement, 1), 
           value = format_indicator(achv, tgt_rslt_sum, achv_color)) %>% 
    select(mech_code, mech_name, indicator, value) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "value") %>% 
    filter(mech_code %in% mech_list) %>% 
    gt() %>% 
    gt::fmt_markdown(columns = 3:9) %>% 
    sub_missing(missing_text = "-") %>% 
    cols_hide(mech_code) %>%
    cols_label(mech_name = "") %>% 
    tab_header(
      title = glue("ZAMBIA MECHANISM PERFORMANCE SUMMARY AS OF {metadata$curr_pd}")
    )  %>% 
    gtsave_extra("Images/USAID_partner_table_achv_levels.png")
  
  # Standard table w/ colors
  df_ach_all_gt <- df_achv_all %>% 
    mutate(mech_name = str_replace_all(mech_name, "( Follow On| Follow on)", "")) %>% 
    filter(mech_code %in% mech_list) %>% 
    select(mech_code, mech_name, indicator, achievement, funding_agency) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "achievement")
  
  
# PEDIATRIC SUMMARY TABLE -------------------------------------------------
  
  df_achv_peds <- df_usaid %>% 
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Modality/Age/Sex/Result"), 
           fiscal_year == metadata$curr_fy, 
           trendscoarse == "<15") %>% 
    group_by(fiscal_year, mech_code, mech_name, funding_agency, indicator) %>% 
    summarise(across(matches("cumul|targ"), sum, na.rm = T), .groups = "drop") %>% 
    calc_achievement() %>% 
    adorn_achievement(qtr = metadata$curr_qtr) %>% 
    mutate(tgt_rslt_sum = format_achv(targets, cumulative))
  
  
  # BUT -- VLS and VLC do not have "targets" so we'll use actual TX_PVLS and TX_CURR #s in place
  df_vl_peds <- df_usaid %>%
    filter(trendscoarse == "<15") %>% 
    create_vl_df(mech_name, mech_code, funding_agency, trendscoarse) %>% 
    filter(period == metadata$curr_pd) %>% 
    select(-vls_adj) %>% 
    pivot_longer(cols = vlc:vls, 
                 names_to = "indicator",
                 values_to = "achievement") %>% 
    mutate(tgt_rslt_sum = case_when(
      indicator == "vls" ~ format_achv(tx_pvls, tx_pvls_d),
      indicator == "vlc" ~ format_achv(tx_pvls_d, tx_curr_lag2)
    )) %>% 
    adorn_achievement() %>% 
    mutate(indicator = str_to_upper(indicator),
           fiscal_year = metadata$curr_fy, .after = "period") %>% 
    select(-c(period, tx_curr, tx_curr_lag2, tx_pvls, tx_pvls_d))
  
  # Bind'em together so we can plot them in a table
  df_achv_pedsl <- 
    bind_rows(df_achv_peds, df_vl_peds) %>% 
    mutate(indicator = fct_relevel(indicator, c("HTS_TST", "HTS_TST_POS", "TX_CURR", "TX_NEW", "VLS", "VLC"))) %>% 
    filter(mech_code %in% mech_list) %>% 
    arrange(mech_name, indicator)  %>% 
    select(mech_code, mech_name, indicator, achievement, funding_agency) %>% 
    pivot_wider(names_from = "indicator",
                values_from = "achievement")  
  
  df_achv_pedsl %>% 
    gt() %>% 
    gt::fmt_markdown(columns = 3:9) %>% 
    sub_missing(missing_text = "-") %>% 
    cols_hide(mech_code) %>%
    cols_label(mech_name = "") %>% 
    tab_header(
      title = glue("ZAMBIA MECHANISM PERFORMANCE SUMMARY AS OF {metadata$curr_pd}")
    )    

  
# CREATE COLOR CODED TABLES -----------------------------------------------
  
  tmp <- df_ach_all_gt %>% 
    fix_mech_names() %>% 
    select(-mech_name_short) %>% 
    gt(groupname_col = "funding_agency") %>% 
    sub_missing(missing_text = "-") %>% 
    fmt_percent(columns = 3:10, decimals = 0)
  
  # Needed this loop to get the colors mapped in correctly 
  # https://stackoverflow.com/questions/63944953/how-to-conditionally-format-a-cell-in-a-gt-table-based-on-the-value-of-the-cel
  indic_cols <- names(df_ach_all_gt)[4:9]
  for(i in seq_along(indic_cols)){
    tmp <- tmp %>% 
      tab_style(
        style = cell_fill(color = "#ff939a", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] < 0
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#ffcaa2", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] >= 0 &  tmp$`_data`[[indic_cols[i]]] < 0.15
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#5BB5D5", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] >= 0.15 &  tmp$`_data`[[indic_cols[i]]] <= 0.35
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#e6e6e6", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] > 0.35
        )
      ) %>% 
      tab_style(
        style = cell_borders(color = "#ffffff", weight = px(2)),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>% 
      cols_width(
        mech_code ~ px(50),
        everything() ~ px(100)
      )
  } 
  
  tmp %>% 
    tab_header(
      title = glue("ZAMBIA MECHANISM PERFORMANCE SUMMARY FOR {metadata$curr_pd}"),
      subtitle = ""
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue::glue("{metadata$caption}"))
    ) %>% 
    tab_options(
      source_notes.font.size = px(10)
    ) %>% 
    selfdestructin5::bold_rowgroup() %>% 
    cols_label(mech_name = "", 
               mech_code = "") %>% 
    cols_hide(mech_code) %>% # ADJUST colors of TX_CURR and VLS below
    tab_style(
      style = cell_fill(color = "#ff939a", alpha = 0.75),
      locations = cells_body(
        columns = 10,
        rows =  tmp$`_data`[[names(df_ach_all_gt)[10]]] <= 0.85
      )
    ) %>% 
    tab_style(
      style = cell_fill(color = "#5BB5D5", alpha = 0.75),
      locations = cells_body(
        columns = 10,
        rows =  tmp$`_data`[[names(df_ach_all_gt)[10]]] > 0.85
      )
    ) %>% 
    gtsave_extra("Images/USAID_summary_table_achv.png")
  
  
  # REPEAT for PEDS ---------------------------------------------------------
  
  remove(tmp)
  tmp <- df_achv_pedsl %>% 
    fix_mech_names() %>% 
    select(-mech_name_short) %>% 
    gt(groupname_col = "funding_agency") %>% 
    sub_missing(missing_text = "-") %>% 
    fmt_percent(columns = 3:9, decimals = 0)
  
  # Needed this loop to get the colors mapped in correctly 
  # https://stackoverflow.com/questions/63944953/how-to-conditionally-format-a-cell-in-a-gt-table-based-on-the-value-of-the-cel
  indic_cols <- names(df_achv_pedsl)[c(4:5, 7:8)]
  for(i in seq_along(indic_cols)){
    tmp <- tmp %>% 
      tab_style(
        style = cell_fill(color = "#ff939a", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] < 0
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#ffcaa2", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] >= 0 &  tmp$`_data`[[indic_cols[i]]] < 0.15
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#5BB5D5", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] >= 0.15 &  tmp$`_data`[[indic_cols[i]]] <= 0.35
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#e6e6e6", alpha = 0.75),
        locations = cells_body(
          columns = indic_cols[i],
          rows = tmp$`_data`[[indic_cols[i]]] > 0.35
        )
      ) %>% 
      tab_style(
        style = cell_borders(color = "#ffffff", weight = px(2)),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>% 
      cols_width(
        mech_code ~ px(50),
        everything() ~ px(100)
      )
  } 
  
  tmp %>% 
    tab_header(
      title = glue("ZAMBIA PEDIATRIC MECHANISM PERFORMANCE SUMMARY FOR {metadata$curr_pd}"),
      subtitle = ""
    ) %>% 
    tab_source_note(
      source_note = gt::md(glue::glue("{metadata$caption}"))
    ) %>% 
    tab_options(
      source_notes.font.size = px(10)
    ) %>% 
    selfdestructin5::bold_rowgroup() %>% 
    cols_label(mech_name = "", 
               mech_code = "") %>% 
    cols_hide(mech_code) %>% # ADJUST colors of TX_CURR and VLS below
    tab_style(
      style = cell_fill(color = "#ff939a", alpha = 0.75),
      locations = cells_body(
        columns = 6,
        rows =  tmp$`_data`[[names(df_achv_pedsl)[6]]] <= 0.85
      )
    ) %>% 
    tab_style(
      style = cell_fill(color = "#5BB5D5", alpha = 0.75),
      locations = cells_body(
        columns = 6,
        rows =  tmp$`_data`[[names(df_achv_pedsl)[6]]] > 0.85
      )
    ) %>% 
    tab_style(
      style = cell_fill(color = "#ff939a", alpha = 0.75),
      locations = cells_body(
        columns = 9,
        rows =  tmp$`_data`[[names(df_achv_pedsl)[9]]] <= 0.85
      )
    ) %>% 
    gtsave_extra("Images/USAID_summary_table_achv_peds.png")
  
