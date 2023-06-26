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
  file_path <- return_latest(folderpath = merdata, pattern = "OU_IM")
  
  # Grab metadata
  get_metadata(file_path)
  
  # REF ID for plots
  ref_id <- "f91a4f6d"
  
  # Functions
  drkn_clmn_hdr <- function(.data){
    .data %>%  
      tab_style(
        style = list(
          cell_text(color = grey90k)
        ),
        locations = cells_column_labels(
        )
      )
  }

# MUNGE -------------------------------------------------------------------


  
  
  df <- read_psd(file_path)
  
  df_prep <- df %>% 
    filter(indicator == "PrEP_NEW", 
           standardizeddisaggregate == "Total Numerator",
           fiscal_year %in% c(2023))
  
  
  df_prep %>%
    group_by(operatingunit, fiscal_year) %>% 
    summarize(across(contains("qtr"), \(x) sum(x, na.rm = T)), .groups = "drop") %>% 
    select(ou = operatingunit,
           fy = fiscal_year,
           Q1 = qtr1,
           Q2 = qtr2)%>% 
    mutate(delta = Q2 - Q1, 
           cumulative = Q1 + Q2,
           pct_chg = ((Q2/Q1) - 1)) %>% 
    mutate(overall = sum(Q2, na.rm = T),
           Q2_share = Q2 / overall) %>% 
    arrange(desc(Q2_share)) %>% 
    filter(row_number() <= 10) %>% 
    gt() %>% 
    cols_hide(c(fy, overall)) %>% 
    fmt_number(Q1:cumulative, 
               decimals = 0) %>% 
    fmt_percent(pct_chg:Q2_share, 
                decimals = 0) %>% 
    cols_label(ou = "",
               cumulative ="FY23 Totals", 
               pct_chg = "Q2 --> Q1 change",
               Q2_share = "Q2 Share") %>% 
    gt_theme_nytimes() %>% 
    drkn_clmn_hdr() %>% 
    tab_header(
      title = glue("PREP_NEW FY23Q2 TOP 10 ACHIEVING OUs")
    ) %>% 
    tab_source_note(
      source_note = glue("Source: {metadata$source}")
    ) %>% 
    tab_options( # Adjusting padding between rows and font sizes
      table.font.size = px(12),
      column_labels.font.size = px(14),
      row_group.font.weight = "bold",
      row_group.font.size = px(14),
      data_row.padding = px(4), 
      summary_row.padding = px(1.5),
      grand_summary_row.padding = px(1.5),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) %>% 
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        rows = ou == "Zambia"
      )
    ) %>% 
    gtsave_extra(filename = glue("Images/PrEP_NEW_GLOBAL_TOP_10.png"))  

  
  

# MDB testing -------------------------------------------------------------

  df_moz <- 
    df %>% 
    filter(operatingunit == "Mozambique")
  
  legend_chunk_q2 <- gt::md(glue::glue("Legend: Cumulative <img src= '{legend_q2}' style='height:15px;'>    &emsp; Snapshot (TX_CURR) <img src= '{legend_snapshot}' style='height:15px;'> "))
  
  mdb_df   <- make_mdb_df(df_moz)
  
  # Create the reshaped df that is gt() ready
  mdb_tbl  <- reshape_mdb_df(mdb_df, metadata$curr_pd)
  
  mdb_tbl %>% create_mdb(ou = "Mozambique", type = "main", metadata$curr_pd, metadata$source, 
                         legend = legend_chunk_q2)
