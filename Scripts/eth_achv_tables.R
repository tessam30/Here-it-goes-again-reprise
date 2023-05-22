# PROJECT: FY23 Q2 Review of Ethiopia OU X IM Data
# PURPOSE: Create MDB tables for all of PEPFAR
# AUTHOR: Tim Essam | SI
# LICENSE: MIT
# DATE: 2023-05-18

# SETUP ---------------------------------------------------------------

  # Set libraries needed
  library(gagglr)
  library(tidyverse)
  library(selfdestructin5)
  library(gt)
  library(gtExtras)


  # Reference genie path from downloads
  genie_path <- "../../../Downloads/Genie-OUByIMs-Ethiopia-Daily-2023-05-18.zip"
  
  # To get list of Q2 indicators from crosswalk
  load_secrets()
  
  # Load and store metadata --> setting all agencies to be PEPFAR
  df_pepfar <- read_psd(genie_path) %>% mutate(funding_agency = "PEPFAR")
  get_metadata(genie_path)


# BASE TABLE --------------------------------------------------------------

  # Create base table
  mbd_df <- make_mdb_df(df_pepfar)
  mdb_tbl <- reshape_mdb_df(mbd_df, metadata$curr_pd) %>%
    mutate(agency = "ALL PEPFAR") # This is setting the column name for the table; modify as needed
  
  # mdb_tbl should give you a dataframe with Ethiopia and Global results (these should be the same)
  
  mdb_tbl %>%
    # filter(indicator != "GEND_GBV") %>%
    create_mdb(ou = "Ethiopia", type = "main", metadata$curr_pd, metadata$source) %>%
    gtsave_extra(path = "Images", filename = glue::glue("{metadata$curr_pd}_ETH_PEPFAR__mdb_main.png"))

# TREATMENT TABLE ---------------------------------------------------------

  # Create the treatment data frame needed for derived indicators
  mdb_df_tx <- make_mdb_tx_df(df_pepfar)
  mdb_tbl_tx <- reshape_mdb_tx_df(mdb_df_tx, metadata$curr_pd) %>%
    mutate(agency = "ALL PEPFAR")
  
  create_mdb(mdb_tbl_tx, ou = "Ethiopia", type = "treatment", metadata$curr_pd, metadata$source) %>%
    bold_column(., metadata$curr_pd %>% substr(., 5, 6)) %>%
    embiggen() %>%
    tab_options(
      data_row.padding = px(1),
      row_group.padding = px(2),
      heading.padding = px(1)
    ) %>%
    gtsave_extra(., path = "Images", filename = glue::glue("{metadata$curr_pd}_ETH_PEPFAR_MMD_VL_MD.png"))
  
  
