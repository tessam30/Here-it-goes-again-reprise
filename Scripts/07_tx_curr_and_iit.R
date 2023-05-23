# PROJECT: FY23 Q2 Review of Zambia Data Treatment Trends at lower level
# PURPOSE: Munge and Analysis of MSD for Zambia
# AUTHOR: Tim Essam | SI
# REF ID:   f91a4f6d
# LICENSE: MIT
# DATE: 2023-05-12
# NOTES: Tim Essam | SI


# LOCALS & SETUP ============================================================================
  
  # Load Q1 functions
    source("Scripts/helper-call_all_helpers.R")

  library(ggtext)
  library(fontawesome)
  library(extrafont)
  library(gtExtras)
  library(gt)
    
  # metadata
    get_metadata(site_path)
    
  # REF ID for plots
    ref_id <- "f91a4f6d" 
    
  # Pull in Site attribute data
    df_site_atrb <- readRDS("../i_knew_you_were_trouble/Dataout/site_attrib_df") %>% 
      filter(operatingunit == "Zambia",
             dataelement == "SA_FACILITY_TYPE")
    
# LOAD DATA ============================================================================  

    df_site <- read_psd(site_path) %>% 
      mutate(snu1 = str_replace_all(snu1, " Province", ""))
    
    full_pds <- (min(df_site$fiscal_year) - 1) %>% 
      paste0("-10-01") %>% 
      as_date() %>% 
      seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>% 
      convert_date_to_qtr()
    
    pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")    

# PEDIATRIC TX_CURR GROWTH/SHRINKAGE=====================================================
    
    df_tx <- df_site %>% 
      filter(funding_agency == "USAID",
             indicator == "TX_CURR",
             standardizeddisaggregate == "Age/Sex/HIVStatus",
             trendscoarse != "Unknown Age") %>% 
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>% 
      group_by(indicator, fiscal_year, trendscoarse, snu1) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd("quarters") %>% 
      select(-results_cumulative)
    
    df_tx <- df_tx %>% 
      group_by(snu1, trendscoarse) %>%
      mutate(decline = results < lag(results, 1),
             decline_shp = ifelse(decline == TRUE, "\u25Bc", "\u25B2"),
             fill_color = case_when(fiscal_year < metadata$curr_fy ~ trolley_grey,
                                    decline == TRUE ~ golden_sand,
                                    TRUE ~ scooter),
             fill_alpha = ifelse(fiscal_year < metadata$curr_fy, .6, .9),
             results_latest = case_when(period == max(period) ~ results),
             decline_latest = case_when(period == max(period) ~ decline_shp)) %>% 
      fill(results_latest,decline_latest, .direction = "up") %>% 
      mutate(disp_name = glue("{snu1} {decline_latest}")) %>% 
      ungroup()  
    
    v_tx_lrg <- df_tx %>% 
      filter(period == max(period),
             trendscoarse == "<15") %>% 
      arrange(desc(results)) %>% 
      mutate(cumsum = cumsum(results)/sum(results)) %>% 
      slice_head(n = 11) %>% 
      pull(snu1)
    
    df_tx %>%
      filter(
        snu1 %ni% c("Eastern", "Southern"),
        trendscoarse == "15+") %>% 
      ggplot(aes(period, results, fill = fill_color, alpha = fill_alpha)) +
      geom_col() +
      geom_text(data = . %>% filter(period == max(period)), 
                aes(label = label_number_si()(results_latest)), 
                vjust = -.7, color = matterhorn,
                family = "Source Sans Pro") +
      facet_wrap(~fct_reorder2(disp_name, period, results), scales = "free_y") +
      scale_fill_identity() +
      scale_alpha_identity() +
      scale_y_continuous(label = label_number_si()) +
      scale_x_discrete(labels = pd_brks) +
      coord_cartesian(expand = T, clip = "off") +
      labs(x = NULL, y = NULL, 
           title = glue("OVERALL PEDIATRIC TX_CURR THROUGH {metadata$curr_pd}"),
           subtitle = glue("FY22 flagged <span style='color:{golden_sand}'>decline</span>/<span style='color:{scooter}'>growth</span>"),
           caption = glue("Source: {metadata$source}")) +
      si_style_ygrid() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown(),
            strip.text = element_markdown())
    
    si_save("Images/TX_CURR_pediatric_growth_snu.png")
    
    
# IIT ANALYSIS ============================================================================

    df_iit <- df_site %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
      group_by(fiscal_year, snu1, trendscoarse, facility, facilityuid, indicator) %>% 
      #group_by(fiscal_year, snu1, facility, facilityuid, indicator, mech_code, mech_name, psnu) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      arrange(period) %>% 
      group_by(trendscoarse, facilityuid) %>% 
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
      ungroup()%>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup()

    
    df_iit %>% 
      filter(trendscoarse == "15+", 
             snu1 %ni% c("Eastern", "Southern")) %>% 
      mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
      filter(!is.na(snu1), period != min(period)) %>% 
      ggplot(aes(period, iit, size = tx_curr_lag1)) +
      geom_point(position = position_jitter(width = .2, seed = 42),
                 na.rm = TRUE, color = scooter,
                 alpha = .2) +
      geom_smooth(aes(weight = tx_curr_lag1, group = snu1),
                  method = "loess",
                  formula = "y ~ x", se = FALSE, na.rm = TRUE,
                  size = 1.5, color = golden_sand) +
      facet_wrap(~snu1) +
      scale_size(label = comma) +
      scale_x_discrete(labels = full_pds[2:6]) +
      scale_y_continuous(limits = c(0,.25),
                         label = percent_format(1),
                         oob = oob_squish) +
      labs(x = NULL, y = NULL,
           size = "Site TX_CURR (1 period prior)",
           title = glue("Adult IIT declined in {metadata$curr_pd}") %>% toupper,
           subtitle = glue("IIT calculated in the largest {length(v_tx_lrg)} TX_CURR regions"),
           caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {metadata$source}")) +
      si_style() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown())
    
    si_save("Images/IIT_adult_increase_snu1.png")
    si_save("Images/IIT_ped_increase_snu1.png")
    
    
# SPARKLINES ============================================================================
    
    df_iit_snu1 <- df_site %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus")) %>%
      group_by(fiscal_year, snu1, indicator) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(snu1) %>% 
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
      ungroup()%>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(period != min(period), 
             str_detect(snu1, "Lusaka|Eastern|Southern", negate = T))
    
    df_iit_spark <- 
      df_iit_snu1 %>% 
      select(period, snu1, iit) %>% 
      arrange(snu1, period) %>% 
      group_by(snu1) %>% 
      summarize(spark_iit = list(iit), .groups = "drop") 
    
    df_iit_snu1 %>% 
      select(period, snu1, iit) %>%
      spread(period, iit) %>% 
      left_join(., df_iit_spark) %>% 
      gt() %>%
      gt_plt_sparkline(spark_iit, 
                       same_limit = , type = "shaded", 
                       fig_dim = c(10, 30),
                       palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                       label = F) %>% 
      fmt_percent(columns = where(is.numeric)) %>% 
      cols_label(snu1 = "",
                 spark_iit = "",
                 FY22Q2 = "Q2",
                 FY22Q3 = "Q3",
                 FY22Q4 = "Q4",
                 FY23Q1 = "Q1"
      ) %>% 
      tab_header(
        title = glue("INTERRUPTION IN TREATMENT SUMMARY BY PROVINCE"),
      ) %>% 
      tab_spanner(
        label = "FY22",
        columns = 2:3
      ) %>% 
      tab_spanner(
        label = "FY23",
        columns = 5
      ) %>% 
      tab_source_note(
        source_note = gt::md(glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW\n
                        Source: {metadata$source}"))) %>% 
      tab_options(
        source_notes.font.size = px(10),
        data_row.padding = px(1),
        ) %>% 
      shrink_rows() %>% 
      gt_theme_nytimes() %>% 
      gtsave_extra("Images/USAID_iit_province.png")
    


# IIT BY MECHANISM SPARKLINES ---------------------------------------------

    df_iit_mech <- df_site %>% 
      fix_mech_names() %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"), 
             mech_name != "Placeholder - 86412") %>% 
      group_by(fiscal_year, indicator, mech_name) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(mech_name) %>% 
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
      ungroup()%>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(period != min(period), mech_name != "ZIHA")
    
    
    df_iit_spark_mech <- 
      df_iit_mech %>% 
      select(period, mech_name, iit) %>% 
      arrange(mech_name, period) %>% 
      group_by(mech_name) %>% 
      summarize(spark_iit = list(iit), .groups = "drop")     
    
    df_iit_mech %>% 
      select(period, mech_name, iit) %>%
      spread(period, iit) %>% 
      left_join(., df_iit_spark_mech) %>% 
      gt() %>%
      gt_plt_sparkline(spark_iit, 
                       same_limit = , type = "shaded", 
                       fig_dim = c(15, 30),
                       palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                       label = F) %>% 
      fmt_percent(columns = where(is.numeric)) %>% 
      cols_label(mech_name = "",
                 spark_iit = "",
                 FY23Q1 = "Q1",
                 FY22Q2 = "Q2",
                 FY22Q3 = "Q3",
                 FY22Q4 = "Q4"
      ) %>% 
      tab_header(
        title = glue("INTERRUPTION IN TREATMENT SUMMARY BY MECHANISM"),
      ) %>% 
      tab_spanner(
        label = "FY22",
        columns = 2:4
      ) %>% 
      tab_spanner(
        label = "FY23",
        columns = 5
      ) %>% 
      sub_missing(columns = everything(), missing_text = "-") %>% 
      tab_source_note(
        source_note = gt::md(glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW\n
                        Source: {metadata$source}"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      gt_theme_nytimes() %>% 
      gtsave_extra("Images/USAID_iit_mech.png")
    
    
# What DOES IIT look like by facility type?

# FACILITY TYPE IIT -------------------------------------------------------

  df_iit %>% 
      left_join(., df_site_atrb %>% select(orgunit, value), by = c("facilityuid" = "orgunit")) %>% 
      filter(trendscoarse == "15+", 
             snu1 %ni% c("Eastern", "Southern"), 
             !is.na(value)) %>% 
      mutate(snu1 = factor(snu1, v_tx_lrg)) %>% 
      filter(!is.na(snu1), period != min(period)) %>% 
      ggplot(aes(period, iit, size = tx_curr_lag1)) +
      geom_point(position = position_jitter(width = .2, seed = 42),
                 na.rm = TRUE, color = scooter,
                 alpha = .2) +
      geom_smooth(aes(weight = tx_curr_lag1, group = value),
                  method = "loess",
                  formula = "y ~ x", se = FALSE, na.rm = TRUE,
                  size = 1.5, color = golden_sand) +
      facet_wrap(~value) +
      scale_size(label = comma) +
      scale_x_discrete(labels = full_pds[2:6]) +
      scale_y_continuous(limits = c(0,.25),
                         label = percent_format(1),
                         oob = oob_squish) +
      labs(x = NULL, y = NULL,
           size = "Site TX_CURR (1 period prior)",
           title = glue("Adult IIT declined in {metadata$curr_pd}") %>% toupper,
           subtitle = glue("IIT calculated in the largest {length(v_tx_lrg)} TX_CURR regions"),
           caption = glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW; ITT capped to 25%
                        Source: {metadata$source}")) +
      si_style() +
      theme(panel.spacing = unit(.5, "line"),
            plot.subtitle = element_markdown())
    

    df_iit_factype <- 
      df_site %>% 
      fix_mech_names() %>% 
      swap_targets() %>% 
      filter(funding_agency == "USAID",
             indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_RTT"), 
             standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"), 
             mech_name != "Placeholder - 86412") %>% 
      left_join(., df_site_atrb %>% select(orgunit, factype = value), by = c("facilityuid" = "orgunit")) %>% 
      group_by(fiscal_year, indicator, factype) %>% 
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
      reshape_msd(include_type = FALSE) %>% 
      pivot_wider(names_from = "indicator",
                  names_glue = "{tolower(indicator)}") %>% 
      group_by(factype) %>% 
      mutate(tx_curr_lag1 = lag(tx_curr, n = 1)) %>% 
      ungroup()%>% 
      rowwise() %>% 
      mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(period != min(period))
    
    df_iit_spark_factype <- 
      df_iit_factype %>% 
      select(period, factype, iit) %>% 
      arrange(factype, period) %>% 
      group_by(factype) %>% 
      summarize(spark_iit = list(iit), .groups = "drop")     
    
    df_iit_factype %>% 
      select(period, factype, iit) %>%
      spread(period, iit) %>% 
      left_join(., df_iit_spark_factype) %>% 
      gt() %>%
      gt_plt_sparkline(spark_iit, 
                       same_limit = , type = "shaded", 
                       fig_dim = c(15, 30),
                       palette = c(grey70k, grey90k, old_rose_light, scooter_med, grey10k),
                       label = F) %>% 
      fmt_percent(columns = where(is.numeric)) %>% 
      cols_label(factype = "",
                 spark_iit = "",
                 FY23Q1 = "Q1",
                 FY22Q2 = "Q2",
                 FY22Q3 = "Q3",
                 FY22Q4 = "Q4"
      ) %>% 
      tab_header(
        title = glue("INTERRUPTION IN TREATMENT SUMMARY BY MECHANISM"),
      ) %>% 
      tab_spanner(
        label = "FY22",
        columns = 2:4
      ) %>% 
      tab_spanner(
        label = "FY23",
        columns = 5
      ) %>% 
      sub_missing(columns = everything(), missing_text = "-") %>% 
      tab_source_note(
        source_note = gt::md(glue("IIT = TX_ML / TX_CURR_LAG1 + TX_NEW\n
                        Source: {metadata$source}"))) %>% 
      tab_options(
        source_notes.font.size = px(10)) %>% 
      gt_theme_nytimes() 
      