# PROJECT: Munge and analysis of FY23Q2 data
# PURPOSE: Munge and Analysis of GEND_GBV
# AUTHOR: Tim Essam | SI
# REF ID:   ee51b07e
# LICENSE: MIT
# DATE: 2023-05-17
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Load Q1 functions
    source("Scripts/helper-call_all_helpers.R")
    
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
   get_metadata(file_path)
   
   shpdata <- glamr::si_path("path_vector")
  
  # REF ID for plots
    ref_id <- "ee51b07e"
    
  # Functions  
    # Functions  
    munge_gbv <- function(...){
      df_msd %>% 
        filter(indicator == "GEND_GBV", 
               standardizeddisaggregate == "Total Numerator") %>% 
        group_by(fiscal_year, indicator, ...) %>% 
        summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
        ungroup() %>% 
        adorn_achievement()
    }
    
  

# LOAD DATA ============================================================================  

    df_msd <- read_msd(file_path) %>% 
      filter(str_detect(indicator, "GEND"))
    
  
# MUNGE ============================================================================
  
    # Trends
    gbv_ou <- munge_gbv()  
    
    gbv_agency <- munge_gbv(funding_agency) %>% clean_agency()    
    
    gbv_dreams <- munge_gbv(dreams, psnu, psnuuid) %>% 
      mutate(dreams = ifelse(dreams == "Y", "DREAMS DISTRICTS", "NON-DREAMS DISTRICTS"))
    
    gbv_dreams_ou <- munge_gbv(dreams) %>% 
      mutate(dreams = ifelse(dreams == "Y", "DREAMS DISTRICTS", "NON-DREAMS DISTRICTS"),
             dreams_color = ifelse(dreams != "DREAMS DISTRICTS", golden_sand, genoa))
    
    # Pull in map data
    
    spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
    zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% 
                            gisr::extract_boundaries(country = "Zambia", level = .x))
    names(zmb_geo) <- list("adm0", "snu1", "psnu")
    
    gbv_geo <- 
      gbv_dreams %>% left_join(zmb_geo$psnu, by = c("psnuuid" = "uid"))
    
    # PEP RESULTS
    gbv_viol <- df_msd %>% 
      filter(standardizeddisaggregate == "Age/Sex/ViolenceType") %>% 
      group_by(fiscal_year, indicator, sex) %>% 
      summarise(across(matches("targ|cumul"), sum, na.rm = T)) %>% 
      ungroup()
  
# VIZ ============================================================================

    # TRENDS GEND_GBV
    
    top <-  gbv_ou %>% 
      mutate(facet = "Zambia") %>% 
      ggplot(aes(x = factor(fiscal_year))) +
      geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.15)) +
      geom_col(aes(y = cumulative), fill = golden_sand, width = 0.5) +
      geom_text(aes(y = cumulative, label = percent(achievement, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro", 
                vjust = -0.2) +
      scale_y_continuous(labels = label_number_si(), limits = c(0, 55000)) +
      facet_wrap(~facet)+
      si_style_ygrid() +
      labs(x = NULL, y = NULL)
    top
    
    bottom <- gbv_agency %>% 
      filter(funding_agency != "DEDUP") %>% 
      mutate(funding_agency = fct_relevel(funding_agency, c("USAID", "CDC", "DOD"))) %>% 
      ggplot(aes(x = factor(fiscal_year))) +
      geom_col(aes(y = targets), fill = grey20k, width = 0.5, position = position_nudge(x = -0.15)) +
      geom_col(aes(y = cumulative), fill = golden_sand, width = 0.5) +
      geom_text(aes(y = cumulative, label = percent(achievement, 1)), 
                size = 10/.pt, 
                family = "Source Sans Pro", 
                vjust = -0.2) +
      scale_y_continuous(labels = label_number_si()) +
      facet_wrap(~funding_agency) +
      si_style_ygrid() +
      labs(x = NULL, y = NULL, subtitle = "GEND_GBV achievement by agency",
           caption = glue("{metadata$caption}"))
    
    top / bottom +
      plot_layout(heights = c(2, 3)) + 
      plot_annotation(title = "ZAMBIA GEND_GBV ACHIEVEMENT TRENDS THROUGH FY23Q2", subtitle = "Targets in gray, results in yellow")
    si_save(glue("Images/{metadata$curr_pd}_GEND_GBV_trends.png"), scale = 1.25)
    

# SPINDOWN ============================================================================

