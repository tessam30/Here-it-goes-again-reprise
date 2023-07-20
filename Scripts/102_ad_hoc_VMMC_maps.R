# PROJECT: Maps of VMMC Progress
# PURPOSE: Munge and Analysis of
# AUTHOR:  Tim Esssam | SI
# REF ID:  1d053acf
# LICENSE: MIT
# DATE:   2023-07-20
# NOTES:   

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
    shpdata <- file.path(glamr::si_path("path_vector"))
    file_path <- return_latest(folderpath = merdata, pattern = "PSNU_IM.*Zambia")

  # Grab metadata
    get_metadata(file_path)
    
  # REF ID for plots
    ref_id <- "1d053acf"
    
  # Functions  
    get_hist <- function(df, metric, bins = 30, lines = 13) {
      df %>%
        filter(str_detect(psnu, "_Military", negate = T)) %>%
        ggplot(aes(x = {{ metric }}, fill = ..x..)) +
        geom_histogram(bins = bins, color = "white") +
        scale_y_continuous(expand = c(0.01, 0)) +
        theme(
          axis.text.y = element_blank(),
          legend.position = "none"
        ) +
        labs(x = NULL, y = NULL) +
        geom_hline(yintercept = seq(0, lines), size = 0.1, color = "White") +
        si_style_xline() +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, color = "#d3d3d3")
        )
    }

# LOAD DATA ============================================================================  

  df_msd <- read_psd(file_path) %>% 
      filter(indicator == "VMMC_CIRC",
            standardizeddisaggregate == "Total Numerator"
             ) %>% 
      group_by(psnuuid, psnu, funding_agency, fiscal_year, snu1) %>% 
      summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = T)), .gropus = "drop") %>% 
      clean_agency() %>% 
      mutate(achv = cumulative / targets,
             gap = cumulative - targets)
    
  # Pull in geospatial data
    cntry <- "Zambia"
    spdf_pepfar <- gisr::get_vcpolygons(path = shpdata, name = "VcPepfarPolygons.shp")
    zmb_geo <- purrr::map(3:5, ~spdf_pepfar %>% gisr::extract_boundaries(country = cntry, 
                                                                         level = .x))
    names(zmb_geo) <- list("adm0", "snu1", "psnu")
    
    zmb_adm1 <- st_read("../Zambezi/GIS/snu1_fy22.shp")
  

# Join results to geospatial ============================================================================
  
  df_geo <- df_msd %>% left_join(., zmb_geo$psnu, by = c("psnuuid" = "uid")) 
    
    df_geo %>% 
      filter(fiscal_year != 2023,
             str_detect(psnu, "_Mil", negate = T)) %>% 
      ggplot(aes(geometry = geometry)) +
      geom_sf(data = zmb_geo$psnu, color = "white", fill = grey10k) +
      geom_sf(aes(fill = achv)) +
      geom_sf(data = zmb_geo$psnu, color = "white", fill = NA) +
      geom_sf(data = zmb_geo$adm0, color = grey90k, fill = NA) +
      scale_fill_viridis_c(limits = c(0, 1.5),
                           breaks = seq(0.25, 1.5, 0.25),
                           labels = percent,
                           oob = scales::squish,
                           direction = -1,
                           option = "D",
                           na.value = grey40k) +
      facet_wrap(~fiscal_year) +
      si_style_map() +
      si_legend_fill() +
      labs(title = "FY21 AND FY22 VMMC ACHIEVEMENT BY PSNU",
           subtitle = "PSNU with results but no targets fill in medium grey",
           fill = "Results to Targets Achievement",
           caption = glue("{metadata$caption}"))
    
    si_save("Images/ZMB_VMMC_CIRC_map_achv_2021_2022.png")
    
    
    
    df_msd %>% 
      ungroup() %>% 
      filter(fiscal_year == 2023,
             str_detect(psnu, "_Mil", negate = T)) %>% 
      clean_psnu() %>% 
      mutate(psnu_order = tidytext::reorder_within(psnu, targets, snu1)) %>% 
      ggplot(aes(y = psnu_order)) +
      geom_col(aes(x = targets), fill = grey20k, width = 0.5, position = position_nudge(y = 0.1)) +
      geom_col(aes(x = cumulative), fill = scooter_med, width = 0.5) +
      geom_text(aes(label = percent(achv, 1), x = cumulative), size = 7/.pt, hjust = -0.1) +
      scale_y_reordered() +
      facet_wrap(~snu1, scales = "free") +
      si_style(facet_space = 0.5) +
      labs(title = "PSNU RESULTS TO TARGETS FOR VMMC_CIRC THROUGH FY23Q2",
           subtitle = "Achievement labeled inf indicates no targets but reported results",
           caption = glue("{metadata$caption}"), 
           x = NULL, y = NULL)
    
    si_save("Images/ZMB_FY23Q2_VMMC_CIRC_summary.png", scale = 1.15)
    
  get_hist(df_geo, achv) +
    facet_wrap(~fiscal_year)
    
  # Show by PSNU and SNU
  
  df_msd %>% 
    ungroup() %>% 
    filter(str_detect(psnu, "_Mil", negate = T),
           gap < 0,
           funding_agency %in% c("CDC", "USAID"), 
           fiscal_year == 2022) %>% 
    clean_psnu() %>% 
    mutate(psnu_order = tidytext::reorder_within(psnu, -gap, snu1, )) %>% 
    ggplot(aes(x = gap, y = psnu_order)) +
    geom_linerange(aes(xmin = 0, xmax = gap), color = grey30k, size = 2) +
    geom_point(color = old_rose, size = 5) +
    geom_point(color = "black", size = 5, shape = 1) +
    geom_text(aes(label = percent(1 - achv, 1)), size = 8/.pt, vjust = -1.15) +
      facet_wrap(~snu1, scales = "free_y", nrow = 2) +
    scale_y_reordered() +
    scale_x_continuous(labels = scales::label_number_si()) +
    si_style(facet_space = 0.75) +
    labs(title = "In FY22, Itezhi-tezhi has the largest achievement gap by percentage, but Mufulira had the largest absolute gap",
         subtitle = "Graphic includes only PSNUs that fell short of targets (shortfall above red circle)",
         caption = glue("{metadata$caption}"),
         y = NULL, x = "VMMC target shortfall")
  
  si_save("Images/ZMB_VMMC_CIRC_achv_gap_fy22.png", scale = 1.25)
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

