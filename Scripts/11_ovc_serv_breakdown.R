# PROJECT: Here-it-goes-again-reprise
# PURPOSE: Analysis of FY23Q2 data for OVC
# AUTHOR: Tim Essam | SI
# REF ID:   3067f005
# LICENSE: MIT
# DATE: 2023-05-11
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
    
    
  # Load functions    
    source("Scripts/helper-call_all_helpers.R")
      
  # Grab metadata
   get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "d3b8f3bc"
    
  # Functions
    # Function to group and sum
    group_sum <- function(.data, ...){
      .data %>% 
        group_by(indicator, fiscal_year, ...) %>% 
        summarise(across(c(targets, starts_with("cumulative")), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    }
  

# LOAD DATA ============================================================================  

  df_hivstat <- 
      df_msd  %>% 
      filter(indicator %in% c("OVC_HIVSTAT", "OVC_SERV"), 
             standardizeddisaggregate  %in% c("Total Denominator", "Age/Sex/DREAMS", 
                                              "Age/Sex/Preventive", "Total Numerator"),
             fiscal_year == metadata$curr_fy) %>% 
      group_sum(standardizeddisaggregate) %>% 
      mutate(indicator = case_when(
        str_detect(standardizeddisaggregate, "Total Den") ~ "Comprehensive",
        str_detect(standardizeddisaggregate, "Age/Sex/DREAMS") ~ "DREAMS",
        str_detect(standardizeddisaggregate, "Preventive") ~ "Preventive",
        str_detect(standardizeddisaggregate, "Total Num") & indicator == "OVC_SERV" ~ "OVC_SERV",
        TRUE ~ indicator
      )) %>% 
      filter(indicator != "OVC_HIVSTAT")
    
  #df_ovc_pstatus <- 
    
    
  df_ovc_pstatus <-   df_msd %>% 
    filter(str_detect(indicator, "OVC_SERV"), 
           str_detect(otherdisaggregate, c("Active|Graduated|Exited|Transf")),
           standardizeddisaggregate %in% c("Age/Sex/ProgramStatus", "ProgramStatus"),
           fiscal_year == metadata$curr_fy) %>% 
    mutate(otherdisaggregate = case_when(
      str_detect(otherdisaggregate, "Exited|Transf") ~ "Transfer/Exit",
      TRUE ~ otherdisaggregate
    )) %>%    group_by(fiscal_year, indicator, otherdisaggregate) %>% 
    summarize(across(c(cumulative), sum, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(disag_fill = case_when(
      str_detect(otherdisaggregate, "Exit") ~ old_rose_light,
      str_detect(otherdisaggregate, "Grad") ~ burnt_sienna_light,
      str_detect(otherdisaggregate, "Active") ~ scooter),
      disag = fct_relevel(otherdisaggregate, c("Transfer/Exit", "Graduated", "Active")),
    ) 

# MUNGE ============================================================================
  
  # Left hand side of bar
    
  # Pull limits for each bar
    ub_lim <- df_hivstat %>% 
      filter(indicator == "OVC_SERV") %>% pull(targets)
    
   a <- df_hivstat %>% 
      filter(indicator == "OVC_SERV") %>% 
      mutate(label = str_c(percent(cumulative/targets), "\n" , comma(cumulative))) %>% 
      ggplot(aes(x = "OVC_SERV")) +
      geom_col(aes(y = targets), fill = grey30k, position = position_nudge(x = 0.1), width = 0.6) +
      geom_col(aes(y = cumulative), fill = denim, width = 0.6) +
      geom_text(aes(y = cumulative, label = label), 
                vjust = 1.25, 
                family = "Source Sans Pro",
                size = 12/.pt, 
                color = "white") +
     geom_text(aes(y = targets, label = "FY23 targets"), 
               vjust = 1.25, 
               family = "Source Sans Pro",
               size = 12/.pt, 
               color = grey90k,
               position = position_nudge(x = 0.1)) +
      scale_y_continuous(labels = label_number_si(), expand = c(0, 0), limits = c(0, ub_lim)) +
      si_style_ygrid() +
      labs(x = NULL, y = NULL)
  
    
    
  b <- df_hivstat %>% 
    filter(indicator != "OVC_SERV") %>% 
    mutate(label = str_c(indicator, "\n", comma(cumulative)),
           fill_clr = case_when(
             indicator == "Comprehensive" ~ trolley_grey_light,
             indicator == "DREAMS" ~ burnt_sienna_light,
             TRUE ~ denim_light
           )) %>%
    mutate(total = sum(cumulative), .by = fiscal_year) %>% 
    ggplot(aes(x = "OVC_SERV <18")) +
    geom_col(aes(y = cumulative, fill = fct_rev(indicator)), width = 0.6) +
    geom_text(aes(y = cumulative, label = label), 
              position = position_stack(vjust = 0.65)) +
    geom_text(aes(y = total, label = str_c(comma(total), " Overall")), 
              size = 14/.pt, 
              family = "Source Sans Pro",
              vjust = -0.45) +
    scale_fill_manual(
      values = c("Comprehensive" = denim_light, 
                 "DREAMS" = scooter_med, 
                 "Preventive" = "grey")
    ) +
    si_style_ygrid() +
    theme(axis.text.y = element_blank(),
          legend.position = "none") +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ub_lim))
  
  c <- a + b + plot_annotation(title = glue("{metadata$curr_pd} OVC_SERV RESULTS"))
  
# OVC_ PROGRAM STATUS ============================================================================

  d <- df_ovc_pstatus %>% 
    ggplot(aes(x = "Program Status", y = cumulative, group = disag, fill = disag_fill)) + 
    geom_col(alpha = 0.85, width = 0.5) +
    scale_fill_identity() +
    geom_text(aes(label = str_c(disag, " ", comma(cumulative))), 
              position = position_stack(), vjust = 1.5,
              family = "Source Sans Pro", 
              size = 12/.pt) +
    scale_y_continuous(labels = label_number_si(), expand = c(0, 0)) +
    si_style_ygrid()  +
    labs(x= NULL, y = NULL,
         title = "OVC_SERV BY PROGRAM STATUS")
  
  c + d + plot_layout(widths = c(1, 1, 2)) +
    plot_annotation(caption = glue("{metadata$caption}"))
  
  si_save(glue("Images/{metadata$curr_pd} OVC_SERV summary.png"))

# SPINDOWN ============================================================================

