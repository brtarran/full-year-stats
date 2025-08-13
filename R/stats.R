# Load libraries

library(ggplot2)
library(readxl)
library(here)
library(dplyr)
library(scales)
library(ggtext)
library(ggrepel)
library(lubridate)


# Load data

load_data <- function(file_path, sheet_name) {
  df <- read_xlsx(file_path, sheet = sheet_name)
  return(df)
}


# Plots

# Box office


uk_box_office <- function() {
  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = month(match(tolower(month), tolower(month.name))) # convert month names to numbers
    )
  
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Find the latest month number in the latest year & quarter
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  
  # Convert back to month name
  latest_month <- month.name[latest_month_num]
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)
  
  # Plot
  ggplot(df, aes(x = year, y = uk_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +
    geom_text(aes(label = scales::comma(round(uk_box_office_m, 0))), 
              vjust = 1.5, color = 'white') +
    labs(
      title = 'UK box office, £ million',
      subtitle = paste0('All titles on release, including event titles, January to ', latest_month),
      x = 'Year',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal()
}


uk_roi_box_office <- function() {
  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = month(match(tolower(month), tolower(month.name))) # convert month names to numbers
    )
  
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Find the latest month number in the latest year & quarter
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  
  # Convert back to month name
  latest_month <- month.name[latest_month_num]
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)
  
  # Plot
  ggplot(df, aes(x = year, y = uk_roi_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#783df6') +  
    geom_text(aes(label = scales::comma(uk_roi_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK and Republic of Ireland box office, £ million', 
      subtitle = paste0("<span style='color:#783df6'>All films</span> released in calendar year, excluding event titles, January to ", latest_month),
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


uk_market_share <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  ggplot(df_filtered, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars 
    geom_text(data = df_filtered %>% filter(film_type == "all_uk_qualifying"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = 1.5),
              color = 'white') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#1197FF'>**all UK qualifying films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = '#1197FF'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


uk_market_share_indie <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  ggplot(df_filtered, aes(x = factor(year), y = box_office_m)) +
    # First layer: all films
    geom_bar(aes(fill = ifelse(film_type == "all_titles", "all_titles", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Second layer: all UK qualifying films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "all_uk_qualifying", "all_uk_qualifying", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Third layer: uk independent films (overlaid)
    geom_bar(aes(fill = ifelse(film_type == "uk_independent", "uk_independent", NA)), 
             stat = 'identity', position = 'identity', na.rm = TRUE) + 
    # Text labels for relevant bars
    geom_text(data = df_filtered %>% filter(film_type == "uk_independent"), 
              aes(label = scales::comma(round(box_office_m, 0)), vjust = -0.5),
              color = '#e50076') +
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  released in calendar year, excluding event titles",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'all_uk_qualifying' = 'lightgrey',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


uk_market_share_percent <- function() {
  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = month(match(tolower(month), tolower(month.name))) # convert month names to numbers
    )
  
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Find the latest month number in the latest year & quarter
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  
  # Convert back to month name
  latest_month <- month.name[latest_month_num]
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year, film_type)

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

  df_filtered$film_type <- factor(df_filtered$film_type, levels = c("other_uk_qualifying", "uk_independent"))

  ggplot(df_filtered, aes(x = factor(year), y = market_share_percent)) +
    # Layer 1: All films (no stacking)
    geom_bar(aes(fill = "all_titles"), 
             stat = 'identity', position = 'identity', na.rm = TRUE) +
    # Layer 2: Stacked bars for uk_independent and other_uk_qualifying (stacked on top of all_titles)
    geom_bar(data = df_filtered %>% filter(film_type %in% c("other_uk_qualifying", "uk_independent")), 
             aes(fill = film_type), 
             stat = 'identity', position = 'stack', na.rm = TRUE) + 
    # Text labels for relevant bars (only for uk_independent and other_uk_qualifying)
    geom_text(data = df_filtered %>% filter(film_type %in% c("other_uk_qualifying", "uk_independent")), 
              aes(label = scales::comma(round(market_share_percent, 0))), 
              position = position_stack(vjust = 0.5),
              color = 'white') +
    labs(
      title = 'Share of UK and Republic of Ireland box office, %',
      subtitle = paste0("For <span style='color:#e50076'>**all UK independent films**</span> 
                  and <span style='color:#1197FF'>**other UK qualifying films**</span> released January to ", latest_month),
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_fill_manual(values = c('all_titles' = 'grey', 
                                 'other_uk_qualifying' = '#1197FF',
                                 'uk_independent' = '#e50076'), 
                      na.value = "transparent") + 
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


# Admissions

uk_admissions <- function() {
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = month(match(tolower(month), tolower(month.name))) # convert month names to numbers
    )
  
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Find the latest month number in the latest year & quarter
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  
  # Convert back to month name
  latest_month <- month.name[latest_month_num]
  
  df_filtered <- df %>%
    filter(quarter <= latest_quarter) %>%
    group_by(year) %>%
    summarise(admissions_m = sum(admissions_m, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_filtered, aes(x = year, y = admissions_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +
    geom_text(aes(label = scales::comma(round(admissions_m, 0))),
              vjust = 1.5, color = 'white') +
    labs(
      title = 'UK admissions, million',
      subtitle = paste0('All titles on release, including event titles, January to ', 
                        latest_month),
      x = 'Year',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal()
}


uk_admissions_month <- function() {
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      month_num = month(match(tolower(month), tolower(month.name))), # convert month names to numbers
      month = factor(month, levels = month.name),
      latest_year = max(year, na.rm = TRUE),
      color = ifelse(year == latest_year, "#e50076", "#D3D3D3")
    )
  
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Find the latest month number in the latest year & quarter
  latest_month_num <- df %>%
    filter(year == latest_year, quarter == latest_quarter) %>%
    summarise(max_month = max(month_num, na.rm = TRUE)) %>%
    pull(max_month)
  
  # Convert back to month name
  latest_month <- month.name[latest_month_num]

  ggplot(df, aes(x = month, y = admissions_m, group = year, color = factor(year))) +
    geom_line(aes(color = color), size = 1) +
    geom_point(aes(color = color), size = 2) +
    geom_text(
      data = df %>% group_by(year) %>% filter(row_number() == n()),
      aes(label = year, x = month, y = admissions_m, color = color),
      hjust = 0, vjust = 0.5, size = 5, fontface = "bold",
      position = position_nudge(x = 0.05)
    ) +
    labs(
      title = 'UK admissions, million',
      subtitle = paste0('All titles on release, including event titles, January to ', 
                        latest_month),      
      x = 'Month',
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    scale_x_discrete(expand = expansion(mult = 0.1)) +  
    scale_color_identity() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}


# Production

all_production_first <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]
  
  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      rolling_end = as.Date(rolling_end, format = "%d/%m/%Y")
    )
  
  # Get latest rolling_end month-year
  latest_month <- format(max(df$rolling_end, na.rm = TRUE), "%B")
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)

  df_revised <- df %>%
    filter(production_type == 'all') %>%
    group_by(year, category) %>%
    filter(!(status == 'first_reported' & any(status == 'revised'))) %>%
    ungroup()

  df_first <- df %>%
    filter(production_type == 'all') %>%
    group_by(year, category) %>%
    filter(status == 'first_reported') %>%
    ungroup()

  df_total_revised <- df_revised %>%
    group_by(year) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  df_total_first <- df_first %>%
    group_by(year) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  ggplot(df_total_revised, aes(x = year, y = total_metric)) +
    geom_bar(stat = 'identity', fill = 'grey', alpha = 0) +
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5, alpha = 0) +
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +
    geom_text(data = df_total_first, aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) +
    labs(
      title = paste0("UK production ", metric_display),
      subtitle = paste0(
    "Film and HETV starting principal photography in the 12 months to ",
    latest_month,
    ", <span style='color:#783df6'>**first reported**</span>"),
      x = 'Year',
      y = ''
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


all_production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]  

  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      rolling_end = as.Date(rolling_end, format = "%d/%m/%Y")
    )
  
  # Get latest rolling_end month-year
  latest_month <- format(max(df$rolling_end, na.rm = TRUE), "%B")
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)

  df_revised <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  df_first <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, category) %>%
    filter(status == 'first_reported') %>%
    ungroup()

  # Calculate total spend per year
  df_total_revised <- df_revised %>%
    group_by(year) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  df_total_first <- df_first %>%
    group_by(year) %>%
    summarise(total_metric = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup()

  # Create the plot
  ggplot(df_total_revised, aes(x = year, y = total_metric)) +
    geom_bar(stat = 'identity', fill = 'darkgrey') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) +  # Position total labels slightly above the top of the bars
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) + 
    labs(
      title = paste0("UK production ", metric_display), 
      subtitle = paste0(
    "Film and HETV starting principal photography in the 12 months to ",
    latest_month,
    ", <span style='color:#783df6'>**first reported**</span> and 
            <span style='color:darkgrey'>**revised**</span>"), 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


film_hetv_production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]

  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      rolling_end = as.Date(rolling_end, format = "%d/%m/%Y")
    )
  
  # Get latest rolling_end month-year
  latest_month <- format(max(df$rolling_end, na.rm = TRUE), "%B")
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)
    
  df_filtered <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = year, y = .data[[metric]], fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::comma(round(.data[[metric]], 0))), 
              position = position_stack(vjust = 0.9), 
              color = 'white') + 
    labs(
      title = paste0("UK production ", metric_display),
      subtitle = paste0("<span style='color:#e50076'>**Film**</span> 
            and <span style='color:#1197FF'>**HETV**</span> 
            starting principal photography in the 12 months to ", latest_month), 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('film' = '#e50076', 
                                 'hetv' = '#1197FF')) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


film_hetv_production_revised_percentage <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend",
    count = "count"
  )

  metric_display <- metric_display_names[[metric]]  
  
  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      rolling_end = as.Date(rolling_end, format = "%d/%m/%Y")
    )
  
  # Get latest rolling_end month-year
  latest_month <- format(max(df$rolling_end, na.rm = TRUE), "%B")
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)
     
  df_filtered <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup() %>%
    # Calculate the total spend per year
    group_by(year) %>%
    mutate(total_metric_year = sum(.data[[metric]], na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate the proportion for each category in each year
    mutate(proportion = .data[[metric]] / total_metric_year)

  ggplot(df_filtered, aes(x = year, y = proportion, fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
              position = position_stack(vjust = 0.5), 
              color = 'white') +  # Position the labels at the center of each segment
    labs(
      title = paste0("UK production ", metric_display, ", % by category"),
      subtitle = paste0("<span style='color:#e50076'>**Film**</span> 
            and <span style='color:#1197FF'>**HETV**</span> 
            starting principal photography in the 12 months to ", latest_month), 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c('film' = '#e50076', 
                                 'hetv' = '#1197FF')) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


production_revised <- function() {
  # Mappings
  metric_display_names <- c(
    UK_spend_m = "spend, £ million",
    count = "count"
  )
  
  category_display_names <- c(
    film = "Film",
    hetv = "HETV"
)
  
  category_colours <- c(
    film = "#e50076",
    hetv = "#1197FF"
  )

  metric_display <- metric_display_names[[metric]]
  category_display <- category_display_names[[category_select]]
  category_colour <- category_colours[[category_select]]

  # Make sure year and quarter are numeric
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      quarter = as.numeric(as.character(quarter)),
      rolling_end = as.Date(rolling_end, format = "%d/%m/%Y")
    )
  
  # Get latest rolling_end month-year
  latest_month <- format(max(df$rolling_end, na.rm = TRUE), "%B")
  latest_year <- max(df$year, na.rm = TRUE)
  latest_quarter <- max(df$quarter[df$year == latest_year], na.rm = TRUE)
  
  # Filter data for all years but only latest_quarter
  df <- df %>%
    filter(quarter == latest_quarter) %>%
    group_by(year)
    
  df_filtered <- df %>%
    filter(category == category_select) %>%
    filter(!production_type %in% c('all', 'inward_investment_and_co_production')) %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  df_total <- df %>%
    filter(category == category_select) %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = year, y = .data[[metric]])) +
    geom_bar(stat = 'identity', , fill = category_colour) +  
    geom_text(data = df_total, aes(label = scales::comma(round(.data[[metric]], 0)), 
              vjust = 1.5), 
              color = 'white') + 
    labs(
      title = paste0("UK production ", metric_display),
      subtitle = paste0(
                "<span style='color:", category_colour, "'>**",
                category_display, "**</span> starting principal photography in the 12 months to ", 
                latest_month), 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}

production_breakdown_revised <- function() {
  df_filtered <- df %>%
    filter(category == category_select) %>%
    filter(!production_type %in% c('all', 'inward_investment_and_co_production')) %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, production_type) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = year, y = .data[[metric]])) +
    geom_bar(stat = 'identity', fill = 'darkgrey') +  
    geom_line(aes(group = production_type), color = category_colour, size = 1) + 
    geom_point(color = category_colour, size = 2) +  # Add points to highlight values
    geom_text_repel(
      aes(label = scales::comma(round(.data[[metric]], 0))),  # Round to 0dp & add comma separator
      nudge_y = 10,
      direction = 'y',  # Only adjust in the vertical direction
      size = 4, 
      fontface = 'bold',
      color = 'white',  # White text color
      box.padding = 0.2,  # Adds slight padding around text
      segment.color = NA  # Removes connector lines
    ) +
    geom_text(
      data = df_filtered %>% filter(year == '2025') %>%
      mutate(production_type = recode(production_type, 
        'inward_investment' = 'INW',
        'co_production' = 'COP',  
        'domestic_uk' = 'DOM'
      )),
      aes(label = production_type),
      color = category_colour, 
      hjust = 0, vjust = 0.5, size = 5, fontface = "bold",
      position = position_nudge(x = 0.1)
    ) +  
    labs(
      title = title, 
      subtitle = subtitle, 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}