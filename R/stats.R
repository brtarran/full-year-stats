# Load libraries

library(ggplot2)
library(readxl)
library(here)
library(dplyr)
library(scales)
library(ggtext)
library(ggrepel)


# Load data

load_data <- function(file_path, sheet_name) {
  df <- read_xlsx(file_path, sheet = sheet_name) %>%
    mutate(year = as.factor(year))
  return(df)
}


# Plots

# Box office


uk_box_office <- function() {
  ggplot(df, aes(x = year, y = uk_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +  
    geom_text(aes(label = scales::comma(uk_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK box office, £ million', 
      subtitle = 'All titles on release, including event titles', 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal()
}


uk_roi_box_office <- function() {
  ggplot(df, aes(x = year, y = uk_roi_box_office_m)) +
    geom_bar(stat = 'identity', fill = '#783df6') +  
    geom_text(aes(label = scales::comma(uk_roi_box_office_m)), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK and Republic of Ireland box office, £ million', 
      subtitle = "<span style='color:#783df6'>All films</span> released in calendar year, excluding event titles",
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
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years)

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
      subtitle = "For <span style='color:#e50076'>**all UK independent films**</span> 
                  and <span style='color:#1197FF'>**other UK qualifying films**</span>",
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
    group_by(year) %>%
    summarise(admissions_m = sum(admissions_m, na.rm = TRUE))

  ggplot(df, aes(x = year, y = admissions_m)) +
    geom_bar(stat = 'identity', fill = '#e50076') +  
    geom_text(aes(label = scales::comma(round(admissions_m, 0))), vjust = 1.5, color = 'white') + 
    labs(
      title = 'UK admissions, million',
      subtitle = 'All titles on release, including event titles',
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal()
}


uk_admissions_month <- function() {
  df <- df %>%
    mutate(
      year = as.numeric(as.character(year)),
      month = factor(month, levels = month.name),
      latest_year = max(year, na.rm = TRUE),
      color = ifelse(year == latest_year, "#e50076", "#D3D3D3")
    )

  ggplot(df, aes(x = month, y = admissions_m, group = year, color = factor(year))) +
    geom_line(aes(color = color), size = 1) +
    geom_point(aes(color = color), size = 2) +
    geom_text(
      data = df %>% filter(month == "December"),
      aes(label = year, x = month, y = admissions_m, color = color),
      hjust = 0, vjust = 0.5, size = 5, fontface = "bold",
      position = position_nudge(x = 0.1)
    ) +
    labs(
      title = 'UK admissions, million',
      subtitle = 'All titles on release, including event titles',
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
    geom_bar(stat = 'identity', fill = 'grey', alpha = 0) +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5, alpha = 0) +  # Position total labels slightly above the top of the bars
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(data = df_total_first, aes(label = scales::comma(round(total_metric, 0))),
              color = 'white', vjust = 1.5) + 
    labs(
      title = 'UK production spend, £ million', 
      subtitle = "For film and HETV starting principal photography in calendar year, 
                  <span style='color:#783df6'>**first reported**</span>", 
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(
      legend.position = 'none',
      plot.subtitle = element_textbox_simple()
    )
}


all_production_revised <- function() {
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
    summarise(total_spend = sum(UK_spend_m, na.rm = TRUE)) %>%
    ungroup()

  df_total_first <- df_first %>%
    group_by(year) %>%
    summarise(total_spend = sum(UK_spend_m, na.rm = TRUE)) %>%
    ungroup()

  # Create the plot
  ggplot(df_total_revised, aes(x = year, y = total_spend)) +
    geom_bar(stat = 'identity', fill = 'darkgrey') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_spend, 0))),
              color = 'white', vjust = 1.5) +  # Position total labels slightly above the top of the bars
    geom_bar(data = df_total_first, stat = 'identity', fill = '#783df6') +  
    # Add total spend labels for each year at the top of the stacked bars
    geom_text(aes(label = scales::comma(round(total_spend, 0))),
              color = 'white', vjust = 1.5) + 
    labs(
      title = 'UK production spend, £ million', 
      subtitle = "For film and HETV starting principal photography in calendar year, 
                  <span style='color:#783df6'>**first reported**</span> and 
                  <span style='color:darkgrey'>**revised**</span>",
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
  df_filtered <- df %>%
    filter(production_type == 'all') %>%
    # For each year, if 'revised' exists, keep only 'revised' or 'first_reported' if 'revised' doesn't exist
    group_by(year, category) %>%
    filter(
      !(status == 'first_reported' & any(status == 'revised'))  # Exclude 'first_reported' if 'revised' is present for the same year
    ) %>%
    ungroup()

  ggplot(df_filtered, aes(x = year, y = UK_spend_m, fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::comma(round(UK_spend_m, 0))), 
              position = position_stack(vjust = 0.9), 
              color = 'white') + 
    labs(
      title = 'UK production spend, £ million', 
      subtitle = "For <span style='color:#e50076'>**film**</span> 
                  and <span style='color:#1197FF'>**HETV**</span> 
                  starting principal photography in calendar year", 
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
    mutate(total_spend_year = sum(UK_spend_m, na.rm = TRUE)) %>%
    ungroup() %>%
    # Calculate the proportion for each category in each year
    mutate(proportion = UK_spend_m / total_spend_year)

  ggplot(df_filtered, aes(x = year, y = proportion, fill = category)) +
    geom_bar(stat = 'identity') +  
    geom_text(aes(label = scales::percent(proportion, accuracy = 1)), 
              position = position_stack(vjust = 0.5), 
              color = 'white') +  # Position the labels at the center of each segment
    labs(
      title = 'UK production spend, proportion (%) by category', 
      subtitle = "For <span style='color:#e50076'>**film**</span> 
                  and <span style='color:#1197FF'>**HETV**</span> 
                  starting principal photography in calendar year", 
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
      data = df_filtered %>% filter(year == '2024') %>%
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