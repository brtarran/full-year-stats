# Load libraries

library(ggplot2)
library(readxl)
library(here)
library(dplyr)
library(scales)
library(ggtext)


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
      subtitle = 'All films released in calendar year, excluding event titles',
      x = 'Year', 
      y = '') +
    scale_y_continuous(labels = scales::comma_format()) + 
    theme_minimal()
}

uk_market_share <- function() {
  df$year <- as.numeric(as.character(df$year))

  last_5_years <- max(df$year, na.rm = TRUE) - 5
  df_filtered <- df %>%
    filter(year >= last_5_years) %>%
    filter(film_type != "other_uk_qualifying") 

  ggplot(df_filtered, aes(x = factor(year), y = box_office_m, fill = film_type)) +
    geom_bar(stat = 'identity', position = 'dodge') + 
    labs(
      title = 'UK and Republic of Ireland box office, £ million',
      subtitle = "For <span style='color:#783df6'>**all films**</span>,
        <span style='color:#1197FF'>**all UK qualifying films**</span>
        and <span style='color:#E50076'>**UK independent films**</span>.",
      x = 'Year',
      y = '',
      fill = 'Film type') +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(values = c('uk_independent' = '#E50076', 
                                'all_uk_qualifying' = '#1197FF', 
                                'all_titles' = '#783df6')) +
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
    geom_text(aes(label = scales::comma(admissions_m)), vjust = 1.5, color = 'white') + 
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
